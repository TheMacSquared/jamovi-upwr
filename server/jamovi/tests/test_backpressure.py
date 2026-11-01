"""Bounded sends, request ordering, configurable workers and interruption."""

import asyncio
from types import SimpleNamespace
from unittest.mock import AsyncMock

import pytest

from jamovi.server.clientconnection import ClientConnection
from jamovi.server import jamovi_pb2 as pb
from jamovi.server.utils import conf
from jamovi.server.utils.websocketqueue import WebSocketQueue
from jamovi.server import session as session_code
from jamovi.server import enginemanager as manager_code
from jamovi.tests.test_analysis_lifecycle import active_engine, request, until, FakeEngine
from jamovi.server.utils.stream import ProgressStream


@pytest.mark.asyncio
async def test_progress_storm_is_coalesced_and_terminal_reply_supersedes_it():
    gate = asyncio.Event()
    messages = []

    async def send(data):
        await gate.wait()
        messages.append(pb.ComsMessage.FromString(data))

    ws = SimpleNamespace(send_bytes=send, close=AsyncMock())
    conn = ClientConnection(ws, {})
    req = pb.ModuleRR()
    conn._transactions[7] = req
    for i in range(10000):
        conn.send(instance_id='one', response_to=req, complete=False, progress=(i, 10000))
    assert len(conn._outgoing._pending) == 1
    conn.send(instance_id='one', response_to=req)
    assert len(conn._outgoing._pending) == 1
    gate.set()
    await until(lambda: len(messages) == 1)
    assert messages[0].status == pb.COMPLETE and messages[0].id == 7
    assert not conn._transactions
    await conn._outgoing.wait_closed()


@pytest.mark.asyncio
async def test_different_requests_keep_latest_progress_and_reply_order():
    ws = SimpleNamespace(send_bytes=AsyncMock(), close=AsyncMock())
    queue = WebSocketQueue(ws, 10, 100, 2)
    queue.send(b'progress one', progress_key=('a', 1))
    queue.send(b'reply two')
    queue.send(b'latest one', progress_key=('a', 1))
    queue.send(b'progress three', progress_key=('a', 3))
    await until(lambda: ws.send_bytes.await_count == 3)
    assert [call.args[0] for call in ws.send_bytes.call_args_list] == [
        b'reply two', b'latest one', b'progress three']
    await queue.wait_closed()


@pytest.mark.asyncio
@pytest.mark.parametrize('limit', ['messages', 'bytes'])
async def test_overflow_closes_connection_instead_of_dropping_critical_replies(limit):
    ws = SimpleNamespace(send_bytes=AsyncMock(), close=AsyncMock())
    queue = WebSocketQueue(ws, 2 if limit == 'messages' else 10,
                           100 if limit == 'messages' else 5, 1)
    for data in [b'aaa', b'bbb', b'ccc']:
        queue.send(data)
    await until(lambda: ws.close.await_count == 1)
    assert queue.failed and not queue._pending
    await queue.wait_closed()
    assert queue._bytes == 0


@pytest.mark.asyncio
@pytest.mark.parametrize('failure', ['timeout', 'error'])
async def test_slow_or_broken_socket_has_one_bounded_writer_and_is_closed(failure):
    async def send(data):
        if failure == 'error':
            raise OSError('broken connection')
        await asyncio.Event().wait()

    ws = SimpleNamespace(send_bytes=send, close=AsyncMock())
    queue = WebSocketQueue(ws, 100, 1000, .01)
    queue.send(b'first')
    writer = queue._writer
    for _ in range(20):
        queue.send(b'next')
    assert queue._writer is writer
    await until(lambda: ws.close.await_count == 1)
    await queue.wait_closed()
    assert queue.failed and writer.done()


@pytest.mark.asyncio
async def test_inflight_bytes_count_towards_budget_and_shutdown_reaps_writer():
    entered = asyncio.Event()

    async def send(data):
        entered.set()
        await asyncio.Event().wait()

    ws = SimpleNamespace(send_bytes=send, close=AsyncMock())
    queue = WebSocketQueue(ws, 10, 5, 1)
    queue.send(b'1234')
    await entered.wait()
    queue.send(b'56')
    assert queue.failed
    await queue.wait_closed()
    assert not queue._pending and queue._writer.done()


@pytest.mark.asyncio
@pytest.mark.parametrize('workers', [1, 2, 6])
async def test_r_process_count_controls_pool_and_scheduler(monkeypatch, tmp_path, workers):
    monkeypatch.setitem(conf.config_values, 'r_processes', str(workers))
    monkeypatch.setattr(manager_code, 'Engine', FakeEngine)
    session = session_code.Session(str(tmp_path), 'workers')
    assert len(session._runner._engines) == workers
    assert session._scheduler.queue.n_slots == workers
    assert session._scheduler._n_run_slots == max(1, workers - 1)
    await session._scheduler.close()
    await session._runner.stop()


@pytest.mark.parametrize('value', ['0', '-1', 'invalid'])
def test_invalid_worker_configuration_is_rejected(monkeypatch, value):
    monkeypatch.setitem(conf.config_values, 'r_processes', value)
    with pytest.raises(ValueError):
        conf.positive_int('r_processes', 4)


@pytest.mark.asyncio
@pytest.mark.parametrize('cancel_task', [False, True])
async def test_cancellation_recovers_busy_process_before_returning(cancel_task):
    engine = active_engine()
    engine._process = SimpleNamespace(returncode=None)
    engine._recover = AsyncMock()
    stream = ProgressStream()
    task = asyncio.create_task(engine.run(request(), stream))
    await until(lambda: bool(engine._results_queue._getters))
    if cancel_task:
        task.cancel()
        with pytest.raises(asyncio.CancelledError):
            await task
    else:
        stream.cancel()
        await task
    engine._recover.assert_awaited_once()
    assert stream.cancelled()
    engine._process = None


@pytest.mark.asyncio
async def test_shutdown_cancellation_does_not_restart_worker():
    engine = active_engine()
    engine._process = SimpleNamespace(returncode=None)
    engine._parent._stopping = True
    engine._recover = AsyncMock()
    await engine._interrupt()
    engine._recover.assert_not_awaited()
    engine._process = None


@pytest.mark.asyncio
async def test_cancelled_real_worker_is_reaped_and_replacement_computes(monkeypatch, tmp_path):
    from pathlib import Path
    home = Path(conf.get('home'))
    fixture = home / 'test-data' / 'upstream-28.17.csv'
    if not fixture.is_file() or not (home / 'bin' / 'jamovi-engine').is_file():
        pytest.skip('Requires Docker R worker and manual fixture')
    monkeypatch.setitem(conf.config_values, 'r_processes', '1')
    session = session_code.Session(str(tmp_path), 'cancel-real-worker')
    await session.start()
    try:
        instance = await session.create()
        await instance.open(str(fixture))
        engine = session._runner._engines[0]
        process = engine._process
        req = request()
        req.sessionId, req.instanceId = session.id, instance.id
        req.options.hasNames = True
        req.options.names.append('vars')
        req.options.options.add().c.options.add().s = 'wynik (cm)'
        stream = ProgressStream()
        task = asyncio.create_task(engine.run(req, stream))
        await until(lambda: bool(engine._results_queue._getters))
        stream.cancel()
        await asyncio.wait_for(task, 15)
        assert process.returncode is not None
        assert engine._process.pid != process.pid
        result = ProgressStream()
        await asyncio.wait_for(engine.run(req, result), 15)
        assert (await result).status == pb.ANALYSIS_COMPLETE
    finally:
        session.stop()
        await asyncio.wait_for(session.wait_ended(), 15)
