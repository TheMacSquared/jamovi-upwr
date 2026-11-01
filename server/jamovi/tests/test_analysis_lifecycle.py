"""Failures, cancellation and shutdown must settle requests and release workers."""

import asyncio
import threading
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import AsyncMock, Mock

import pytest
import pytest_asyncio

from jamovi.server import engine as engine_module
from jamovi.server import enginemanager as manager_module
from jamovi.server import session as session_module
from jamovi.server import jamovi_pb2 as pb
from jamovi.server.analyses import Analysis
from jamovi.server.clientconnection import ClientConnection
from jamovi.server.engine import Engine
from jamovi.server.pool import Pool
from jamovi.server.scheduler import Scheduler, BadAnalysis
from jamovi.server.server import Server
from jamovi.server.utils.stream import ProgressStream
from jamovi.server.utils import conf


def request(revision=1, analysis_id=2, perform=pb.AnalysisRequest.RUN):
    return pb.AnalysisRequest(instanceId='instance', analysisId=analysis_id,
                              ns='jmv', name='descriptives', revision=revision,
                              perform=perform)


def response(req):
    return pb.AnalysisResponse(instanceId=req.instanceId, analysisId=req.analysisId,
                               revision=req.revision, status=pb.ANALYSIS_COMPLETE)


async def until(predicate):
    async with asyncio.timeout(2):
        while not predicate():
            await asyncio.sleep(0)


def active_engine():
    parent = SimpleNamespace(_next_conn_index=0, _notify_engine_event=Mock())
    engine = Engine(parent, '/tmp', 'ipc://test', {'home': '/missing'})
    engine._socket = Mock()
    engine._running.set()
    engine._stopped.clear()
    return engine


@pytest.mark.asyncio
async def test_websocket_creates_instance_and_replies():
    ws = SimpleNamespace(send_bytes=AsyncMock())
    instance = SimpleNamespace(id='new-instance', set_coms=Mock())
    session = SimpleNamespace(create=AsyncMock(return_value=instance))
    connection = ClientConnection(ws, session)
    message = pb.ComsMessage(id=17, payloadType='InstanceRequest',
                            payload=pb.InstanceRequest().SerializeToString())
    await connection.on_message_async(message.SerializeToString())
    await until(lambda: ws.send_bytes.await_count == 1)
    session.create.assert_awaited_once()
    result = pb.ComsMessage.FromString(ws.send_bytes.call_args.args[0])
    assert result.id == 17 and result.status == pb.COMPLETE
    assert pb.InstanceResponse.FromString(result.payload).instanceId == instance.id
    assert not connection._transactions


@pytest.mark.asyncio
@pytest.mark.parametrize('payload_type', ['InfoRequest', 'UnknownRequest'])
async def test_websocket_handler_error_replies_with_original_id(payload_type):
    ws = SimpleNamespace(send_bytes=AsyncMock())
    instance = SimpleNamespace(set_coms=Mock(),
                               on_request=AsyncMock(side_effect=RuntimeError('handler failed')))
    connection = ClientConnection(ws, {'instance': instance})
    message = pb.ComsMessage(id=23, instanceId='instance', payloadType=payload_type)
    await connection.on_message_async(message.SerializeToString())
    await until(lambda: ws.send_bytes.await_count == 1)
    result = pb.ComsMessage.FromString(ws.send_bytes.call_args.args[0])
    assert result.id == 23 and result.status == pb.ERROR
    assert result.error.message
    assert not connection._transactions


@pytest.mark.asyncio
async def test_engine_send_failure_settles_stream():
    engine = active_engine()
    engine._socket.send.side_effect = RuntimeError('socket failed')
    stream = ProgressStream()
    await engine.run(request(), stream)
    with pytest.raises(RuntimeError, match='socket failed'):
        await stream
    assert engine._current_analysis is None


@pytest.mark.asyncio
@pytest.mark.parametrize('timeout', [0, 2])
async def test_engine_connection_retry_is_bounded_and_nonblocking(monkeypatch, timeout):
    class NotConnected(Exception):
        errno = engine_module.nanomsg.EAGAIN

    monkeypatch.setattr(engine_module.nanomsg, 'NanoMsgAPIError', NotConnected)
    engine = active_engine()
    engine._socket.send.side_effect = [NotConnected(), None]
    if timeout == 0:
        with pytest.raises(TimeoutError, match='connecting'):
            await engine._send(b'request', timeout=timeout)
    else:
        await engine._send(b'request', timeout=timeout)
        assert engine._socket.send.call_count == 2
    assert engine._socket.send.call_args.kwargs['flags'] == engine_module.nanomsg.DONTWAIT


@pytest.mark.asyncio
async def test_engine_cancellation_cleans_up_receivers():
    engine = active_engine()
    stream = ProgressStream()
    task = asyncio.create_task(engine.run(request(), stream))
    await until(lambda: bool(engine._results_queue._getters))
    task.cancel()
    with pytest.raises(asyncio.CancelledError):
        await task
    assert stream.cancelled()
    assert not engine._results_queue._getters
    assert not engine._stopped._waiters
    assert engine._current_analysis is None


@pytest.mark.asyncio
async def test_cancelled_stream_wins_over_simultaneous_final_result():
    engine = active_engine()
    req = request()
    stream = ProgressStream()
    task = asyncio.create_task(engine.run(req, stream))
    await until(lambda: bool(engine._results_queue._getters))
    engine._results_queue.put_nowait((response(req), True))
    stream.cancel()
    await task
    assert stream.cancelled()
    assert not engine._results_queue._getters


@pytest.mark.asyncio
async def test_latest_revision_replaces_running_analysis():
    engine = active_engine()
    old_stream, new_stream = ProgressStream(), ProgressStream()
    old = asyncio.create_task(engine.run(request(1), old_stream))
    await until(lambda: bool(engine._results_queue._getters))
    latest = request(2)
    new = asyncio.create_task(engine.run(latest, new_stream))
    await until(lambda: engine._socket.send.call_count == 2)
    engine._results_queue.put_nowait((response(request(1)), True))
    engine._results_queue.put_nowait((response(latest), True))
    assert (await asyncio.wait_for(new_stream, 2)).revision == 2
    await new
    with pytest.raises(asyncio.CancelledError):
        await old
    assert old_stream.cancelled()
    assert not engine._results_queue._getters


@pytest.mark.asyncio
@pytest.mark.parametrize('failure', ['timeout', 'crash'])
async def test_failure_keeps_slot_until_worker_recovers(failure):
    engine = active_engine()
    recovering, release = asyncio.Event(), asyncio.Event()

    async def restart():
        recovering.set()
        await release.wait()
        engine._stopped.clear()

    engine.restart = AsyncMock(side_effect=restart)
    if failure == 'timeout':
        engine._analysis_duration_limit = 0
    stream = ProgressStream()
    task = asyncio.create_task(engine.run(request(), stream))
    await until(lambda: bool(engine._results_queue._getters))
    if failure == 'crash':
        engine._stopped.set()
    await asyncio.wait_for(recovering.wait(), 2)
    assert not stream.done()
    release.set()
    result = await asyncio.wait_for(stream, 2)
    assert result.status == pb.ANALYSIS_ERROR
    await task
    engine.restart.assert_awaited_once()
    assert not engine._results_queue._getters


@pytest.mark.asyncio
async def test_replacement_does_not_cancel_worker_recovery():
    engine = active_engine()
    recovering, release = asyncio.Event(), asyncio.Event()

    async def restart():
        recovering.set()
        await release.wait()

    engine.restart = AsyncMock(side_effect=restart)
    engine._analysis_duration_limit = 0
    old_stream, latest_stream = ProgressStream(), ProgressStream()
    old = asyncio.create_task(engine.run(request(1), old_stream))
    await asyncio.wait_for(recovering.wait(), 2)
    engine._analysis_duration_limit = None
    req = request(2)
    latest = asyncio.create_task(engine.run(req, latest_stream))
    await until(old.done)
    assert not engine._restart_task.cancelled()
    release.set()
    await until(lambda: engine._socket.send.call_count == 2)
    engine._results_queue.put_nowait((response(req), True))
    assert (await asyncio.wait_for(latest_stream, 2)).revision == 2
    await latest
    with pytest.raises(asyncio.CancelledError):
        await old
    assert old_stream.cancelled()
    engine.restart.assert_awaited_once()


def fake_process():
    process = SimpleNamespace(returncode=None, stdin=Mock())
    process.terminate = Mock(side_effect=lambda: setattr(process, 'returncode', 0))
    process.kill = Mock(side_effect=lambda: setattr(process, 'returncode', -9))
    process.wait = AsyncMock(return_value=0)
    return process


@pytest.mark.asyncio
async def test_stop_handles_broken_socket_and_is_idempotent():
    engine = active_engine()
    process, socket = fake_process(), engine._socket
    engine._process = process
    engine._process_stopping = threading.Event()
    engine._process_abandoned = threading.Event()
    socket.send.side_effect = RuntimeError('broken socket')
    await engine.stop()
    await engine.stop()
    process.terminate.assert_called_once()
    process.wait.assert_awaited_once()
    assert engine._process is None and engine._socket is None
    assert engine._stopped.is_set() and not engine._running.is_set()


@pytest.mark.asyncio
async def test_cancelling_stop_still_waits_for_cleanup(monkeypatch):
    engine = active_engine()
    stopping, release = asyncio.Event(), asyncio.Event()
    completed = []

    async def cleanup():
        stopping.set()
        await release.wait()
        completed.append(True)

    monkeypatch.setattr(engine, '_stop', cleanup)
    task = asyncio.create_task(engine.stop())
    await stopping.wait()
    task.cancel()
    await asyncio.sleep(0)
    assert not task.done()
    release.set()
    with pytest.raises(asyncio.CancelledError):
        await task
    assert completed == [True]


@pytest.mark.asyncio
async def test_send_failure_recovers_live_worker_before_releasing_stream():
    engine = active_engine()
    engine._process = fake_process()
    engine._socket.send.side_effect = RuntimeError('send failed')
    engine.restart = AsyncMock()
    stream = ProgressStream()
    await engine.run(request(), stream)
    with pytest.raises(RuntimeError, match='send failed'):
        await stream
    engine.restart.assert_awaited_once()


@pytest.mark.asyncio
async def test_start_failure_after_spawn_cleans_process(monkeypatch):
    parent = SimpleNamespace(_next_conn_index=0, _notify_engine_event=Mock())
    engine = Engine(parent, '/tmp', 'ipc://test', {'home': '/missing'})
    process = fake_process()
    monkeypatch.setattr(engine_module, 'create_subprocess_exec', AsyncMock(return_value=process))
    monkeypatch.setattr(engine_module.nanomsg, 'Socket', Mock(side_effect=RuntimeError('socket init failed')))
    with pytest.raises(RuntimeError, match='socket init failed'):
        await engine.start()
    process.terminate.assert_called_once()
    assert engine._process is None and engine._stopped.is_set()
    parent._notify_engine_event.assert_called_once()


class FakeEngine:
    def __init__(self, **kwargs):
        self.start = AsyncMock()
        self.stop = AsyncMock()
        self.restart = AsyncMock()
        self.run = AsyncMock(side_effect=self.complete)

    async def complete(self, req, stream):
        stream.set_result(response(req))


@pytest_asyncio.fixture
async def manager(monkeypatch, tmp_path):
    monkeypatch.setattr(manager_module, 'Engine', FakeEngine)
    manager = manager_module.EngineManager(str(tmp_path), Pool(2), {})
    yield manager
    await manager.stop()


@pytest.mark.asyncio
async def test_manager_start_failure_waits_for_all_workers_and_rolls_back(manager):
    release = asyncio.Event()
    manager._engines[0].start.side_effect = RuntimeError('startup failed')
    manager._engines[1].start.side_effect = release.wait
    task = asyncio.create_task(manager.start())
    await until(lambda: manager._engines[1].start.await_count == 1)
    assert not task.done()
    release.set()
    with pytest.raises(RuntimeError, match='startup failed'):
        await task
    assert all(engine.stop.await_count == 1 for engine in manager._engines)
    assert manager._run_loop_task.done()


@pytest.mark.asyncio
@pytest.mark.parametrize('failure', ['exception', 'no-result'])
async def test_manager_failure_settles_request_and_reuses_slot(manager, failure):
    if failure == 'exception':
        manager._engines[0].run.side_effect = RuntimeError('worker failed')
    else:
        manager._engines[0].run.side_effect = None
    await manager.start()
    stream = manager._queue.add(request())
    with pytest.raises(RuntimeError):
        await asyncio.wait_for(stream, 2)
    await until(lambda: all(value is None for value in manager._requests))
    manager._engines[0].run.side_effect = manager._engines[0].complete
    next_stream = manager._queue.add(request(2))
    assert (await asyncio.wait_for(next_stream, 2)).revision == 2


@pytest.mark.asyncio
@pytest.mark.parametrize('started', [False, True])
async def test_manager_stop_cancels_queued_and_running_requests(manager, started):
    pending = asyncio.Event()
    if started:
        async def wait_for_cancellation(*args):
            await pending.wait()
        manager._engines[0].run.side_effect = wait_for_cancellation
        await manager.start()
    stream = manager._queue.add(request())
    await until(lambda: bool(manager._tasks))
    await manager.stop()
    assert stream.cancelled()
    assert manager._run_loop_task.done()
    assert not manager._tasks
    assert all(value is None for value in manager._requests)
    with pytest.raises(RuntimeError, match='closed'):
        manager._queue.add(request(2))


@pytest.mark.asyncio
async def test_manager_restart_holds_new_work_until_ready(manager):
    running, restarting, release = asyncio.Event(), asyncio.Event(), asyncio.Event()

    async def run(req, stream):
        running.set()
        await asyncio.Event().wait()

    async def restart():
        restarting.set()
        await release.wait()

    manager._engines[0].run.side_effect = run
    manager._engines[0].restart.side_effect = restart
    await manager.start()
    old_stream = manager._queue.add(request())
    await running.wait()
    task = asyncio.create_task(manager.restart_engines())
    await restarting.wait()
    assert old_stream.cancelled()
    manager._engines[0].run.side_effect = manager._engines[0].complete
    latest_stream = manager._queue.add(request(2))
    await asyncio.sleep(0)
    assert not latest_stream.done()
    release.set()
    await task
    assert (await asyncio.wait_for(latest_stream, 2)).revision == 2


@pytest.mark.asyncio
async def test_manager_stop_waits_for_every_worker_even_if_one_fails(manager):
    release, stopping = asyncio.Event(), asyncio.Event()

    async def stop():
        stopping.set()
        await release.wait()

    manager._engines[0].stop.side_effect = RuntimeError('stop failed')
    manager._engines[1].stop.side_effect = stop
    task = asyncio.create_task(manager.stop())
    await stopping.wait()
    assert not task.done()
    release.set()
    with pytest.raises(RuntimeError, match='stop failed'):
        await task
    assert manager._run_loop_task.done()
    assert manager._engines[1].stop.await_count == 1


@pytest.mark.asyncio
async def test_terminal_stream_releases_slot_before_old_task_cleanup_finishes(manager):
    release = asyncio.Event()

    async def run(req, stream):
        stream.set_result(response(req))
        if req.analysisId == 2:
            await release.wait()

    manager._engines[0].run.side_effect = run
    await manager.start()
    first = manager._queue.add(request())
    await first
    next_stream = manager._queue.add(request(2, analysis_id=4))
    await asyncio.wait_for(next_stream, 2)
    assert manager._engines[0].run.await_count == 2
    release.set()


class FakeAnalyses:
    def __init__(self, analysis):
        self.analysis = analysis

    def add_options_changed_listener(self, listener):
        pass

    def get(self, *args):
        return self.analysis

    def __iter__(self):
        return iter([self.analysis])

    @property
    def needs_init(self):
        if self.analysis.status == Analysis.Status.NONE:
            return [self.analysis]
        return []

    needs_run = ()
    needs_op = ()


def analysis_scheduler(monkeypatch, perform=pb.AnalysisRequest.INIT):
    analysis = SimpleNamespace(id=2, revision=1, ns='jmv', state_dropped=False,
                               status=Analysis.Status.NONE, _ops=[], _run_waiters=[], enabled=True,
                               instance=SimpleNamespace(id='instance'))
    analysis.set_status = lambda status: setattr(analysis, 'status', status)
    analysis.set_error = Mock()
    analysis.cancel_pending = lambda: Analysis.cancel_pending(analysis)
    analysis.set_results = Mock(side_effect=lambda result, *args, **kwargs:
                                setattr(analysis, 'status', kwargs.get('status', Analysis.Status.RUNNING)))
    analysis.op = Analysis.Op(Analysis.Op.SAVE, analysis) if perform == pb.AnalysisRequest.SAVE else None
    if analysis.op is not None:
        analysis._ops.append(analysis.op)
    scheduler = Scheduler(1, 1, FakeAnalyses(analysis), {'jmv': object()})
    monkeypatch.setattr(scheduler, '_to_message',
                        lambda analysis, action: request(analysis.revision, perform=perform))
    if perform == pb.AnalysisRequest.SAVE:
        analysis.status = Analysis.Status.RUNNING
        scheduler._n_running += 1
        scheduler._run_analysis(analysis, 'op')
    else:
        scheduler._send_next()
    return analysis, scheduler


@pytest.mark.asyncio
async def test_scheduler_reports_worker_exception_and_frees_counters(monkeypatch):
    analysis, scheduler = analysis_scheduler(monkeypatch)
    _, stream = scheduler.queue.get(('instance', 2))
    stream.set_exception(RuntimeError('worker failed'))
    await until(lambda: not scheduler._tasks)
    assert analysis.status == Analysis.Status.ERROR
    assert analysis.state_dropped
    assert 'worker failed' in analysis.set_error.call_args.args[0]
    assert scheduler._n_initing == scheduler._n_running == 0
    await scheduler.close()


@pytest.mark.asyncio
@pytest.mark.parametrize('revision', [1, 2])
async def test_scheduler_replacement_does_not_report_stale_cancellation(monkeypatch, revision):
    analysis, scheduler = analysis_scheduler(monkeypatch)
    analysis.revision = revision
    scheduler._send_next(analysis)
    req, stream = scheduler.queue.get(('instance', 2))
    stream.set_result(response(req))
    await until(lambda: not scheduler._tasks)
    analysis.set_error.assert_not_called()
    assert analysis.status == Analysis.Status.INITED
    assert scheduler._n_initing == scheduler._n_running == 0
    await scheduler.close()


@pytest.mark.asyncio
@pytest.mark.parametrize('started', [False, True])
async def test_shutdown_settles_export_even_before_handler_starts(monkeypatch, started):
    analysis, scheduler = analysis_scheduler(monkeypatch, pb.AnalysisRequest.SAVE)
    operation = analysis.op
    if started:
        await asyncio.sleep(0)
    await scheduler.close()
    with pytest.raises(asyncio.CancelledError):
        await operation.future
    assert not analysis._ops
    assert not scheduler._tasks
    assert scheduler._n_initing == scheduler._n_running == 0


@pytest.mark.asyncio
async def test_export_failure_settles_operation(monkeypatch):
    analysis, scheduler = analysis_scheduler(monkeypatch, pb.AnalysisRequest.SAVE)
    _, stream = scheduler.queue.get(('instance', 2))
    stream.set_exception(RuntimeError('export failed'))
    with pytest.raises(RuntimeError, match='export failed'):
        await asyncio.wait_for(analysis.op.future, 2)
    await until(lambda: not scheduler._tasks)
    assert not analysis._ops
    await scheduler.close()


@pytest.mark.asyncio
async def test_export_completion_tolerates_cancelled_caller(monkeypatch):
    analysis, scheduler = analysis_scheduler(monkeypatch, pb.AnalysisRequest.SAVE)
    analysis.op.future.cancel()
    req, stream = scheduler.queue.get(('instance', 2))
    stream.set_result(response(req))
    await until(lambda: not scheduler._tasks)
    assert not analysis._ops
    analysis.set_error.assert_not_called()
    await scheduler.close()


@pytest.mark.asyncio
@pytest.mark.parametrize('state_dropped', [False, True])
async def test_export_fails_if_restoring_analysis_state_fails(state_dropped):
    analysis = SimpleNamespace(state_dropped=state_dropped, _run_waiters=[], _ops=[], enabled=True,
                               status=Analysis.Status.ERROR, results=pb.AnalysisResponse())
    analysis.results.results.error.message = 'restoring analysis failed'

    def rerun():
        for waiter in analysis._run_waiters:
            waiter.set_result(None)

    analysis.rerun = rerun
    with pytest.raises(ValueError, match='restoring analysis failed'):
        await Analysis.save(analysis, '/tmp/export.csv', 'table')
    assert not analysis._ops and not analysis._run_waiters


@pytest.mark.asyncio
async def test_disabled_analysis_cannot_queue_export_that_will_never_run():
    analysis = SimpleNamespace(enabled=False, _ops=[])
    with pytest.raises(RuntimeError, match='disabled'):
        await Analysis.save(analysis, '/tmp/export.csv', 'table')
    assert not analysis._ops


@pytest.mark.asyncio
async def test_export_cleans_operation_if_dispatch_raises():
    analysis = SimpleNamespace(enabled=True, state_dropped=False, _ops=[],
                               status=Analysis.Status.COMPLETE, results=pb.AnalysisResponse(),
                               parent=SimpleNamespace(_notify_options_changed=Mock(side_effect=RuntimeError('dispatch failed'))))
    with pytest.raises(RuntimeError, match='dispatch failed'):
        await Analysis.save(analysis, '/tmp/export.csv', 'table')
    assert not analysis._ops


@pytest.mark.asyncio
async def test_missing_module_settles_export_operation(monkeypatch):
    analysis, scheduler = analysis_scheduler(monkeypatch)
    await scheduler.close()
    operation = Analysis.Op(Analysis.Op.SAVE, analysis)
    analysis._ops.append(operation)
    analysis.op = operation
    scheduler._modules = {}
    with pytest.raises(BadAnalysis):
        scheduler._run_analysis(analysis, 'op')
    with pytest.raises(ValueError):
        await operation.future
    assert not analysis._ops


@pytest.mark.asyncio
async def test_shutdown_cancels_export_waiting_for_state_restore(monkeypatch):
    analysis, scheduler = analysis_scheduler(monkeypatch)
    analysis.state_dropped = True
    analysis.rerun = lambda: None
    task = asyncio.create_task(Analysis.save(analysis, '/tmp/export.csv', 'table'))
    await until(lambda: bool(analysis._run_waiters))
    await scheduler.close()
    with pytest.raises(asyncio.CancelledError):
        await task
    assert not analysis._run_waiters


@pytest.mark.asyncio
async def test_removing_analysis_settles_exports_and_state_waiters():
    analysis = SimpleNamespace(_run_waiters=[asyncio.get_running_loop().create_future()], _ops=[])
    operation = Analysis.Op(Analysis.Op.SAVE, analysis)
    analysis._ops.append(operation)
    waiter = analysis._run_waiters[0]
    Analysis.notify_removing(analysis)
    for future in (operation.future, waiter):
        with pytest.raises(RuntimeError, match='removed'):
            await future
    assert not analysis._ops and not analysis._run_waiters
    assert analysis._status == Analysis.Status.DELETED


@pytest.mark.asyncio
async def test_export_settles_if_analysis_disappears_before_handler_starts(monkeypatch):
    analysis, scheduler = analysis_scheduler(monkeypatch, pb.AnalysisRequest.SAVE)
    monkeypatch.setattr(scheduler._analyses, 'get', lambda *args: None)
    with pytest.raises(asyncio.CancelledError):
        await asyncio.wait_for(analysis.op.future, 2)
    await until(lambda: not scheduler._tasks)
    assert not analysis._ops
    await scheduler.close()


@pytest.mark.asyncio
async def test_server_start_failure_reaches_ports_waiter(monkeypatch):
    server = Server()
    monkeypatch.setattr(server, '_run', AsyncMock(side_effect=RuntimeError('startup failed')))
    server.start()
    with pytest.raises(RuntimeError, match='startup failed'):
        await asyncio.wait_for(server.ports_opened, 2)
    with pytest.raises(RuntimeError, match='startup failed'):
        await server.wait_ended()


@pytest.mark.asyncio
@pytest.mark.parametrize('startup_failure', [False, True])
async def test_session_shutdown_waits_for_workers(monkeypatch, tmp_path, startup_failure):
    monkeypatch.setattr(manager_module, 'Engine', FakeEngine)
    session = session_module.Session(str(tmp_path), 'session-shutdown')
    session._modules.read = AsyncMock()
    if startup_failure:
        session._runner._engines[0].start.side_effect = RuntimeError('startup failed')
        with pytest.raises(RuntimeError, match='startup failed'):
            await session.start()
    else:
        await session.start()
        session.stop()
    await asyncio.wait_for(session.wait_ended(), 2)
    assert session._scheduler._closed
    assert session._runner._run_loop_task.done()
    assert all(engine.stop.await_count == 1 for engine in session._runner._engines)
    assert not session._runner.notifications()._getters


@pytest.mark.asyncio
async def test_fatal_startup_event_cannot_be_lost_before_session_loop(monkeypatch, tmp_path):
    monkeypatch.setattr(manager_module, 'Engine', FakeEngine)
    session = session_module.Session(str(tmp_path), 'session-startup-event')
    session._modules.read = AsyncMock()
    session._runner._engines[0].start.side_effect = lambda: session._on_engine_event({
        'type': 'error', 'message': 'worker exited at startup', 'cause': 'exit code 1'})
    with pytest.raises(RuntimeError, match='worker exited at startup'):
        await session.start()
    assert session._ended.is_set()
    assert session._run_loop_task is None
    assert all(engine.stop.await_count == 1 for engine in session._runner._engines)


@pytest.mark.asyncio
async def test_real_worker_failure_restart_and_shutdown(tmp_path):
    """Run against the actual C++/R workers shipped in the Docker image."""
    home = Path(conf.get('home'))
    fixture = home / 'test-data' / 'upstream-28.17.csv'
    if not (home / 'bin' / 'jamovi-engine').is_file() or not fixture.is_file():
        pytest.skip('Requires the Docker engine and bundled manual fixture')
    session = session_module.Session(str(tmp_path), 'real-workers')
    await session.start()

    async def check_analysis(analysis):
        analysis.rerun()
        async with asyncio.timeout(30):
            while not analysis.complete or analysis.status not in (Analysis.Status.COMPLETE, Analysis.Status.ERROR):
                await asyncio.sleep(0.01)
        assert analysis.status == Analysis.Status.COMPLETE, analysis.results
        means = [cell.d for element in analysis.results.results.group.elements
                 if element.WhichOneof('type') == 'table'
                 for column in element.table.columns if column.name == 'mean'
                 for cell in column.cells if cell.WhichOneof('cellType') == 'd']
        assert 35 in means, means

    try:
        instance = await session.create()
        stream = instance.open(str(fixture))
        async for _ in stream:
            pass
        await stream
        options = pb.AnalysisOptions(hasNames=True, names=['vars'])
        options.options.add().c.options.add().s = 'wynik (cm)'
        analysis = instance.analyses.create(2, 'descriptives', 'jmv', options)
        await check_analysis(analysis)
        engine = session._runner._engines[0]

        # Kill the real process immediately after sending it a request.
        process = engine._process
        socket = engine._socket
        original_send = socket.send

        def send_then_kill(data, flags=0):
            result = original_send(data, flags=flags)
            process.kill()
            return result

        socket.send = send_then_kill
        req = request(99, analysis_id=200)
        req.sessionId = session.id
        req.instanceId = instance.id
        req.options.CopyFrom(options)
        failed = ProgressStream()
        await asyncio.wait_for(engine.run(req, failed), 15)
        assert (await failed).status == pb.ANALYSIS_ERROR
        assert engine._process.pid != process.pid
        assert process.returncode is not None
        await check_analysis(analysis)

        engine._analysis_duration_limit = 0
        timed_out = ProgressStream()
        await asyncio.wait_for(engine.run(req, timed_out), 15)
        assert (await timed_out).status == pb.ANALYSIS_ERROR
        engine._analysis_duration_limit = None
        await check_analysis(analysis)

        processes = [worker._process for worker in session._runner._engines]
        await session.restart_engines()
        assert all(process.returncode is not None for process in processes)
        await check_analysis(analysis)
        processes = [worker._process for worker in session._runner._engines]
        threads = [worker._thread for worker in session._runner._engines]
    finally:
        session.stop()
        await asyncio.wait_for(session.wait_ended(), 15)
    assert all(process.returncode is not None for process in processes)
    assert not any(thread.is_alive() for thread in threads)
    assert not session._runner._tasks and not session._scheduler._tasks
