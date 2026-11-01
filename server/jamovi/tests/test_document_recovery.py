"""Atomic publication, executor cancellation and unsaved document recovery."""

import asyncio
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
import threading
from zipfile import ZipFile

import pytest

from jamovi.server import formatio
from jamovi.server import jamovi_pb2 as pb
from jamovi.server.utils import conf
from jamovi.tests.test_instance import FakeComs


def old_document(path):
    with ZipFile(path, 'w') as archive:
        archive.writestr('old', 'working document')
    return path.read_bytes()


@pytest.mark.parametrize('failure', ['write', 'fsync', 'replace', 'cancel'])
def test_failed_save_preserves_existing_document_and_cleans_staging(tmp_path, monkeypatch, failure):
    target = tmp_path / 'document.omv'
    before = old_document(target)
    # A fixed .tmp filename may belong to another program or an interrupted save.
    unrelated = tmp_path / 'document.omv.tmp'
    unrelated.write_text('do not overwrite')
    cancelled = threading.Event()

    def write(project, path, *args, **kwargs):
        Path(path).write_bytes(b'incomplete replacement')
        if failure == 'write':
            raise OSError('disk full')
        if failure == 'cancel':
            cancelled.set()

    def fail(*args):
        raise OSError('injected failure')

    monkeypatch.setattr(formatio.omv, 'write', write)
    if failure == 'fsync':
        monkeypatch.setattr(formatio.os, 'fsync', fail)
    elif failure == 'replace':
        monkeypatch.setattr(formatio.os, 'replace', fail)
    with pytest.raises((OSError, RuntimeError)):
        formatio.write(None, str(target), lambda p: None, cancelled=cancelled)
    assert target.read_bytes() == before
    assert unrelated.read_text() == 'do not overwrite'
    assert not list(tmp_path.glob('.*.tmp'))


def test_simultaneous_writers_publish_complete_independent_files(tmp_path, monkeypatch):
    target = tmp_path / 'document.omv'
    old_document(target)
    barrier = threading.Barrier(2)
    stages = []

    def write(project, path, *args, **kwargs):
        stages.append(path)
        with ZipFile(path, 'w') as archive:
            archive.writestr('value', project)
        barrier.wait(timeout=5)

    monkeypatch.setattr(formatio.omv, 'write', write)
    with ThreadPoolExecutor(2) as pool:
        list(pool.map(lambda value: formatio.write(value, str(target), lambda p: None), ['one', 'two']))
    assert len(set(stages)) == 2
    with ZipFile(target) as archive:
        assert archive.testzip() is None
        assert archive.read('value') in (b'one', b'two')
    assert not list(tmp_path.glob('.*.tmp'))


@pytest.mark.asyncio
async def test_repeated_cancellation_keeps_dataset_locked_until_writer_exits(instance, tmp_path, monkeypatch):
    await instance.open('')
    target = tmp_path / 'document.omv'
    before = old_document(target)
    started, release = threading.Event(), threading.Event()

    def write(project, path, *args, **kwargs):
        Path(path).write_bytes(b'replacement')
        started.set()
        assert release.wait(5)

    monkeypatch.setattr(formatio.omv, 'write', write)
    stream = instance.save({'path': str(target), 'overwrite': True})
    assert await asyncio.to_thread(started.wait, 2)
    try:
        stream.cancel()
        await asyncio.sleep(.01)
        stream._task.cancel()
        await asyncio.sleep(.01)
        assert instance._save_lock.locked()
        assert not stream._task.done()
    finally:
        release.set()
        await asyncio.gather(stream._task, return_exceptions=True)
    assert target.read_bytes() == before
    assert not instance._save_lock.locked()
    assert not list(tmp_path.glob('.*.tmp'))


async def edited_document(instance):
    await instance.open('')
    coms = FakeComs()
    instance.set_coms(coms)
    req = pb.DataSetRR(op=pb.SET, incData=True)
    block = req.data.add(rowStart=0, columnStart=0, rowCount=2, columnCount=1)
    block.values.add().i = 7
    block.values.add().i = 9
    await instance.on_request(req)
    assert not coms.errors
    assert instance.project.is_edited
    return coms


@pytest.mark.asyncio
async def test_unsaved_document_recovery_preserves_dirty_state_and_reopens(instance, tmp_path, monkeypatch):
    recovery = tmp_path / 'persistent-recovery'
    monkeypatch.setitem(conf.config_values, 'recovery_path', str(recovery))
    coms = await edited_document(instance)
    title = instance.project.title
    assert instance.project.path == ''
    assert instance.needs_recovery()
    await instance.recover()
    copy = Path(instance._recovery_file)
    assert copy.parent == recovery and copy.exists()
    assert instance.project.title == title
    assert instance.project.path == '' and instance.project.is_edited
    with ZipFile(copy) as archive:
        assert archive.testzip() is None
        assert 'metadata.json' in archive.namelist()
    await instance.recover()
    assert list(recovery.glob('*.omv')) == [copy]
    instance._on_fs_request(pb.FSRequest(path='{{Root}}'))
    assert any(entry.path == '{{Recovery}}' for entry in coms.sent[-1].contents)
    other = await instance.session.create()
    await other.open('{{Recovery}}/' + copy.name)
    dataset = other.project.get_dataset()
    assert dataset.row_count == 2 and dataset[0][0] == 7 and dataset[0][1] == 9
    assert other.project.path == '' and other.project.is_edited
    assert copy.exists()


@pytest.mark.asyncio
async def test_saved_original_is_not_overwritten_by_recovery(instance, tmp_path, monkeypatch):
    monkeypatch.setitem(conf.config_values, 'recovery_path', str(tmp_path / 'recovery'))
    await edited_document(instance)
    original = tmp_path / 'original.omv'
    await instance.save({'path': str(original), 'overwrite': True})
    before = original.read_bytes()
    instance.project.is_edited = True
    await instance.recover()
    assert original.read_bytes() == before
    assert instance.project.is_edited
    assert instance._normalise_path(instance.project.path) == str(original)


@pytest.mark.asyncio
async def test_failed_recovery_keeps_last_good_copy(instance, tmp_path, monkeypatch):
    monkeypatch.setitem(conf.config_values, 'recovery_path', str(tmp_path / 'recovery'))
    await edited_document(instance)
    await instance.recover()
    copy = Path(instance._recovery_file)
    before = copy.read_bytes()

    def fail(project, path, *args, **kwargs):
        Path(path).write_bytes(b'partial')
        raise OSError('disk full')

    monkeypatch.setattr(formatio.omv, 'write', fail)
    with pytest.raises(OSError):
        await instance.recover()
    assert copy.read_bytes() == before
    assert instance.project.is_edited and instance.project.path == ''
    assert not instance._recovering


@pytest.mark.asyncio
async def test_blank_untouched_document_does_not_create_recovery(instance, tmp_path, monkeypatch):
    recovery = tmp_path / 'recovery'
    monkeypatch.setitem(conf.config_values, 'recovery_path', str(recovery))
    await instance.open('')
    await instance.recover()
    assert not recovery.exists()


@pytest.mark.asyncio
async def test_recovery_path_cannot_escape_directory(instance, tmp_path, monkeypatch):
    monkeypatch.setitem(conf.config_values, 'recovery_path', str(tmp_path / 'recovery'))
    with pytest.raises(PermissionError):
        instance._normalise_path('{{Recovery}}/../outside.omv')


@pytest.mark.asyncio
@pytest.mark.parametrize('periodic', [False, True])
async def test_recovery_survives_session_restart(tmp_path, monkeypatch, periodic):
    from jamovi.server import session as session_code
    from jamovi.server import enginemanager as manager_code
    from jamovi.tests.test_analysis_lifecycle import FakeEngine, until
    from time import monotonic

    monkeypatch.setitem(conf.config_values, 'recovery_path', str(tmp_path / 'persistent'))
    monkeypatch.setitem(conf.config_values, 'recovery_interval', '1' if periodic else '120')
    monkeypatch.setattr(manager_code, 'Engine', FakeEngine)
    session = session_code.Session(str(tmp_path / 'first'), 'session')
    await session.start()
    try:
        instance = await session.create()
        await edited_document(instance)
        if periodic:
            instance._last_recovered = monotonic() - 10
            await until(lambda: bool(instance._recovery_file) and Path(instance._recovery_file).is_file())
    finally:
        session.stop()
        await asyncio.wait_for(session.wait_ended(), 5)
    copy = Path(instance._recovery_file)
    assert copy.is_file()
    second = session_code.Session(str(tmp_path / 'second'), 'another-session')
    try:
        restored = await second.create()
        await restored.open('{{Recovery}}/' + copy.name)
        assert restored.project.get_dataset()[0][1] == 9
        assert restored.project.path == '' and restored.project.is_edited
    finally:
        await second._scheduler.close()
        await second._runner.stop()


@pytest.mark.asyncio
async def test_disabled_recovery_does_not_write_at_shutdown(tmp_path, monkeypatch):
    from jamovi.server import session as session_code
    from jamovi.server import enginemanager as manager_code
    from jamovi.tests.test_analysis_lifecycle import FakeEngine

    recovery = tmp_path / 'disabled'
    monkeypatch.setitem(conf.config_values, 'recovery_path', str(recovery))
    monkeypatch.setitem(conf.config_values, 'recovery_interval', '0')
    monkeypatch.setattr(manager_code, 'Engine', FakeEngine)
    session = session_code.Session(str(tmp_path), 'disabled-recovery')
    await session.start()
    try:
        await edited_document(await session.create())
    finally:
        session.stop()
        await asyncio.wait_for(session.wait_ended(), 5)
    assert not recovery.exists()
