"""jUPWR (audit 2026-11-05): recovery copies are cleaned up; new files are
readable by others (umask), not 0600."""

import os
import stat
import time
from pathlib import Path

import pytest

from jamovi.server import formatio
from jamovi.server.instance import prune_recovery_copies
from jamovi.server.utils import conf
from jamovi.tests.test_document_recovery import edited_document


@pytest.mark.asyncio
async def test_saving_the_document_removes_its_recovery_copy(instance, tmp_path, monkeypatch):
    recovery = tmp_path / 'recovery'
    monkeypatch.setitem(conf.config_values, 'recovery_path', str(recovery))
    await edited_document(instance)
    await instance.recover()
    copy = Path(instance._recovery_file)
    assert copy.exists()
    await instance.save({'path': str(tmp_path / 'saved.omv'), 'overwrite': True})
    assert not copy.exists()
    assert instance._recovery_file is None
    assert (tmp_path / 'saved.omv').exists()


@pytest.mark.asyncio
async def test_export_keeps_the_recovery_copy(instance, tmp_path, monkeypatch):
    recovery = tmp_path / 'recovery'
    monkeypatch.setitem(conf.config_values, 'recovery_path', str(recovery))
    await edited_document(instance)
    await instance.recover()
    copy = Path(instance._recovery_file)
    await instance.save({'path': str(tmp_path / 'data.csv'), 'overwrite': True, 'export': True})
    assert copy.exists()


@pytest.mark.asyncio
async def test_a_reopened_recovery_copy_is_reused_not_duplicated(instance, tmp_path, monkeypatch):
    recovery = tmp_path / 'recovery'
    monkeypatch.setitem(conf.config_values, 'recovery_path', str(recovery))
    await edited_document(instance)
    await instance.recover()
    copy = Path(instance._recovery_file)
    other = await instance.session.create()
    await other.open('{{Recovery}}/' + copy.name)
    assert Path(other._recovery_file) == copy
    await other.recover()
    assert list(recovery.glob('*.omv')) == [copy]


def test_old_recovery_copies_are_pruned(tmp_path):
    old = tmp_path / 'old - 1.omv'
    fresh = tmp_path / 'fresh - 2.omv'
    other = tmp_path / 'notes.txt'
    for f in (old, fresh, other):
        f.write_text('x')
    now = time.time()
    os.utime(old, (now - 31 * 86400, now - 31 * 86400))
    os.utime(other, (now - 90 * 86400, now - 90 * 86400))
    assert prune_recovery_copies(str(tmp_path), 30, now) == 1
    assert not old.exists() and fresh.exists() and other.exists()
    assert prune_recovery_copies(str(tmp_path / 'missing'), 30, now) == 0


@pytest.mark.skipif(os.name == 'nt', reason='POSIX modes')
def test_a_new_file_gets_the_umask_mode_not_0600(tmp_path, monkeypatch):
    def write(project, path, *args, **kwargs):
        Path(path).write_bytes(b'data')

    monkeypatch.setattr(formatio.omv, 'write', write)
    target = tmp_path / 'new.omv'
    old = os.umask(0o022)
    try:
        formatio.write(None, str(target), lambda p: None)
    finally:
        os.umask(old)
    assert stat.S_IMODE(target.stat().st_mode) == 0o644
    # an existing file keeps its own mode
    os.chmod(target, 0o600)
    formatio.write(None, str(target), lambda p: None)
    assert stat.S_IMODE(target.stat().st_mode) == 0o600
