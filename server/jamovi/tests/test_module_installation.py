"""Real ZIP/filesystem installs with failures before and during replacement."""

import asyncio
from io import BytesIO
from contextlib import nullcontext
from pathlib import Path
import stat
from types import SimpleNamespace
from zipfile import BadZipFile, ZipFile, ZipInfo

import pytest
import pytest_asyncio
import yaml

from jamovi.server.modules import modules as module_code
from jamovi.server.utils import conf


def metadata(version='1.0.0', **changes):
    return dict(name='audit', title='Audit', version=version,
                rVersion='4.6.0', analyses=[], **changes)


def package(defn=None, extra=()):
    stream = BytesIO()
    with ZipFile(stream, 'w') as archive:
        archive.writestr('audit/jamovi.yaml', yaml.safe_dump(defn or metadata('2.0.0')))
        archive.writestr('audit/payload.txt', 'new module payload')
        for name, value in extra:
            archive.writestr(name, value)
    stream.seek(0)
    return stream


def files(root):
    return {str(path.relative_to(root)): path.read_bytes()
            for path in root.rglob('*') if path.is_file()}


@pytest_asyncio.fixture
async def installed(tmp_path, monkeypatch):
    monkeypatch.setattr(module_code, 'Dirs', SimpleNamespace(app_data_dir=lambda: str(tmp_path)))
    monkeypatch.setattr(module_code, 'app_info', SimpleNamespace(version='28.17.0.0', r_version='4.6.0'))
    system = tmp_path / 'system'
    system.mkdir()
    monkeypatch.setattr(conf, 'config_values', {'modules_path': str(system)})
    destination = tmp_path / 'modules' / 'audit'
    destination.mkdir(parents=True)
    (destination / 'jamovi.yaml').write_text(yaml.safe_dump(metadata()), encoding='utf-8')
    (destination / 'old-only.txt').write_text('working version', encoding='utf-8')
    manager = module_code.Modules(None)
    await manager.read()
    events = []
    manager.add_listener(events.append)
    return manager, destination, events


def unchanged(manager, destination, before, events):
    assert files(destination) == before
    assert manager.get('audit').version == [1, 0, 0]
    assert events == []
    assert list(destination.parent.iterdir()) == [destination]


@pytest.mark.asyncio
async def test_update_validates_before_replacing_and_notifies(installed, monkeypatch):
    manager, destination, events = installed
    validate = manager._validate_candidate

    def check(candidate, name):
        assert (destination / 'old-only.txt').read_text() == 'working version'
        assert Path(candidate).parent != destination.parent
        return validate(candidate, name)

    monkeypatch.setattr(manager, '_validate_candidate', check)
    await manager.install_from_file(package(), update=True)
    assert manager.get('audit').version == [2, 0, 0]
    assert Path(manager.get('audit').path) == destination
    assert (destination / 'payload.txt').read_text() == 'new module payload'
    assert not (destination / 'old-only.txt').exists()
    assert list(destination.parent.iterdir()) == [destination]
    assert events == [{'type': 'moduleUpdated', 'data': {'name': 'audit'}}, {'type': 'modulesChanged'}]


@pytest.mark.asyncio
async def test_first_install_from_path(installed, tmp_path):
    manager, destination, events = installed
    await manager.uninstall('audit')
    events.clear()
    archive = tmp_path / 'audit.jmo'
    archive.write_bytes(package().getvalue())
    await manager.install_from_file(str(archive))
    assert manager.get('audit').version == [2, 0, 0]
    assert (destination / 'payload.txt').exists()
    assert events == [{'type': 'modulesChanged'}]


@pytest.mark.asyncio
@pytest.mark.parametrize('kind', ['not-zip', 'empty', 'missing-meta', 'invalid-yaml',
                                 'mismatched-name', 'bad-version', 'bad-analysis',
                                 'wrong-r', 'newer-app', 'corrupt-crc'])
async def test_invalid_archive_preserves_working_module(installed, kind):
    manager, destination, events = installed
    before = files(destination)
    archive = package()
    if kind == 'not-zip':
        archive = BytesIO(b'not a zip archive')
    elif kind in ('empty', 'missing-meta', 'invalid-yaml'):
        archive = BytesIO()
        with ZipFile(archive, 'w') as z:
            if kind == 'missing-meta':
                z.writestr('audit/payload.txt', 'data')
            elif kind == 'invalid-yaml':
                z.writestr('audit/jamovi.yaml', 'name: [')
        archive.seek(0)
    elif kind == 'corrupt-crc':
        archive = BytesIO(archive.getvalue().replace(b'new module payload', b'bad module payload'))
    else:
        defn = metadata('2.0.0')
        if kind == 'mismatched-name':
            defn['name'] = 'other'
        elif kind == 'bad-version':
            defn['version'] = 'invalid'
        elif kind == 'bad-analysis':
            defn['analyses'] = [{'name': 'broken'}]
        elif kind == 'wrong-r':
            defn.update(rVersion='3.0.0', analyses=[{'name': 'test'}])
        elif kind == 'newer-app':
            defn['requires'] = {'jamovi': '>= 99.0'}
        archive = package(defn)
    with pytest.raises((BadZipFile, ValueError, TypeError, KeyError, yaml.YAMLError)):
        await manager.install_from_file(archive)
    unchanged(manager, destination, before, events)


@pytest.mark.asyncio
@pytest.mark.parametrize('entry', ['../outside', '/absolute', 'audit/../../outside',
                                  'audit/./file', 'audit//file', 'other/payload',
                                  'audit\\file', 'audit/C:/file', 'audit/.. /outside',
                                  'audit/file.', 'audit/payload.txt'])
async def test_invalid_archive_paths_do_not_touch_installed_modules(installed, entry):
    manager, destination, events = installed
    before = files(destination)
    with pytest.warns(UserWarning) if entry == 'audit/payload.txt' else nullcontext():
        archive = package(extra=[(entry, 'bad')])
    with pytest.raises(ValueError):
        await manager.install_from_file(archive)
    unchanged(manager, destination, before, events)


@pytest.mark.asyncio
async def test_symlink_entry_is_rejected(installed):
    manager, destination, events = installed
    before = files(destination)
    link = ZipInfo('audit/link')
    link.create_system = 3
    link.external_attr = (stat.S_IFLNK | 0o777) << 16
    with pytest.raises(ValueError):
        await manager.install_from_file(package(extra=[(link, '../../outside')]))
    unchanged(manager, destination, before, events)


@pytest.mark.asyncio
@pytest.mark.parametrize('stage', ['extract', 'backup', 'publish', 'refresh', 'cancel'])
async def test_io_failures_and_cancellation_restore_old_module(installed, monkeypatch, stage):
    manager, destination, events = installed
    before = files(destination)
    rename = module_code.os.rename

    def fail_rename(source, target):
        if ((stage == 'backup' and Path(source) == destination)
                or (stage == 'publish' and Path(source).parent.name == 'incoming')):
            raise PermissionError('file is locked')
        return rename(source, target)

    async def fail_refresh():
        manager._modules = []
        await asyncio.sleep(0)
        if stage == 'cancel':
            raise asyncio.CancelledError()
        raise OSError('registry refresh failed')

    if stage == 'extract':
        def fail_extract(*args, **kwargs):
            raise OSError('disk full')
        monkeypatch.setattr(ZipFile, 'extractall', fail_extract)
    elif stage in ('backup', 'publish'):
        monkeypatch.setattr(module_code.os, 'rename', fail_rename)
    else:
        monkeypatch.setattr(manager, '_reread_installed', fail_refresh)
    with pytest.raises(asyncio.CancelledError if stage == 'cancel' else OSError):
        await manager.install_from_file(package())
    unchanged(manager, destination, before, events)


@pytest.mark.asyncio
async def test_failed_first_install_leaves_no_module(installed, monkeypatch):
    manager, destination, events = installed
    await manager.uninstall('audit')
    events.clear()

    async def fail_refresh():
        raise OSError('registry refresh failed')

    monkeypatch.setattr(manager, '_reread_installed', fail_refresh)
    with pytest.raises(OSError):
        await manager.install_from_file(package())
    assert not destination.exists()
    assert 'audit' not in manager
    assert events == []
    assert list(destination.parent.iterdir()) == []


@pytest.mark.asyncio
async def test_cleanup_failure_keeps_backup_hidden_and_reports_success(installed, monkeypatch):
    manager, destination, events = installed
    remove = module_code.shutil.rmtree

    def fail_backup_cleanup(path, *args, **kwargs):
        if Path(path).name == 'previous':
            raise PermissionError('locked DLL')
        return remove(path, *args, **kwargs)

    monkeypatch.setattr(module_code.shutil, 'rmtree', fail_backup_cleanup)
    await manager.install_from_file(package())
    assert manager.get('audit').version == [2, 0, 0]
    backups = list(destination.parent.glob('.install-*/previous/old-only.txt'))
    assert len(backups) == 1
    assert backups[0].read_text() == 'working version'
    await manager._reread_installed()
    assert [m.name for m in manager] == ['audit']
    assert events == [{'type': 'modulesChanged'}]


@pytest.mark.asyncio
async def test_failed_rollback_never_deletes_backup(installed, monkeypatch):
    manager, destination, events = installed
    rename = module_code.os.rename

    def fail_publish_and_restore(source, target):
        if Path(target) == destination:
            raise PermissionError('destination locked')
        return rename(source, target)

    monkeypatch.setattr(module_code.os, 'rename', fail_publish_and_restore)
    with pytest.raises(PermissionError):
        await manager.install_from_file(package())
    backups = list(destination.parent.glob('.install-*/previous/old-only.txt'))
    assert len(backups) == 1
    assert backups[0].read_text() == 'working version'
    assert events == []


@pytest.mark.asyncio
async def test_bad_listener_does_not_fail_committed_install(installed):
    manager, destination, events = installed

    def fail_listener(event):
        raise RuntimeError('broken listener')

    manager.add_listener(fail_listener)
    await manager.install_from_file(package())
    assert manager.get('audit').version == [2, 0, 0]
    assert (destination / 'payload.txt').exists()


@pytest.mark.asyncio
async def test_updates_are_serialized(installed, monkeypatch):
    manager, destination, events = installed
    entered, release = asyncio.Event(), asyncio.Event()
    reread = manager._reread_installed

    async def delayed_refresh():
        entered.set()
        await release.wait()
        await reread()

    monkeypatch.setattr(manager, '_reread_installed', delayed_refresh)
    first = asyncio.create_task(manager.install_from_file(package()))
    await entered.wait()
    second = asyncio.create_task(manager.install_from_file(package(metadata('3.0.0'))))
    await asyncio.sleep(0)
    assert (destination / 'payload.txt').read_text() == 'new module payload'
    assert not second.done()
    release.set()
    await asyncio.gather(first, second)
    assert manager.get('audit').version == [3, 0, 0]
    assert list(destination.parent.iterdir()) == [destination]


@pytest.mark.asyncio
@pytest.mark.parametrize('kind', ['missing-r', 'missing-data', 'unsafe-data-path'])
async def test_missing_declared_payload_preserves_previous_module(installed, kind):
    manager, destination, events = installed
    before = files(destination)
    defn = metadata('2.0.0')
    if kind == 'missing-r':
        defn['analyses'] = [dict(name='test', ns='audit', title='Test',
                                 menuGroup='Tests', menuTitle='Test')]
    else:
        defn['datasets'] = [dict(name='data', description='Data',
                                path='../old-only.txt' if kind == 'unsafe-data-path' else 'data.csv')]
    with pytest.raises(ValueError):
        await manager.install_from_file(package(defn))
    unchanged(manager, destination, before, events)


@pytest.mark.asyncio
async def test_data_only_module_does_not_require_r_package(installed):
    manager, destination, events = installed
    defn = metadata('2.0.0')
    defn['datasets'] = [dict(name='data', description='Data', path='data.csv')]
    await manager.install_from_file(package(defn, extra=[('audit/data/data.csv', 'x\n1\n')]))
    assert manager.get('audit').version == [2, 0, 0]
    assert (destination / 'data' / 'data.csv').is_file()


@pytest.mark.asyncio
async def test_registry_scan_failure_is_not_silently_committed(installed, monkeypatch):
    manager, destination, events = installed
    before = files(destination)
    read = manager._read_module

    def fail_published_read(path, is_sys=False):
        if Path(path) == destination:
            raise OSError('unable to read published module')
        return read(path, is_sys)

    monkeypatch.setattr(manager, '_read_module', fail_published_read)
    with pytest.raises(ValueError, match='could not be registered'):
        await manager.install_from_file(package())
    unchanged(manager, destination, before, events)
