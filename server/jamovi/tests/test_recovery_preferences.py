"""Recovery preferences are live, validated and persisted in the user profile."""

import json
from types import SimpleNamespace

import pytest
import pytest_asyncio

from jamovi.server import session as session_code
from jamovi.server import enginemanager as manager_code
from jamovi.server import jamovi_pb2 as pb
from jamovi.server.utils import conf
from jamovi.tests.test_analysis_lifecycle import FakeEngine
from jamovi.tests.test_instance import FakeComs


@pytest_asyncio.fixture
async def preferences(tmp_path, monkeypatch):
    monkeypatch.setattr(session_code, 'Dirs', SimpleNamespace(app_data_dir=lambda: str(tmp_path)))
    monkeypatch.setattr(manager_code, 'Engine', FakeEngine)
    monkeypatch.setitem(conf.config_values, 'backend', 'file')
    monkeypatch.delitem(conf.config_values, 'recovery_interval', raising=False)
    session = session_code.Session(str(tmp_path), 'preferences')
    try:
        yield session, tmp_path
    finally:
        await session._settings.flush()
        await session._scheduler.close()
        await session._runner.stop()


@pytest.mark.asyncio
async def test_new_profile_defaults_to_five_minutes(preferences):
    session, _ = preferences
    assert session._recovery_interval == 300
    assert session._settings.group('main').get('recoveryInterval') == 300


@pytest.mark.asyncio
@pytest.mark.parametrize('value', [0, 60, 120, 300, 600, 900])
async def test_settings_request_changes_interval_and_survives_restart(preferences, value):
    session, directory = preferences
    instance = await session.create()
    instance.set_coms(FakeComs())
    request = pb.SettingsRequest()
    request.settings.add(name='recoveryInterval', valueType=pb.INT, i=value)
    instance._on_settings(request)
    assert session._recovery_interval == value
    await session._settings.flush()
    assert json.loads((directory / 'settings.json').read_text())['main']['recoveryInterval'] == value
    reopened = session_code.Session(str(directory), 'reopened')
    try:
        assert reopened._recovery_interval == value
    finally:
        await reopened._scheduler.close()
        await reopened._runner.stop()


@pytest.mark.asyncio
@pytest.mark.parametrize('kind, value', [('INT', -1), ('DOUBLE', 1.5), ('BOOL', False), ('STRING', '60')])
async def test_invalid_request_does_not_change_interval(preferences, kind, value):
    session, _ = preferences
    instance = await session.create()
    instance.set_coms(FakeComs())
    request = pb.SettingsRequest()
    option = request.settings.add(name='recoveryInterval', valueType=pb.ValueType.Value(kind))
    setattr(option, {'INT': 'i', 'DOUBLE': 'd', 'BOOL': 'b', 'STRING': 's'}[kind], value)
    instance._on_settings(request)
    assert session._recovery_interval == 300


@pytest.mark.asyncio
async def test_configured_default_and_saved_preference_precedence(preferences, monkeypatch):
    session, _ = preferences
    monkeypatch.setitem(conf.config_values, 'recovery_interval', '120')
    session._specify_defaults(session._settings)
    assert session._recovery_interval == 120
    session._settings.group('main').set('recoveryInterval', 600)
    assert session._recovery_interval == 600
    session._settings.group('main').set('recoveryInterval', 'invalid')
    assert session._recovery_interval == 120
