"""Language defaults and saved preferences in the jUPWR distribution."""

from types import SimpleNamespace

import pytest

from jamovi.server import i18n
from jamovi.server import session as session_module
from jamovi.server.backend import FileSystemBackend
from jamovi.server.utils import conf


@pytest.mark.parametrize('saved, configured, expected_setting, expected_language', [
    ({}, {}, 'pl', 'pl'),
    ({'selectedLanguage': 'en'}, {}, 'en', 'en'),
    ({'selectedLanguage': 'pl'}, {}, 'pl', 'pl'),
    ({'selectedLanguage': ''}, {}, '', None),
    ({}, {'lang': 'de'}, 'de', 'de'),
    ({'selectedLanguage': 'pl'}, {'lang': 'en'}, 'pl', 'pl'),
])
def test_session_language_preference(monkeypatch, tmp_path, saved, configured,
                                     expected_setting, expected_language):
    monkeypatch.setattr(conf, 'config_values', {'home': str(tmp_path), **configured})
    monkeypatch.setattr(i18n, '_code', None)
    monkeypatch.setattr(i18n, 'i18n_msgs', None)
    monkeypatch.setattr(FileSystemBackend, 'read_settings_nowait',
                        lambda self: {'main': saved})
    # Language selection must not depend on starting analysis workers.
    monkeypatch.setattr(session_module, 'EngineManager',
                        lambda *args: SimpleNamespace(add_engine_listener=lambda listener: None))

    session = session_module.Session(str(tmp_path), 'language-test')

    assert session._settings.group('main').get('selectedLanguage') == expected_setting
    assert session.get_language() == expected_language
    session.set_language('en-US,en;q=0.9')
    assert session.get_language() == (expected_language or 'en-US,en;q=0.9')
