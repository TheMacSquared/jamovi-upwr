
import os
import os.path
import logging
import shutil
import json
import re
import stat
from tempfile import mkdtemp
from asyncio import Lock
from zipfile import ZipFile
from collections import namedtuple
from functools import lru_cache

import yaml

from jamovi.core import Dirs
from jamovi.core import PlatformInfo

import jamovi.server.i18n as i18n
from jamovi.server.utils import conf
from jamovi.server.downloader import Downloader
from jamovi.server.downloader import DownloadError
from jamovi.server.utils.stream import ProgressStream
from jamovi.server.utils.stream import progress_stream
from jamovi.server.utils.zipverify import verify_jmo
from jamovi.server.appinfo import determine_r_version
from jamovi.server.appinfo import app_info

try:
    from yaml import CSafeLoader as Loader
except ImportError:
    from yaml import SafeLoader as Loader


log = logging.getLogger('jamovi')


LibraryContent = namedtuple('Library', 'message modules')


class LibraryError(Exception):
    pass


class DataSetMeta:
    def __init__(self):
        self.name = ''
        self.title = ''
        self.description = ''
        self.path = ''
        self.tags: list = []


class License:
    def __init__(self):
        self.name = ''
        self.url = ''


class ModuleMeta:
    def __init__(self):
        self.name = ''
        self.title = ''
        self.build_time = 0
        self.version = [0, 0, 0]
        self.description = ''
        self.authors: list = []
        self.analyses: list = []
        self.datasets: list = []
        self.path = ''
        self.is_sys = False
        self.new = False
        self.min_app_version = [0, 0, 0, 0]
        self.license: License | None = None
        self.datasets_license: License | None = None
        self.visible = True
        self.incompatible = False
        self.i18n_msgs: dict | None = None
        self.category = ''
        self._code: str | None = None
        self.index = 1

    def get(self, name):
        for analysis in self.analyses:
            if analysis.name == name:
                return analysis
        else:
            return None

    def __getitem__(self, name):
        for analysis in self.analyses:
            if analysis.name == name:
                return analysis
        else:
            raise KeyError

    @property
    def language(self):
        return self._code

    def set_language(self, code):
        if code != self._code:
            self.i18n_msgs = None
            self._code = code

    def load_translations(self, code):
        self.set_language(code)
        self.i18n_msgs = self._load_translations(code)
        return bool(self.i18n_msgs)

    @lru_cache(None)
    def _load_translations(self, code):
        if code:
            i18n_root = os.path.join(self.path, 'R', self.name, 'i18n', f'{code}.json')
            if os.path.exists(i18n_root):
                with open(i18n_root, 'r', encoding='utf-8') as stream:
                    i18n_def = json.load(stream)
                    return i18n_def['locale_data']['messages']

        return { }

    def translate(self, value):
        if value == '':
            return value

        if self.i18n_msgs is None and self._code:
            self.i18n_msgs = self._load_translations(self._code)

        translation = value

        if self.i18n_msgs:
            trans = self.i18n_msgs.get(value)
            if trans is not None:
                translation = trans[0].strip()
                if translation == '':
                    translation = value

        return translation


class AnalysisMeta:
    def __init__(self):
        self.name = ''
        self.ns = ''
        self.title = ''
        self.ribbon = ''
        self.menuGroup = ''
        self.menuSubgroup = ''
        self.menuTitle = ''
        self.menuSubtitle = ''
        self.addon_for: list | None = None
        self.addons: list = []
        self.in_menu = True
        self.defn: dict = {}
        self.translated = ''
        self.category = ''

    def translate_defaults(self, module_meta, code):
        if self.translated == code:
            return

        if not module_meta.load_translations(code):
            return

        options_defn = self.defn['options']
        for opt_defn in options_defn:
            if 'name' not in opt_defn or 'type' not in opt_defn:
                continue
            self.tag_translatable_defaults(module_meta, opt_defn)
        self.translated = code

    def tag_translatable_defaults(self, module_meta, opt_defn, _default_value=None):
        if _default_value is None:
            if 'default' in opt_defn and opt_defn['default'] is not None:
                translated = self.tag_translatable_defaults(module_meta, opt_defn, opt_defn['default'])
                if translated is not None:
                    opt_defn['default'] = translated
                return None
        else:
            typ  = opt_defn['type']
            if typ == 'String':
                return  module_meta.translate(_default_value)
            elif typ == 'Group':
                for element in opt_defn['elements']:
                    translated = self.tag_translatable_defaults(module_meta, element, _default_value[element['name']])
                    if translated is not None:
                        _default_value[element['name']] = translated
            elif typ == 'Array':
                for i, value in enumerate(_default_value):
                    translated = self.tag_translatable_defaults(module_meta, opt_defn['template'], value)
                    if translated is not None:
                        _default_value[i] = translated

        return _default_value


LIBRARY_ROOT = f'https://library.jamovi.org/{app_info.os}/R{app_info.r_version}'


class Modules:

    def __init__(self, settings):
        self._settings = settings
        self._read = False
        self._modules = [ ]
        self._listeners = [ ]
        self._original: list | None = None
        self._read_task = None
        self._install_lock = Lock()

    def get(self, name):
        for module in self._modules:
            if module.name == name:
                return module
        raise KeyError()

    def __getitem__(self, name):
        for module in self._modules:
            if module.name == name:
                return module
        raise KeyError()

    def __contains__(self, name):
        for module in self._modules:
            if module.name == name:
                return True
        return False

    def add_listener(self, listener):
        self._listeners.append(listener)

    def remove_listener(self, listener):
        self._listeners.remove(listener)

    def _notify_listeners(self, event):
        for listener in self._listeners:
            listener(event)

    async def read(self):
        if self._read is False:
            await self._reread_installed()
            self._original = list(map(lambda x: x.name, self._modules))

    def read_library(self) -> ProgressStream:

        if self._read_task is not None:
            self._read_task.cancel()

        self._read_task = self._read_library()
        return self._read_task

    @progress_stream
    async def _read_library(self, progress):
        lang_code = i18n.get_language()
        if lang_code and not lang_code.startswith('en'):
            lang_stream = Downloader.download(
                f'{LIBRARY_ROOT}/index-{lang_code}')
            try:
                result = await lang_stream
                return self._parse_index(result)
            except DownloadError as e:
                if e.status != 404:
                    raise

        url = f'{LIBRARY_ROOT}/index'
        result = await Downloader.download(url)
        return self._parse_index(result)

    def _parse_index(self, path):
        try:
            message = None
            modules = [ ]
            module_data = yaml.load(path, Loader=Loader)
            if 'jds' not in module_data:
                raise LibraryError('No jds')
            jds = float(module_data['jds'])
            if jds > 1.4:
                raise ValueError('The library requires a newer version of jamovi. Please upgrade to the latest version of jamovi.')
            message = module_data.get('message')
            for defn in module_data['modules']:
                module = self._parse_module_defn(defn)
                modules.append(module)

            content = LibraryContent(message, modules)
            return content
        except Exception as e:
            log.exception(e)
            raise ValueError('Unable to parse module list. Please try again later.') from e

    def _read_module(self, path, is_sys=False):
        defn = None
        for leaf in ('jamovi-full.yaml', 'jamovi.yaml'):
            try:
                meta_path = os.path.join(path, leaf)
                with open(meta_path, encoding='utf-8') as stream:
                    defn = yaml.load(stream, Loader=Loader)
            except FileNotFoundError:
                continue
            else:
                break

        return self._parse_module_defn(defn, path, is_sys)


    def _read_modules(self, pth, is_system):

        modules = [ ]

        for entry in os.scandir(pth):
            if entry.name == 'base' or entry.name.startswith('.'):
                continue
            if entry.is_dir() is False:
                continue
            try:
                module = self._read_module(entry.path, is_system)
                if not is_system:
                    module.new = self._read and module.name not in (self._original or [])
                modules.append(module)
            except Exception as e:
                log.exception(e)

        modules = list(sorted(modules, key=lambda m: m.index))

        return modules


    def _attach_addons(self):
        # fill in addons
        modules_by_name = dict(map(lambda m: (m.name, m), self))

        for module in self:
            for analysis in module.analyses:
                del analysis.addons[:]

        for module in self:
            for analysis in module.analyses:
                if analysis.addon_for is not None:
                    try:
                        (ns, name) = analysis.addon_for
                        target_module = modules_by_name.get(ns)
                        target_analysis = next(iter(filter(lambda a: a.name == name, target_module.analyses)))
                        target_analysis.addons.append((analysis.ns, analysis.name))
                    except Exception:
                        pass


    async def _reread_installed(self):

        module_path = conf.get('modules_path')
        user_module_path = None

        if os.name != 'nt':
            module_paths = module_path.split(':')
        else:
            module_paths = module_path.split(';')

        user_module_path = os.path.join(Dirs.app_data_dir(), 'modules')
        os.makedirs(user_module_path, exist_ok=True)


        modules = [ ]

        try:
            for pth in module_paths:
                try:
                    modules_here = self._read_modules(pth, is_system=True)
                    modules += modules_here
                except Exception as e:
                    log.exception(e)

            try:
                modules_here = self._read_modules(user_module_path, is_system=False)
                modules += modules_here
            except Exception as e:
                log.exception(e)

            self._read = True
        except Exception as e:
            log.exception(e)

        self._modules = modules
        self._attach_addons()

    def set_visibility(self, name, value):
        for module in self._modules:
            if module.name == name:
                module.visible = value
                return True
        return False

    async def uninstall(self, name):
        async with self._install_lock:
            module_dir = os.path.join(Dirs.app_data_dir(), 'modules', name)
            shutil.rmtree(module_dir)
            await self._reread_installed()
            self._notify_listeners({ 'type': 'modulesChanged' })

    @staticmethod
    def _archive_module_name(zip_file):
        # Zip paths are portable: reject names which change meaning on Windows,
        # and require every member to belong to the same module directory.
        module_name = None
        seen = set()
        for member in zip_file.infolist():
            name = member.filename
            parts = name.rstrip('/').split('/')
            if (member.orig_filename != name or '\\' in name or ':' in name
                    or any(part in ('', '.', '..') or part.endswith(('.', ' ')) for part in parts)
                    or stat.S_ISLNK(member.external_attr >> 16)):
                raise ValueError('Invalid module archive path')
            root = parts[0]
            if (not re.fullmatch(r'[A-Za-z][A-Za-z0-9._-]*', root)
                    or root.casefold() == 'base'):
                raise ValueError('Invalid module directory name')
            if module_name is None:
                module_name = root
            elif root != module_name:
                raise ValueError('A module archive must contain one module directory')
            key = os.path.normcase(name.rstrip('/'))
            if key in seen:
                raise ValueError('Duplicate module archive entry')
            seen.add(key)
        if module_name is None:
            raise ValueError('Empty module archive')
        return module_name

    def _validate_candidate(self, module_path, module_name):
        meta = self._read_module(module_path)
        if meta.name != module_name:
            raise ValueError('Module name does not match its directory')
        if meta.incompatible:
            raise ValueError('This module requires a different version of R')
        # Compare zero-padded versions so 28.17 and 28.17.0.0 are equivalent.
        required = (meta.min_app_version + [0] * 4)[:4]
        current = ([int(part) for part in app_info.version.split('.')] + [0] * 4)[:4]
        if required > current:
            raise ValueError('This module requires a newer version of jamovi')
        if meta.analyses:
            package_dir = os.path.join(module_path, 'R', module_name)
            for leaf in ('DESCRIPTION', 'NAMESPACE', 'Meta/package.rds',
                         f'R/{module_name}.rdx', f'R/{module_name}.rdb'):
                if not os.path.isfile(os.path.join(package_dir, leaf)):
                    raise ValueError('Module archive is missing its installed R package')
        for dataset in meta.datasets:
            parts = dataset.path.split('/')
            if (not parts or any(part in ('', '.', '..') for part in parts)
                    or '\\' in dataset.path or ':' in dataset.path
                    or not os.path.isfile(os.path.join(module_path, 'data', *parts))):
                raise ValueError('Module archive is missing a declared dataset')
        return meta

    async def install_from_file(self, path, update: bool = False):
        async with self._install_lock:
            await self._install_from_file(path, update)

    async def _install_from_file(self, path, update):
        module_dir = os.path.join(Dirs.app_data_dir(), 'modules')
        os.makedirs(module_dir, exist_ok=True)
        # Staging and backup stay on the destination filesystem for rename.
        staging = mkdtemp(prefix='.install-', dir=module_dir)
        backup = os.path.join(staging, 'previous')
        module_path = None
        published = False
        previous_modules, previous_read = self._modules, self._read
        try:
            with ZipFile(path) as zip_file:
                module_name = self._archive_module_name(zip_file)
                incoming = os.path.join(staging, 'incoming')
                zip_file.extractall(incoming)
            candidate = os.path.join(incoming, module_name)
            meta = self._validate_candidate(candidate, module_name)
            module_path = os.path.join(module_dir, module_name)
            if os.path.lexists(module_path):
                if os.path.islink(module_path) or not os.path.isdir(module_path):
                    raise ValueError('Module destination is not a directory')
                os.rename(module_path, backup)
            os.rename(candidate, module_path)
            published = True
            await self._reread_installed()
            # Directory scans tolerate unreadable modules. A missing candidate
            # here is an installation failure and must still trigger rollback.
            if not any(module.name == meta.name and module.version == meta.version
                       and os.path.normcase(module.path) == os.path.normcase(module_path)
                       for module in self._modules):
                raise ValueError('Installed module could not be registered')
        except BaseException:
            # This also covers cancellation during a registry refresh.
            self._modules, self._read = previous_modules, previous_read
            try:
                if published:
                    os.rename(module_path, candidate)
                if os.path.exists(backup):
                    os.rename(backup, module_path)
            except OSError:
                log.exception('Unable to restore module; previous copy preserved at %s', backup)
                raise
            raise
        else:
            if os.path.exists(backup):
                try:
                    shutil.rmtree(backup)
                except OSError:
                    # A loaded DLL may be locked on Windows. Installation has
                    # succeeded; preserve the backup for later cleanup.
                    log.warning('Unable to remove old module backup: %s', backup,
                                exc_info=True)
        finally:
            # If rollback or backup cleanup failed, never delete the last copy
            # of the previous module. Hidden staging dirs are not modules.
            if not os.path.exists(backup):
                shutil.rmtree(staging, ignore_errors=True)

        events = []
        if update:
            events.append({ 'type': 'moduleUpdated', 'data': { 'name': meta.name }})
        events.append({ 'type': 'modulesChanged' })
        for event in events:
            try:
                self._notify_listeners(event)
            except Exception:
                # Notification failures must not turn a committed install into
                # an apparent failure, nor remove the successfully installed copy.
                log.exception('Unable to notify listeners after module installation')

    @progress_stream
    async def install(self, progress, path: str):
        if path.startswith(LIBRARY_ROOT):
            in_stream = Downloader.download(path)
            try:
                async for p in in_stream:
                    progress(p)
                path = await in_stream
            finally:
                in_stream.cancel()  # does nothing if it's done
            verify_jmo(path)

        await self.install_from_file(path)
        return (1, 1)

    def __iter__(self):
        return self._modules.__iter__()

    def _parse_module_defn(self, defn, path='', is_sys=False):
        module = ModuleMeta()
        module.path = path
        module.is_sys = is_sys
        module.name = str(defn['name'])
        module.title = str(defn['title'])
        module.build_time = defn.get('build-time', '')
        if 'description' in defn:
            module.description = str(defn['description'])
        else:
            module.description = ''

        if 'index' in defn:
            module.index = defn['index']

        if path != '':
            module.path = path
        else:
            for arch in defn.get('architectures', [ ]):
                if arch['name'] == '*' or arch['name'] in PlatformInfo.platform():
                    module.path = f'{LIBRARY_ROOT}/{arch["path"]}'
                    break
            else:
                module.path = ''

        version = defn['version'].split('.')
        version = version[:4]
        version = list(map(int, version))
        module.version = version

        if 'category' in defn:
            module.category = str(defn['category'])
        else:
            module.category = ''

        mod_r_v = defn.get('rVersion', None)
        mod_r_v = determine_r_version(mod_r_v)

        incompatible = False
        if mod_r_v != app_info.r_version:
            incompatible = True

        if 'analyses' in defn and len(defn['analyses']) > 0:
            # we don't need to mark data-sets-only modules as
            # incompatible
            module.incompatible = incompatible

        if 'requires' in defn and 'jamovi' in defn['requires']:
            min_app_version = defn['requires']['jamovi'].lstrip('>=').strip().split('.')
            min_app_version = list(map(int, min_app_version))
        else:
            min_app_version = [0, 0, 0, 0]

        module.min_app_version = min_app_version

        module.authors = [ ]
        if 'authors' in defn:
            module.authors.extend(defn['authors'])

        if module.incompatible:
            return module

        if 'analyses' in defn:
            for analysis_defn in defn['analyses']:
                analysis = AnalysisMeta()
                analysis.name = analysis_defn['name']
                analysis.ns = analysis_defn['ns']
                analysis.title = analysis_defn['title']
                analysis.defn = analysis_defn

                analysis.menuGroup = analysis_defn['menuGroup']
                analysis.menuTitle = analysis_defn['menuTitle']
                analysis.category = analysis_defn.get('category', 'analyses')

                if 'menuSubgroup' in analysis_defn and analysis_defn['menuSubgroup'] is not None:
                    analysis.menuSubgroup = analysis_defn['menuSubgroup']
                if 'menuSubtitle' in analysis_defn and analysis_defn['menuSubtitle'] is not None:
                    analysis.menuSubtitle = analysis_defn['menuSubtitle']
                if 'addonFor' in analysis_defn:
                    analysis.in_menu = False
                    analysis.addon_for = analysis_defn['addonFor'].split('::')
                if 'hidden' in analysis_defn and analysis_defn['hidden'] is True:
                    analysis.in_menu = False

                module.analyses.append(analysis)

        if 'datasets' in defn:
            for dataset_defn in defn['datasets']:
                dataset = DataSetMeta()
                dataset.name = dataset_defn['name']
                dataset.path = dataset_defn['path']
                dataset.description = dataset_defn['description']
                if 'tags' in dataset_defn:
                    dataset.tags[:] = dataset_defn['tags']
                module.datasets.append(dataset)

        if 'license' in defn:
            license_info = defn['license']
            if 'name' in license_info:
                module.license = License()
                module.license.name = license_info['name']
                module.license.url = license_info['url']
            else:
                if 'main' in license_info:
                    module.license = License()
                    module.license.name = license_info['main']['name']
                    module.license.url  = license_info['main']['url']
                    module.datasets_license = License()
                    module.datasets_license.name = license_info['main']['name']
                    module.datasets_license.url  = license_info['main']['url']
                if 'datasets' in license_info:
                    module.datasets_license = License()
                    module.datasets_license.name = license_info['datasets']['name']
                    module.datasets_license.url  = license_info['datasets']['url']

        return module
