

import os
import os.path as path
import platform

import threading
from time import monotonic
import subprocess
from enum import Enum

import nanomsg

from .jamovi_pb2 import ComsMessage
from .jamovi_pb2 import AnalysisStatus
from .jamovi_pb2 import AnalysisRequest
from .jamovi_pb2 import AnalysisResponse
from .jamovi_pb2 import Status as MessageStatus

import logging
from asyncio import get_event_loop
from asyncio import Queue
from asyncio import ensure_future as create_task
from asyncio import CancelledError
from asyncio import wait
from asyncio import wait_for
from asyncio import TimeoutError
from asyncio import sleep
from asyncio import create_subprocess_exec
from asyncio import Event
from asyncio import Lock
from asyncio import FIRST_COMPLETED
from asyncio import current_task
from asyncio import gather
from asyncio import to_thread
from asyncio import shield

from .utils import req_str
from .i18n import _


MESSAGE_COMPLETE = MessageStatus.Value('COMPLETE')
MESSAGE_ERROR = MessageStatus.Value('ERROR')
MESSAGE_IN_PROGRESS = MessageStatus.Value('IN_PROGRESS')

ANALYSIS_INITED = AnalysisStatus.Value('ANALYSIS_INITED')
ANALYSIS_COMPLETE = AnalysisStatus.Value('ANALYSIS_COMPLETE')
ANALYSIS_ERROR = AnalysisStatus.Value('ANALYSIS_ERROR')

PERFORM_INIT = AnalysisRequest.Perform.Value('INIT')
PERFORM_RUN = AnalysisRequest.Perform.Value('RUN')
PERFORM_SAVE = AnalysisRequest.Perform.Value('SAVE')


log = logging.getLogger(__name__)


class Engine:

    class Status(Enum):
        WAITING = 0
        INITING = 1
        RUNNING = 2
        OPPING = 3  # performing operation

    def __init__(self, parent, data_path, conn_root, config, monitor=None):
        self._parent = parent
        self._data_path = data_path
        self._conn_root = conn_root
        self._config = config
        self._monitor = monitor

        allow_arbitrary_code = config.get('allow_arbitrary_code', 'true')
        self._allow_arbitrary_code = not (allow_arbitrary_code == 'false' or allow_arbitrary_code == '0')

        self._conn_path = None
        self._process = None
        self._process_stopping = None
        self._process_abandoned = None
        self._socket = None
        self._thread = None

        # if a crash happens at start up, well, that's bad
        self._at_startup = True

        self._message_id = 0

        self._running = Event()
        self._stopped = Event()
        self._stopped.set()

        # start() and stop() both leave the engine half-built partway through
        # (_conn_path is claimed before the process is spawned, but not bound
        # until after), so they mustn't overlap -- two starts racing would bind
        # the same conn path twice, leaving an engine process nothing can reach
        self._lifecycle = Lock()
        self._run_lock = Lock()

        self._current_analysis = None
        self._restart_task = None

        self._ioloop = get_event_loop()

        self._results_queue = Queue()

        dur_limit = self._config.get('analysis_duration_limit', None)
        if dur_limit:
            dur_limit = int(dur_limit)
        self._analysis_duration_limit = dur_limit

        # jUPWR: after a cancellation (an option changed during a calculation)
        # the worker gets this long to finish the cancelled request; only if
        # it does not is it restarted. A restart costs an R start and the
        # package loading, which every click during a calculation used to pay.
        self._cancel_grace = float(self._config.get('analysis_cancel_grace', 2.0))

    async def start(self):
        async with self._lifecycle:
            if not self._running.is_set():
                await self._start()

    async def _start(self):

        self._at_startup = True
        self._results_queue = Queue()
        self._process_stopping = threading.Event()
        self._process_abandoned = threading.Event()

        if self._socket is not None:
            try:
                self._socket.close()
            except Exception as e:
                log.exception(e)
            self._socket = None

        self._conn_path = f'{self._conn_root}-{self._parent._next_conn_index}'
        self._parent._next_conn_index += 1

        bin_dir = 'bin' if platform.system() != 'Darwin' else 'MacOS'
        exe_dir = path.join(self._config.get('home'), bin_dir)
        exe_path = path.join(exe_dir, 'jamovi-engine')

        env = os.environ.copy()
        env['R_HOME'] = self._config.get('r_home', env.get('R_HOME', ''))
        env['R_LIBS'] = self._config.get('r_libs', env.get('R_LIBS', ''))
        env['FONTCONFIG_PATH'] = self._config.get('fontconfig_path', env.get('FONTCONFIG_PATH', ''))
        env['JAMOVI_MODULES_PATH'] = self._config.get('modules_path', env.get('JAMOVI_MODULES_PATH', ''))
        env['PATH'] = self._config.get('path', env.get('PATH', ''))

        if platform.uname().system == 'Linux':
            # plotting under linux sometimes doesn't work without this
            env['LC_ALL'] = 'en_US.UTF-8'
            # https://github.com/jamovi/jamovi/issues/801
            # https://github.com/jamovi/jamovi/issues/831

        elif platform.uname().system == 'Windows':
            # lubridate doesn't work without this
            env['TZDIR'] = f'{ env["R_HOME"] }\\share\\zoneinfo'

        con = '--con={}'.format(self._conn_path)
        pth = '--path={}'.format(self._data_path)

        try:
            if platform.uname().system == 'Windows':
                si = subprocess.STARTUPINFO()
                # makes the engine windows visible in debug mode (on windows)
                if not self._config.get('debug', False):
                    si.dwFlags |= subprocess.STARTF_USESHOWWINDOW

                self._process = subprocess.Popen(
                    [ exe_path, con, pth ],
                    startupinfo=si,
                    stdout=None,  # stdouts seem to break things on windows
                    stderr=None,
                    env=env,
                    cwd=exe_dir)  # set the cwd to prevent picking up stray .dlls
            else:
                # stdin=PIPE, because the engines use the closing of
                # stdin to terminate themselves.
                self._process = await create_subprocess_exec(
                    exe_path, con, pth,
                    stdout=None,
                    stderr=None,
                    stdin=subprocess.PIPE,
                    env=env)

            mem_limit = self._config.get('memory_limit_engine', None)
            if mem_limit:
                if platform.uname().system == 'Linux':
                    import resource
                    try:
                        limit = int(mem_limit) * 1024 * 1024  # Mb
                        resource.prlimit(self._process.pid, resource.RLIMIT_AS, (limit, limit))
                    except ValueError:
                        raise ValueError('memory_limit_engine: bad value')
                else:
                    raise ValueError('memory_limit_engine is unavailable on systems other than linux')

            if self._monitor is not None:
                self._monitor.monitor(self._process)

            self._socket = nanomsg.Socket(nanomsg.PAIR)
            self._socket._set_recv_timeout(500)

            # max size of nanomsg messages ... default was 1Mb under macOS
            self._socket.set_int_option(nanomsg.SOL_SOCKET, nanomsg.RCVMAXSIZE, 4 * 1024 * 1024)
            self._socket.bind(self._conn_path)

            # need a separate thread for nanomsg :/
            self._thread = threading.Thread(target=self._run_loop, args=(
                self._socket,
                self._process,
                self._process_stopping,
                self._process_abandoned))
            self._thread.start()

            self._stopped.clear()
            self._running.set()

        except (Exception, CancelledError) as e:
            await self._stop()
            if not isinstance(e, CancelledError):
                log.exception(e)
                self._parent._notify_engine_event({
                    'type': 'error',
                    'message': 'Engine process could not be started',
                    'cause': str(e),
                })
            raise

    async def stop(self):
        cleanup = create_task(self._stop_lifecycle())
        try:
            await shield(cleanup)
        except CancelledError:
            await gather(cleanup, return_exceptions=True)
            raise

    async def _stop_lifecycle(self):
        restart_task = self._restart_task
        if restart_task is not None and restart_task is not current_task():
            restart_task.cancel()
            await gather(restart_task, return_exceptions=True)
        async with self._lifecycle:
            await self._stop()

    async def _wait_process(self, process):
        try:
            if isinstance(process, subprocess.Popen):
                await to_thread(process.wait, timeout=1)
            else:
                await wait_for(process.wait(), 1)
        except (TimeoutError, subprocess.TimeoutExpired):
            pass

    async def _send(self, data, timeout=10):
        deadline = self._ioloop.time() + timeout
        while True:
            if self._stopped.is_set():
                raise RuntimeError('Analysis engine stopped before receiving the request')
            try:
                self._socket.send(data, flags=nanomsg.DONTWAIT)
                return
            except nanomsg.NanoMsgAPIError as e:
                if e.errno not in (nanomsg.EAGAIN, nanomsg.ETIMEDOUT):
                    raise
                if self._ioloop.time() >= deadline:
                    raise TimeoutError('Timed out connecting to the analysis engine') from e
                # Startup may load R libraries before the IPC peer connects.
                await sleep(0.01)

    async def _stop(self):
        process = self._process
        if self._process_stopping is not None:
            self._process_stopping.set()

        if process is not None and process.returncode is None:
            if self._socket is not None and self._running.is_set():
                self._message_id += 1
                request = AnalysisRequest(restartEngines=True)
                message = ComsMessage(id=self._message_id,
                                     payload=request.SerializeToString(),
                                     payloadType='AnalysisRequest')
                try:
                    await self._send(message.SerializeToString(), timeout=1)
                    await self._wait_process(process)
                except Exception as e:
                    log.exception(e)

            for action in ('terminate', 'kill'):
                if process.returncode is not None:
                    break
                try:
                    getattr(process, action)()
                    await self._wait_process(process)
                except ProcessLookupError:
                    break
                except Exception as e:
                    log.exception(e)

        # Late callbacks from this generation must not stop its replacement.
        if self._process_abandoned is not None:
            self._process_abandoned.set()
        if self._socket is not None:
            try:
                self._socket.close()
            except Exception as e:
                log.debug('Socket close failed: %s', e)
        if self._thread is not None and self._thread.is_alive():
            await to_thread(self._thread.join, 1)
        if process is not None and process.stdin is not None:
            process.stdin.close()
        self._process = None
        self._socket = None
        self._thread = None
        self._notify_process_ended()

    async def run(self, request, results_stream):
        if results_stream.done():
            return
        if request.arbitraryCode and not self._allow_arbitrary_code:
            results_stream.set_result(self._create_error_response(
                request,
                _('Analyses which execute arbitrary code are not permitted in this session.')))
            return

        task = current_task()
        if self._current_analysis is not None and not self._current_analysis.done():
            self._current_analysis.cancel()
        self._current_analysis = task
        helpers = set()
        sent = False
        settled = False
        results_received = None

        try:
            async with self._run_lock:
                if results_stream.done():
                    return
                if self._restart_task is not None:
                    await shield(self._restart_task)
                if not self._running.is_set():
                    raise RuntimeError('Analysis engine is not running')
                request.restartEngines = False
                message = ComsMessage(id=self._message_id,
                                     payload=request.SerializeToString(),
                                     payloadType='AnalysisRequest')
                sent = True
                await self._send(message.SerializeToString())
                self._message_id += 1
                self._at_startup = False

                results_received = create_task(self._results_queue.get())
                engine_stopped = create_task(self._stopped.wait())
                helpers.update((results_received, engine_stopped))
                pending = {results_received, engine_stopped, results_stream}
                timeout = None
                if self._analysis_duration_limit is not None:
                    timeout = create_task(sleep(self._analysis_duration_limit))
                    helpers.add(timeout)
                    pending.add(timeout)

                # jUPWR: a cancellation is settled while the lock is held, so the
                # next request is sent only after the worker finished the
                # cancelled one or was restarted (and its results cannot be
                # taken by this task)
                try:
                    while True:
                        done, pending = await wait(pending, return_when=FIRST_COMPLETED)
                        # Cancellation can race with the delivery of a final result.
                        if results_stream.done():
                            if results_stream.cancelled():
                                await self._finish_or_interrupt(request, results_received)
                            break
                        if results_received in done:
                            helpers.discard(results_received)
                            results, complete = results_received.result()
                            if (request.instanceId == results.instanceId
                                    and request.analysisId == results.analysisId
                                    and request.revision == results.revision):
                                if complete:
                                    results_stream.set_result(results)
                                    break
                                results_stream.write(results)
                            results_received = create_task(self._results_queue.get())
                            helpers.add(results_received)
                            pending.add(results_received)
                        elif timeout is not None and timeout in done:
                            error = self._create_error_response(
                                request,
                                _('This analysis has exceeded the current time limits and has been terminated.'))
                            await self._recover()
                            if not results_stream.done():
                                results_stream.set_result(error)
                            break
                        elif engine_stopped in done:
                            error = self._create_error_response(
                                request,
                                _('This analysis has terminated, likely due to hitting a resource limit.'))
                            await self._recover()
                            if not results_stream.done():
                                results_stream.set_result(error)
                            break
                except CancelledError:
                    results_stream.cancel()
                    settled = True
                    try:
                        await self._finish_or_interrupt(request, results_received)
                    except CancelledError:
                        await self._interrupt()
                    raise
        except CancelledError:
            results_stream.cancel()
            if sent and not settled:
                await self._interrupt()
            raise
        except Exception as e:
            log.exception(e)
            try:
                if self._process is not None:
                    await self._recover()
            finally:
                if not results_stream.done():
                    results_stream.set_exception(e)
        finally:
            for helper in helpers:
                helper.cancel()
            await gather(*helpers, return_exceptions=True)
            if self._current_analysis is task:
                self._current_analysis = None

    async def _finish_or_interrupt(self, request, receiver):
        # jUPWR: wait up to _cancel_grace for the final result of the
        # cancelled request (R does not stop computing it); when it comes the
        # worker is idle again and keeps running. Otherwise interrupt.
        deadline = monotonic() + self._cancel_grace
        task = receiver
        created = []
        try:
            while self._cancel_grace > 0:
                remaining = deadline - monotonic()
                if remaining <= 0:
                    break
                if task is None or task.cancelled():
                    task = create_task(self._results_queue.get())
                    created.append(task)
                done, _ = await wait({task}, timeout=remaining)
                if task not in done:
                    break
                try:
                    results, complete = task.result()
                except Exception:
                    task = None
                    continue
                task = None
                if (complete and request.instanceId == results.instanceId
                        and request.analysisId == results.analysisId
                        and request.revision == results.revision):
                    return
        finally:
            for t in created:
                if not t.done():
                    t.cancel()
        await self._interrupt()

    async def _interrupt(self):
        # A cancelled R calculation may never return to the IPC loop. Reap the
        # worker before admitting another calculation. Shutdown/restart already
        # owns worker cleanup and must not start a replacement process.
        ready = getattr(self._parent, '_ready', None)
        stopping = self._process_stopping
        if (self._process is not None
                and not getattr(self._parent, '_stopping', False)
                and (ready is None or ready.is_set())
                and (stopping is None or not stopping.is_set())):
            await self._recover()

    async def _recover(self):
        if self._restart_task is None or self._restart_task.done():
            self._restart_task = create_task(self.restart())
            self._restart_task.add_done_callback(self._restart_done)
        # Replacing an analysis must not interrupt recovery of its worker.
        await shield(self._restart_task)

    async def restart(self):
        async with self._lifecycle:
            # Reap the previous process even if its exit callback has run.
            await self._stop()
            log.info('Restarting engine')
            await self._start()

    def _notify_process_ended(self):
        self._running.clear()
        self._stopped.set()

    def _run_loop(self, socket, process, stopping_flag, abandoned_flag):
        parent = threading.main_thread()

        try:
            while parent.is_alive():
                try:
                    bytes = socket.recv()

                    message = ComsMessage()
                    message.ParseFromString(bytes)
                    complete = (message.status != MESSAGE_IN_PROGRESS)

                    results = AnalysisResponse()
                    results.ParseFromString(message.payload)

                    try:
                        self._ioloop.call_soon_threadsafe(self._results_queue.put_nowait, (results, complete))
                    except RuntimeError:
                        # loop already closed during shutdown -- nothing to deliver
                        break

                except nanomsg.NanoMsgAPIError as e:
                    if e.errno != nanomsg.ETIMEDOUT and e.errno != nanomsg.EAGAIN:
                        raise e

                if abandoned_flag.is_set():
                    break
                if isinstance(process, subprocess.Popen):
                    process.poll()
                if process.returncode is not None:
                    break
        except nanomsg.NanoMsgAPIError as e:
            # we expect this exception if the process is stopping
            if not stopping_flag.is_set():
                raise e
        finally:
            try:
                self._ioloop.call_soon_threadsafe(self._on_terminated, process.returncode, stopping_flag, abandoned_flag)
            except RuntimeError:
                # loop already closed during shutdown -- nothing to notify
                pass
            try:
                socket.close()
            except nanomsg.NanoMsgAPIError:
                # socket may already be closed (e.g. by stop() to unblock recv,
                # or by nn_term during shutdown) -- a double close raises here
                pass

    def _on_terminated(self, return_code, stopping_flag, abandoned_flag):
        if abandoned_flag.is_set():
            return
        self._notify_process_ended()
        if self._current_analysis is None or self._current_analysis.done():
            if not stopping_flag.is_set():
                # spontaneous crash ... not caused by an analysis
                log.error('Engine crash (spontaneous) with exit code {}\n'.format(return_code))
                if self._at_startup:
                    # if the engine crashes at start up, we're in trouble
                    # this probably means the service isn't viable
                    self._parent._notify_engine_event({
                        'type': 'error',
                        'message': 'Engine process terminated',
                        'cause': 'Exit code: {}'.format(return_code),
                    })
                else:
                    # crash while analysis not running
                    # unusual, but recoverable -- perform a restart
                    self._restart_task = create_task(self.restart())
                    self._restart_task.add_done_callback(self._restart_done)
            else:
                # intentional stop
                pass
        else:
            # termination while analysis running
            # restarting is handled elsewhere
            pass

    def _restart_done(self, task):
        if self._restart_task is task:
            self._restart_task = None
        if not task.cancelled():
            try:
                task.result()
            except Exception as e:
                log.exception(e)

    def _create_error_response(self, request, message):
        results = AnalysisResponse()
        results.instanceId = request.instanceId
        results.analysisId = request.analysisId
        results.name = request.name
        results.ns = request.ns
        results.options.CopyFrom(request.options)
        results.status = ANALYSIS_ERROR
        results.revision = request.revision
        results.version = 0

        results.results.name = request.name
        results.results.title = request.name
        results.results.status = ANALYSIS_ERROR
        results.results.error.message = message

        item = results.results.group.elements.add()
        item.preformatted = ''

        return results

    def __del__(self):
        if self._process is not None and self._process.returncode is None:
            try:
                self._process.terminate()
            except (OSError, ProcessLookupError):
                pass
