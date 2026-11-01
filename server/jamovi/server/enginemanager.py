
import platform

import tempfile
from uuid import uuid4

from asyncio import gather
from asyncio import Event
from asyncio import Lock
from asyncio import shield
from asyncio import ensure_future as create_task
from asyncio import Queue
from asyncio import QueueFull
from asyncio import CancelledError


from .utils import req_str
from .engine import Engine

import logging

log = logging.getLogger(__name__)


class EngineManager:

    def __init__(self, data_path, queue, config, monitor=None):

        self._data_path = data_path
        self._queue = queue
        self._config = config
        self._monitor = monitor

        self._requests = [ None ] * queue.qsize
        self._engines = [ None ] * queue.qsize
        self._next_conn_index = 0

        self._message_id = 1
        self._listeners = [ ]
        self._notifications = Queue(maxsize=0)

        if platform.uname().system == 'Windows':
            self._conn_root = "ipc://{}".format(str(uuid4()))
        else:
            self._dir = tempfile.TemporaryDirectory()  # assigned to self so it doesn't get cleaned up
            self._conn_root = "ipc://{}/conn".format(self._dir.name)

        for index in range(queue.qsize):
            engine = Engine(
                parent=self,
                data_path=data_path,
                conn_root=self._conn_root,
                config=self._config,
                monitor=self._monitor)
            self._engines[index] = engine

        self._tasks = set()
        self._ready = Event()
        self._lifecycle = Lock()
        self._stopping = False
        self._run_loop_task = create_task(self._run_loop())

        mem_limit = self._config.get('memory_limit_engine', None)
        if mem_limit and platform.uname().system == 'Linux':
            log.info('Applying engine memory limit %s Mb', mem_limit)

    def notifications(self):
        return self._notifications

    async def _run_loop(self):
        try:
            async for analysis in self._queue.stream():
                task = create_task(self._run_analysis(analysis))
                self._tasks.add(task)
                task.add_done_callback(self._analysis_done)
        except CancelledError:
            raise
        except Exception as e:
            log.exception(e)
            self._queue.close()
            self._notify_engine_event({
                'type': 'error',
                'message': 'Analysis dispatch failed',
                'cause': str(e),
            })
        finally:
            await self._cancel_analyses()

    def _analysis_done(self, task):
        self._tasks.discard(task)
        if not task.cancelled():
            try:
                task.result()
            except Exception as e:
                log.exception(e)

    async def _cancel_analyses(self):
        tasks = list(self._tasks)
        for task in tasks:
            task.cancel()
        await gather(*tasks, return_exceptions=True)

    async def _run_analysis(self, analysis):
        request, stream = analysis
        index = None
        try:
            await self._ready.wait()
            if stream.done():
                return

            for candidate, value in enumerate(self._requests):
                if value is not None:
                    ex_request, _ = value
                    if (request.instanceId == ex_request.instanceId
                            and request.analysisId == ex_request.analysisId):
                        index = candidate
                        break
            if index is None:
                for candidate, value in enumerate(self._requests):
                    if value is None or value[1].done():
                        index = candidate
                        break
                else:
                    raise QueueFull

            log.debug('%s %s on %s', 'running', req_str(request), index)
            self._requests[index] = (request, stream)
            await self._engines[index].run(request, stream)
            if not stream.done():
                raise RuntimeError('Analysis engine returned without a result')
        except CancelledError:
            stream.cancel()
            raise
        except Exception as e:
            log.exception(e)
            if not stream.done():
                stream.set_exception(e)
        finally:
            if index is not None and self._requests[index] == (request, stream):
                self._requests[index] = None

    async def _run_engines(self, method):
        tasks = [create_task(getattr(engine, method)()) for engine in self._engines]
        try:
            results = await gather(*tasks, return_exceptions=True)
        except CancelledError:
            for task in tasks:
                task.cancel()
            await gather(*tasks, return_exceptions=True)
            raise
        for result in results:
            if isinstance(result, BaseException):
                raise result

    async def start(self):
        async with self._lifecycle:
            if self._stopping:
                raise RuntimeError('Engine manager is stopped')
            try:
                await self._run_engines('start')
            except (Exception, CancelledError):
                try:
                    await self._stop()
                except Exception as e:
                    log.exception(e)
                raise
            self._ready.set()

    async def stop(self):
        async with self._lifecycle:
            cleanup = create_task(self._stop())
            try:
                await shield(cleanup)
            except CancelledError:
                await gather(cleanup, return_exceptions=True)
                raise

    async def _stop(self):
        if self._stopping:
            return
        self._stopping = True
        self._ready.clear()
        self._queue.close()
        self._run_loop_task.cancel()
        await gather(self._run_loop_task, return_exceptions=True)
        await self._cancel_analyses()
        try:
            await self._run_engines('stop')
        finally:
            if hasattr(self, '_dir'):
                self._dir.cleanup()

    async def restart_engines(self):
        async with self._lifecycle:
            if self._stopping:
                raise RuntimeError('Engine manager is stopped')
            self._ready.clear()
            await self._cancel_analyses()
            try:
                await self._run_engines('restart')
            except (Exception, CancelledError):
                try:
                    await self._stop()
                except Exception as e:
                    log.exception(e)
                raise
            self._ready.set()

    def add_engine_listener(self, listener):
        self._listeners.append(('engine-event', listener))

    def _notify_engine_event(self, *args):
        for listener in self._listeners:
            if listener[0] == 'engine-event':
                listener[1](*args)
