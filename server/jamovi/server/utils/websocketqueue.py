"""One bounded sender per connection; progress is replaceable, replies are not."""

from asyncio import CancelledError, create_task, gather, wait_for
from collections import OrderedDict
from logging import getLogger

log = getLogger(__name__)


class WebSocketQueue:
    def __init__(self, ws, max_messages, max_bytes, timeout):
        self._ws = ws
        self._max_messages = max_messages
        self._max_bytes = max_bytes
        self._timeout = timeout
        self._pending = OrderedDict()
        self._bytes = 0
        self._sequence = 0
        self._writer = None
        self._closer = None
        self._closed = False
        self.failed = False

    def send(self, data, progress_key=None, final_key=None):
        if self._closed:
            return
        # Terminal replies supersede pending progress for that request.
        for key in (progress_key, final_key):
            if key is not None and key in self._pending:
                self._bytes -= len(self._pending.pop(key))
        if (len(self._pending) >= self._max_messages
                or self._bytes + len(data) > self._max_bytes):
            # Never silently discard a reply. Reconnect restores instance state.
            self._fail()
            return
        self._sequence += 1
        key = progress_key if progress_key is not None else self._sequence
        self._pending[key] = data
        self._bytes += len(data)
        if self._writer is None or self._writer.done():
            self._writer = create_task(self._drain())

    async def _drain(self):
        try:
            while self._pending and not self._closed:
                _, data = self._pending.popitem(last=False)
                try:
                    await wait_for(self._ws.send_bytes(data), self._timeout)
                finally:
                    self._bytes -= len(data)
        except CancelledError:
            raise
        except Exception:
            log.warning('WebSocket send failed', exc_info=True)
            self._fail()

    def _fail(self):
        if self._closed:
            return
        self.failed = True
        self.close()
        self._closer = create_task(self._close_socket())

    async def _close_socket(self):
        try:
            await wait_for(self._ws.close(code=1013, message=b'Slow WebSocket consumer'),
                           self._timeout)
        except Exception:
            log.debug('WebSocket close failed', exc_info=True)

    def close(self):
        self._closed = True
        self._pending.clear()
        # Keep the bytes of an in-flight send until its finally block runs.
        if self._writer is not None and not self._writer.done():
            self._writer.cancel()

    async def wait_closed(self):
        self.close()
        await gather(*(task for task in (self._writer, self._closer) if task is not None),
                     return_exceptions=True)
        self._bytes = 0
