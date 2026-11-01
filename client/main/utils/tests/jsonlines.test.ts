import { describe, expect, it, vi } from 'vitest';
import { parse } from '../jsonlines';

function reader(chunks: (Uint8Array | string)[]) {
    let index = 0;
    return {
        read: vi.fn(async () => index < chunks.length
            ? { value: chunks[index++], done: false }
            : { value: undefined, done: true }),
        cancel: vi.fn(async () => undefined),
        releaseLock: vi.fn(),
    };
}

const encode = (text: string) => new TextEncoder().encode(text);

describe('JSON Lines response parser', () => {
    it('preserves a response split at every possible UTF-8 byte boundary', async () => {
        const result = { status: 'OK', title: 'Zażółć gęślą jaźń 🐄', instanceId: 'one' };
        const bytes = encode('{"status":"IN_PROGRESS","progress":25}\n' + JSON.stringify(result) + '\n');
        for (let split = 1; split < bytes.length; split++) {
            const source = reader([bytes.slice(0, split), bytes.slice(split)]);
            expect(await parse(source)).toEqual(result);
            expect(source.releaseLock).toHaveBeenCalledOnce();
        }
    });

    it('handles one byte per chunk, CRLF, empty lines and no final newline', async () => {
        const text = '\r\n{"progress":10}\r\n \n{"status":"OK","message":"Łąka"}';
        const source = reader([...encode(text)].map(byte => new Uint8Array([byte])));
        expect(await parse(source)).toEqual({ status: 'OK', message: 'Łąka' });
    });

    it('parses string chunks and several messages in one chunk', async () => {
        const source = reader(['{"pro', 'gress":1}\n{"progress":2}\n{"sta', 'tus":"OK"}\n\n']);
        expect(await parse(source)).toEqual({ status: 'OK' });
    });

    it('reports complete progress while retaining an incomplete next line', async () => {
        let finish;
        const gate = new Promise<any>(resolve => { finish = resolve; });
        const source = reader(['{"progress":10}\n{"status":']);
        source.read.mockImplementationOnce(async () => ({ value: '{"progress":10}\n{"status":', done: false }));
        source.read.mockImplementationOnce(() => gate);
        source.read.mockImplementationOnce(async () => ({ value: undefined, done: true }));
        const stream = parse(source);
        const next = await stream[Symbol.asyncIterator]().next();
        expect(next).toEqual({ done: false, value: { progress: 10 } });
        finish({ value: '"OK"}', done: false });
        expect(await stream).toEqual({ status: 'OK' });
    });

    it('processes data included with the end-of-stream marker', async () => {
        const source = reader([]);
        source.read.mockResolvedValue({ value: encode('{"status":"OK"}'), done: true });
        expect(await parse(source)).toEqual({ status: 'OK' });
    });

    it.each(['{"status":', '{"progress":1}\n{"status":', '{bad}\n{"status":"OK"}\n'])
    ('rejects malformed or truncated JSON instead of returning stale progress: %s', async text => {
        const source = reader([encode(text)]);
        await expect(parse(source)).rejects.toBeInstanceOf(SyntaxError);
        expect(source.cancel).toHaveBeenCalledOnce();
        expect(source.releaseLock).toHaveBeenCalledOnce();
    });

    it('rejects invalid UTF-8 instead of silently changing a string', async () => {
        const source = reader([encode('{"title":"'), new Uint8Array([0xc5])]);
        await expect(parse(source)).rejects.toThrow();
        expect(source.cancel).toHaveBeenCalledOnce();
    });

    it('propagates read errors and releases the reader even if cancellation fails', async () => {
        const source = reader([]);
        source.read.mockRejectedValue(new Error('connection lost'));
        source.cancel.mockRejectedValue(new Error('cancel failed'));
        await expect(parse(source)).rejects.toThrow('connection lost');
        expect(source.releaseLock).toHaveBeenCalledOnce();
    });

    it('returns undefined for an empty response', async () => {
        expect(await parse(reader(['\n \r\n']))).toBeUndefined();
    });
});
