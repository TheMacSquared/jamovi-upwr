import { ProgressStream } from './progressstream';

export function parse(reader) {
    return new ProgressStream(async (setProgress) => {
        const decoder = new TextDecoder('utf-8', { fatal: true });
        let buffer = '';
        let message;

        function consume(line: string) {
            if (line.trim() === '')
                return;
            message = JSON.parse(line);
            setProgress(message);
        }

        try {
            for (;;) {
                const { done, value } = await reader.read();
                if (typeof value === 'string')
                    buffer += decoder.decode() + value;
                else if (value)
                    buffer += decoder.decode(value, { stream: true });
                if (done)
                    buffer += decoder.decode();

                const lines = buffer.split('\n');
                buffer = lines.pop() ?? '';
                for (const line of lines)
                    consume(line);

                if (done) {
                    // A final newline is optional, but an incomplete final JSON
                    // must fail rather than return an earlier progress as success.
                    consume(buffer);
                    return message;
                }
            }
        }
        catch (error) {
            try {
                await reader.cancel?.();
            }
            catch {
                // Preserve the original parse/read error if cancellation fails.
            }
            throw error;
        }
        finally {
            reader.releaseLock?.();
        }
    });
}
