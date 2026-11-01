// @vitest-environment jsdom

import { beforeEach, describe, expect, it, vi } from 'vitest';

const host = vi.hoisted(() => ({ isElectron: true, showMessageBox: vi.fn() }));
vi.mock('../../host', () => ({ default: host }));
import { confirmDeleteAllResults } from '../confirmdeleteall';

beforeEach(() => {
    vi.restoreAllMocks();
    vi.clearAllMocks();
    host.isElectron = true;
    (globalThis as any)._ = (text: string) => text;
});

describe('delete all results confirmation', () => {
    it('uses cancellation for the default button and Escape', async () => {
        host.showMessageBox.mockResolvedValue(0);
        expect(await confirmDeleteAllResults()).toBe(false);
        expect(host.showMessageBox).toHaveBeenCalledWith(expect.objectContaining({
            type: 'warning', buttons: ['Cancel', 'Delete all'], defaultId: 0, cancelId: 0,
            detail: expect.stringContaining('cannot be undone'),
        }));
    });

    it('accepts only an explicit destructive button selection', async () => {
        host.showMessageBox.mockResolvedValue(1);
        expect(await confirmDeleteAllResults()).toBe(true);
        host.showMessageBox.mockResolvedValue(undefined);
        expect(await confirmDeleteAllResults()).toBe(false);
    });

    it.each([false, true])('honours browser confirmation: %s', async answer => {
        host.isElectron = false;
        const confirm = vi.spyOn(window, 'confirm').mockReturnValue(answer);
        expect(await confirmDeleteAllResults()).toBe(answer);
        expect(confirm).toHaveBeenCalledWith(expect.stringContaining('output columns'));
        expect(host.showMessageBox).not.toHaveBeenCalled();
    });
});
