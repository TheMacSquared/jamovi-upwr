// @vitest-environment jsdom

import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import type { Modules } from '../../modules';

const host = vi.hoisted(() => ({
    isElectron: true,
    getPathForFile: vi.fn((file: File) => `C:/Moduły testowe/${file.name}`),
    showOpenDialog: vi.fn(),
}));
vi.mock('../../host', () => ({ default: host }));

import PageSideload from '../pagesideload';

function drop(page: PageSideload, names: string[]) {
    const event = new Event('drop', { bubbles: true, cancelable: true });
    Object.defineProperty(event, 'dataTransfer', {
        value: { files: names.map(name => new File(['module'], name)) },
    });
    page.$drop.querySelector('span').dispatchEvent(event);
    return event;
}

beforeEach(() => {
    vi.clearAllMocks();
    (globalThis as any)._ = (text: string) => text;
    host.getPathForFile.mockImplementation(file => `C:/Moduły testowe/${file.name}`);
});
afterEach(() => document.body.replaceChildren());

describe('module sideloading', () => {
    function setup(install = vi.fn().mockResolvedValue(undefined)) {
        const page = new PageSideload({ install } as unknown as Modules);
        document.body.append(page);
        const notify = vi.fn();
        const close = vi.fn();
        page.addEventListener('notification', notify);
        page.addEventListener('close', close);
        return { page, install, notify, close };
    }

    it('installs dropped packages without opening them as datasets', async () => {
        const { page, install, notify, close } = setup();
        const openDataset = vi.fn();
        document.addEventListener('drop', openDataset, { once: true });
        const event = drop(page, ['jRISK.jmo', 'jSpace.JMO']);
        await vi.waitFor(() => expect(close).toHaveBeenCalledOnce());
        expect(event.defaultPrevented).toBe(true);
        expect(openDataset).not.toHaveBeenCalled();
        expect(install.mock.calls).toEqual([
            ['C:/Moduły testowe/jRISK.jmo'], ['C:/Moduły testowe/jSpace.JMO'],
        ]);
        expect(notify.mock.calls[0][0].detail.get('type')).toBe('success');
        document.removeEventListener('drop', openDataset);
    });

    it('shows install errors and keeps the panel open', async () => {
        const { page, notify, close } = setup(vi.fn().mockRejectedValue(new Error('Invalid module')));
        drop(page, ['invalid.jmo']);
        await vi.waitFor(() => expect(notify).toHaveBeenCalledOnce());
        expect(notify.mock.calls[0][0].detail.get('message')).toBe('Invalid module');
        expect(close).not.toHaveBeenCalled();
        expect(page.$drop.disabled).toBe(false);
    });

    it('reports an unavailable native path without calling install', async () => {
        const { page, install, notify, close } = setup();
        host.getPathForFile.mockReturnValue('');
        drop(page, ['jRISK.jmo']);
        await vi.waitFor(() => expect(notify).toHaveBeenCalledOnce());
        expect(install).not.toHaveBeenCalled();
        expect(close).not.toHaveBeenCalled();
    });

    it('still installs through the file picker', async () => {
        const { page, install, close } = setup();
        host.showOpenDialog.mockResolvedValue({ cancelled: false, paths: ['C:/jRISK.jmo'] });
        page.$drop.click();
        await vi.waitFor(() => expect(close).toHaveBeenCalledOnce());
        expect(install).toHaveBeenCalledWith('C:/jRISK.jmo');
    });

    it('ignores an empty drop', async () => {
        const { page, install, notify } = setup();
        drop(page, []);
        await Promise.resolve();
        expect(install).not.toHaveBeenCalled();
        expect(notify).not.toHaveBeenCalled();
    });
});
