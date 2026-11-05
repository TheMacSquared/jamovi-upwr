// @vitest-environment jsdom

import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

beforeEach(() => {
    vi.resetModules();
    vi.useFakeTimers();
    document.body.innerHTML = `
        <div id="jupwr-splash"><div class="jupwr-splash-version"></div></div>`;
});

afterEach(() => {
    vi.useRealTimers();
    document.body.replaceChildren();
});

describe('loading screen', () => {

    it('shows both versions, then fades out and is removed', async () => {
        const { initSplash, hideSplash } = await import('../jupwrsplash');
        initSplash(Promise.resolve('28.17.0.0'));
        await Promise.resolve();
        const splash = document.getElementById('jupwr-splash')!;
        expect(splash.textContent).toMatch(/^jUPWR \S+ \(jamovi 28\.17\.0\)$/);
        hideSplash();
        expect(splash.classList.contains('done')).toBe(true);
        expect(document.getElementById('jupwr-splash')).not.toBeNull();
        vi.advanceTimersByTime(500);
        expect(document.getElementById('jupwr-splash')).toBeNull();
        // hiding again is harmless
        hideSplash();
    });

    it('goes away by itself if the app never reports ready', async () => {
        const { initSplash } = await import('../jupwrsplash');
        initSplash(new Promise(() => { }));
        vi.advanceTimersByTime(59000);
        expect(document.getElementById('jupwr-splash')!.classList.contains('done')).toBe(false);
        vi.advanceTimersByTime(2000);
        expect(document.getElementById('jupwr-splash')).toBeNull();
    });
});
