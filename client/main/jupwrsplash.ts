'use strict';

// jUPWR: the loading screen with the UPWr logo (markup and style in
// index.html, so it shows before the app loads). It goes away when the
// window is ready, and earlier whenever the app has something to show
// itself: the progress of opening a file, a sign-in, an error.

import { JUPWR_VERSION } from '../common/jupwr';

// never stay over the app longer than this, whatever happens
const FALLBACK_MS = 60000;
const FADE_MS = 400;

let hidden = false;

export function initSplash(version: Promise<string>) {
    const splash = document.getElementById('jupwr-splash');
    if ( ! splash)
        return;
    const $version = splash.querySelector('.jupwr-splash-version');
    if ($version) {
        $version.textContent = `jUPWR ${ JUPWR_VERSION }`;
        version.then(v => {
            if (v)
                $version.textContent = `jUPWR ${ JUPWR_VERSION } (jamovi ${ shortVersion(v) })`;
        }, () => { });
    }
    setTimeout(hideSplash, FALLBACK_MS);
}

export function hideSplash() {
    if (hidden)
        return;
    hidden = true;
    const splash = document.getElementById('jupwr-splash');
    if ( ! splash)
        return;
    splash.classList.add('done');
    setTimeout(() => splash.remove(), FADE_MS);
}

// 28.17.0.0 -> 28.17.0
export function shortVersion(version: string): string {
    return version.split('.').slice(0, 3).join('.');
}
