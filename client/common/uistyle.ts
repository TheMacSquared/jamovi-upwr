'use strict';

// jUPWR: interface style chosen in the preferences. The style is exposed as
// html[data-ui-style] in the main window and in the options and results
// iframes; each style lives in its own stylesheet scoped by that attribute
// (main/jupwr-ui-a.css, analysisui/jupwr-ui-a.css, resultsview/jupwr-ui-a.css)
// so the classic look stays the untouched jamovi CSS

export const UI_STYLE_CLASSIC = 'classic';
export const UI_STYLES = [ UI_STYLE_CLASSIC, 'a' ];

export function sanitizeUiStyle(style: unknown): string {
    return typeof style === 'string' && UI_STYLES.includes(style) ? style : UI_STYLE_CLASSIC;
}

export function applyUiStyle(style: unknown) {
    document.documentElement.setAttribute('data-ui-style', sanitizeUiStyle(style));
}
