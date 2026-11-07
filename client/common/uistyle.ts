'use strict';

// jUPWR: interface style chosen in the preferences. The style is exposed as
// html[data-ui-style] in the main window and in the options and results
// iframes; each style lives in its own stylesheet scoped by that attribute
// (main/jupwr-ui-a.css, analysisui/jupwr-ui-a.css, resultsview/jupwr-ui-a.css)
// so the classic look stays the untouched jamovi CSS

export const UI_STYLE_CLASSIC = 'classic';
// 'a-dim' is style A on a warm grey ground: it sets data-ui-style="a" (so all
// of style A applies) plus data-ui-tone="dim", which only swaps the colour
// tokens (common/jupwr-colors.css)
export const UI_STYLES = [ UI_STYLE_CLASSIC, 'a', 'a-dim' ];

export function sanitizeUiStyle(style: unknown): string {
    return typeof style === 'string' && UI_STYLES.includes(style) ? style : UI_STYLE_CLASSIC;
}

export function applyUiStyle(style: unknown) {
    const sanitized = sanitizeUiStyle(style);
    const root = document.documentElement;
    root.setAttribute('data-ui-style', sanitized === 'a-dim' ? 'a' : sanitized);
    if (sanitized === 'a-dim')
        root.setAttribute('data-ui-tone', 'dim');
    else
        root.removeAttribute('data-ui-tone');
}
