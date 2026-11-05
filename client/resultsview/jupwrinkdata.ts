'use strict';

// jUPWR: handwriting saved with the results -- the data side (no DOM), shared
// by the results frame (jupwrink.ts) and the tests.
//
// The ink of one results element is one analysis option,
// results/<element address>/ink, holding a JSON string; such options are
// saved in the .omv and do not re-run the analysis (server options.py).
//
//   { "v": 1,            format version
//     "fp": "1a2b3c4d",  fingerprint of the element's content when drawn on
//     "n": 1,            1: coordinates relative to the element's size
//                        (plots, so the ink follows a resize); 0: in pixels
//     "s": [ { "c": "#d62728", "k": "p" | "h", "p": [ x, y, w, x, y, w, ... ] } ] }
//
// x, y are relative to the element's top-left corner; w is the line width
// in pixels (never scaled).

export const INK_OPTION = 'ink';
export const INK_VERSION = 1;

export type InkKind = 'p' | 'h';  // pen, highlighter

export interface InkStroke {
    c: string;
    k: InkKind;
    p: number[];
}

export interface InkDoc {
    fp: string;
    n: 0 | 1;
    s: InkStroke[];
}

export function encodeInk(doc: InkDoc): string | null {
    if (doc.s.length === 0)
        return null;
    const digits = doc.n ? 4 : 1;
    const round = (v: number, d: number) => Number(v.toFixed(d));
    const s = doc.s.map(stroke => ({
        c: stroke.c,
        k: stroke.k,
        p: stroke.p.map((v, i) => round(v, i % 3 === 2 ? 2 : digits)),
    }));
    return JSON.stringify({ v: INK_VERSION, fp: doc.fp, n: doc.n, s });
}

export function decodeInk(value: unknown): InkDoc | null {
    if (typeof value !== 'string' || value === '')
        return null;
    try {
        const raw = JSON.parse(value);
        if ( ! raw || raw.v !== INK_VERSION || ! Array.isArray(raw.s))
            return null;
        const s: InkStroke[] = raw.s
            .filter((st: any) => st && typeof st.c === 'string' && Array.isArray(st.p)
                && st.p.length >= 3 && st.p.length % 3 === 0
                && st.p.every((v: unknown) => typeof v === 'number' && isFinite(v)))
            .map((st: any) => ({ c: st.c, k: st.k === 'h' ? 'h' : 'p', p: st.p }));
        return { fp: typeof raw.fp === 'string' ? raw.fp : '', n: raw.n ? 1 : 0, s };
    }
    catch (e) {
        return null;
    }
}

// 'results/<address>/ink' -> the address key ('' is the analysis itself)
export function inkKeyFromOption(name: string): string | null {
    const prefix = 'results/';
    const suffix = '/' + INK_OPTION;
    if ( ! name.startsWith(prefix) || ! name.endsWith(suffix))
        return null;
    return name.slice(prefix.length, name.length - suffix.length);
}

// FNV-1a, 32 bit: a short, stable fingerprint, not a security hash
export function fingerprint(text: string): string {
    let h = 0x811c9dc5;
    for (let i = 0; i < text.length; i++) {
        h ^= text.charCodeAt(i);
        h = Math.imul(h, 0x01000193);
    }
    return (h >>> 0).toString(16).padStart(8, '0');
}

// what identifies the content of a results element: for a plot its image
// file (a new file per render), otherwise the element's data
export function contentFingerprint(type: string, element: any): string {
    if (type === 'Image')
        return fingerprint('img:' + (element && element.path ? element.path : ''));
    return fingerprint(JSON.stringify(element ?? null));
}

export function distanceToSegment(px: number, py: number, ax: number, ay: number, bx: number, by: number): number {
    const dx = bx - ax;
    const dy = by - ay;
    const length2 = dx * dx + dy * dy;
    const t = length2 === 0 ? 0 : Math.max(0, Math.min(1, ((px - ax) * dx + (py - ay) * dy) / length2));
    return Math.hypot(px - (ax + t * dx), py - (ay + t * dy));
}
