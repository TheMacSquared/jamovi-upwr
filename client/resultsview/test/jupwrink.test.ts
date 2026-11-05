// @vitest-environment jsdom

import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import {
    encodeInk, decodeInk, inkKeyFromOption, contentFingerprint, fingerprint, distanceToSegment,
} from '../jupwrinkdata';
import { FrameInk, InkHost } from '../jupwrink';

describe('ink data', () => {

    it('round-trips, rounding coordinates but not widths too coarsely', () => {
        const value = encodeInk({ fp: 'abc', n: 0, s: [ { c: '#d62728', k: 'p', p: [ 1.234, 5.678, 3.456 ] } ] });
        expect(JSON.parse(value!)).toEqual({ v: 1, fp: 'abc', n: 0, s: [ { c: '#d62728', k: 'p', p: [ 1.2, 5.7, 3.46 ] } ] });
        expect(decodeInk(value)).toEqual({ fp: 'abc', n: 0, s: [ { c: '#d62728', k: 'p', p: [ 1.2, 5.7, 3.46 ] } ] });
    });

    it('keeps 4 decimals for coordinates relative to a plot size', () => {
        const value = encodeInk({ fp: '', n: 1, s: [ { c: '#000', k: 'h', p: [ 0.123456, 0.5, 18 ] } ] });
        expect(decodeInk(value)!.s[0].p).toEqual([ 0.1235, 0.5, 18 ]);
    });

    it('an element without strokes stores nothing', () => {
        expect(encodeInk({ fp: 'x', n: 0, s: [] })).toBeNull();
    });

    it('rejects what it cannot read instead of failing', () => {
        expect(decodeInk(null)).toBeNull();
        expect(decodeInk('')).toBeNull();
        expect(decodeInk('{not json')).toBeNull();
        expect(decodeInk(JSON.stringify({ v: 2, s: [] }))).toBeNull();
        const bad = decodeInk(JSON.stringify({ v: 1, fp: 'f', n: 0, s: [
            { c: '#000', k: 'p', p: [ 1, 2 ] },          // not triples
            { c: '#000', k: 'p', p: [ 1, 'x', 3 ] },     // not numbers
            { c: '#000', k: 'p', p: [ 1, 2, 3 ] },
        ] }));
        expect(bad!.s).toHaveLength(1);
    });

    it('finds the element address in an option name', () => {
        expect(inkKeyFromOption('results//ink')).toBe('');
        expect(inkKeyFromOption('results/descriptives/ink')).toBe('descriptives');
        expect(inkKeyFromOption('results/plots/"x"/ink')).toBe('plots/"x"');
        expect(inkKeyFromOption('results/plots/widthScale')).toBeNull();
        expect(inkKeyFromOption('vars')).toBeNull();
    });

    it('fingerprints a plot by its image file, anything else by its data', () => {
        expect(contentFingerprint('Image', { path: 'a/1.svg', width: 400 }))
            .toBe(contentFingerprint('Image', { path: 'a/1.svg', width: 500 }));
        expect(contentFingerprint('Image', { path: 'a/1.svg' }))
            .not.toBe(contentFingerprint('Image', { path: 'a/2.svg' }));
        expect(contentFingerprint('Table', { rows: [ 1 ] }))
            .not.toBe(contentFingerprint('Table', { rows: [ 2 ] }));
        expect(fingerprint('abc')).toMatch(/^[0-9a-f]{8}$/);
    });

    it('measures the distance to a segment, not just to its ends', () => {
        expect(distanceToSegment(50, 5, 0, 0, 100, 0)).toBeCloseTo(5);
        expect(distanceToSegment(-3, 4, 0, 0, 100, 0)).toBeCloseTo(5);
    });
});

// --- the frame ---

class FakeView extends HTMLElement {
    model: any;
    parentView: FakeView | null = null;
    children_: FakeView[] = [];
    kind = 'Table';
    $image?: HTMLElement;
    rect = new DOMRect(0, 0, 100, 100);
    address(): string[] {
        return this.parentView ? [ ...this.parentView.address(), this.model.attributes.name ] : [];
    }
    type() { return this.kind; }
    get(address: string[]): any {
        if (address.length === 0)
            return this;
        const child = this.children_.find(c => c.model.attributes.name === address[0]);
        return child ? child.get(address.slice(1)) : null;
    }
    getBoundingClientRect() { return this.rect; }
    getClientRects() { return [ this.rect ] as any; }
}
customElements.define('fake-view', FakeView);

function view(name: string, element: any, rect: DOMRect, parent: FakeView | null = null, kind = 'Table') {
    const v = document.createElement('fake-view') as FakeView;
    v.model = { attributes: { name, element } };
    v.rect = rect;
    v.kind = kind;
    if (parent) {
        v.parentView = parent;
        parent.children_.push(v);
        parent.append(v);
    }
    return v;
}

let root: FakeView;
let table: FakeView;
let plot: FakeView;
let host: InkHost & { setParam: any, post: any };
let ink: FrameInk;
let under: Element | null;

function pointer(type: string, x: number, y: number, extra: any = {}) {
    const e = new MouseEvent(type, { bubbles: true, cancelable: true, clientX: x, clientY: y, button: 0 });
    Object.assign(e, { pointerId: 1, pointerType: 'mouse', pressure: 0.5 });
    Object.defineProperty(e, 'pageX', { value: x });
    Object.defineProperty(e, 'pageY', { value: y });
    for (const [ k, v ] of Object.entries(extra))
        Object.defineProperty(e, k, { value: v });
    ink.overlay.dispatchEvent(e);
}

function draw(points: Array<[ number, number ]>, extra: any = {}) {
    const [ first, ...rest ] = points;
    pointer('pointerdown', first[0], first[1], extra);
    for (const [ x, y ] of rest)
        pointer('pointermove', x, y, extra);
    const last = points[points.length - 1];
    pointer('pointerup', last[0], last[1], extra);
}

const lastInk = () => {
    const calls = host.setParam.mock.calls;
    return calls.length ? { address: calls[calls.length - 1][0], doc: decodeInk(calls[calls.length - 1][1].ink) } : null;
};

beforeEach(() => {
    vi.stubGlobal('ResizeObserver', class { observe() {} unobserve() {} disconnect() {} });
    vi.stubGlobal('requestAnimationFrame', (fn: () => void) => { fn(); return 0; });
    root = view('', { title: 'Analiza' }, new DOMRect(0, 0, 600, 800));
    document.body.append(root);
    table = view('desc', { rows: [ [ 1, 2 ] ] }, new DOMRect(10, 100, 300, 200), root);
    plot = view('plot', { path: 'r/a.svg' }, new DOMRect(10, 400, 400, 300), root, 'Image');
    plot.$image = document.createElement('div');
    plot.$image.getBoundingClientRect = () => new DOMRect(20, 420, 400, 300);
    plot.$image.getClientRects = () => [ new DOMRect(20, 420, 400, 300) ] as any;
    plot.append(plot.$image);
    under = table;
    (document as any).elementsFromPoint = () => [ ink.overlay, under ];
    host = { results: () => root, setParam: vi.fn(), post: vi.fn() };
    ink = new FrameInk(host);
    ink.setState({ active: true, tool: 'pen', colour: '#d62728' });
});

afterEach(() => {
    document.body.replaceChildren();
    vi.unstubAllGlobals();
});

describe('FrameInk', () => {

    it('anchors a stroke to the element it starts on, in its pixels', () => {
        draw([ [ 30, 150 ], [ 80, 150 ], [ 80, 190 ] ]);
        const saved = lastInk()!;
        expect(saved.address).toEqual([ 'desc' ]);
        expect(saved.doc!.n).toBe(0);
        expect(saved.doc!.s[0].p.slice(0, 2)).toEqual([ 20, 50 ]);
        expect(saved.doc!.s[0].c).toBe('#d62728');
        expect(saved.doc!.fp).toBe(contentFingerprint('Table', table.model.attributes.element));
        expect(host.post).toHaveBeenCalledWith('jupwrInkAction', expect.objectContaining({ id: expect.any(String) }));
        expect(host.post).toHaveBeenCalledWith('annotationChanged');
    });

    it('anchors on a plot relative to its size, so the ink follows a resize', () => {
        under = plot;
        draw([ [ 220, 570 ], [ 420, 720 ] ]);
        const saved = lastInk()!;
        expect(saved.address).toEqual([ 'plot' ]);
        expect(saved.doc!.n).toBe(1);
        expect(saved.doc!.s[0].p.slice(0, 2)).toEqual([ 0.5, 0.5 ]);
        expect(saved.doc!.s[0].p.slice(3, 5)).toEqual([ 1, 1 ]);
    });

    it('does not draw while the pen is off', () => {
        ink.setState({ active: false });
        expect(ink.overlay.hidden).toBe(true);
        ink.setState({ active: true });
        expect(ink.overlay.hidden).toBe(false);
    });

    it('reads the saved ink of the analysis and draws it again', () => {
        const fp = contentFingerprint('Table', table.model.attributes.element);
        ink.sync({ 'results/desc/ink': encodeInk({ fp, n: 0, s: [ { c: '#1f5fbf', k: 'p', p: [ 0, 0, 3, 10, 10, 3 ] } ] }), vars: [ 'x' ] });
        const doc = ink.docs.get('desc')!;
        expect(doc.strokes).toHaveLength(1);
        expect(doc.g.getAttribute('transform')).toBe('translate(10 100) scale(1 1)');
        expect(doc.stale).toBe(false);
        expect(host.setParam).not.toHaveBeenCalled();
    });

    it('fades and marks ink when the element content changed; "Zachowaj" accepts it', () => {
        const fp = contentFingerprint('Table', table.model.attributes.element);
        ink.sync({ 'results/desc/ink': encodeInk({ fp, n: 0, s: [ { c: '#000', k: 'p', p: [ 0, 0, 3 ] } ] }) });
        table.model.attributes.element = { rows: [ [ 9, 9 ] ] };
        ink.layout();
        const doc = ink.docs.get('desc')!;
        expect(doc.stale).toBe(true);
        expect(doc.g.getAttribute('opacity')).toBe('0.35');
        expect(doc.badge!.hidden).toBe(false);
        expect(doc.badge!.textContent).toContain('wcześniejszej wersji');
        const keep = Array.from(doc.badge!.querySelectorAll('button')).find(b => b.textContent === 'Zachowaj')!;
        keep.click();
        expect(doc.stale).toBe(false);
        expect(doc.badge!.hidden).toBe(true);
        expect(lastInk()!.doc!.fp).toBe(contentFingerprint('Table', { rows: [ [ 9, 9 ] ] }));
    });

    it('"Usuń" on the mark removes the ink, and it can be undone', () => {
        ink.sync({ 'results/desc/ink': encodeInk({ fp: 'old', n: 0, s: [ { c: '#000', k: 'p', p: [ 0, 0, 3 ] } ] }) });
        const doc = ink.docs.get('desc')!;
        const remove = Array.from(doc.badge!.querySelectorAll('button')).find(b => b.textContent === 'Usuń')!;
        remove.click();
        expect(doc.strokes).toHaveLength(0);
        expect(lastInk()!.doc).toBeNull();
        const id = host.post.mock.calls.filter((c: any[]) => c[0] === 'jupwrInkAction').pop()[1].id;
        ink.undo(id);
        expect(doc.strokes).toHaveLength(1);
    });

    it('a resize by hand gives the plot a new image without marking its ink', () => {
        under = plot;
        draw([ [ 220, 570 ], [ 300, 600 ] ]);
        document.dispatchEvent(new CustomEvent('jupwr-image-resized', { detail: { address: [ 'plot' ] } }));
        plot.model.attributes.element = { path: 'r/b.svg' };
        ink.layout();
        expect(ink.docs.get('plot')!.stale).toBe(false);
        expect(lastInk()!.doc!.fp).toBe(contentFingerprint('Image', { path: 'r/b.svg' }));
        // a later re-run is a new content again
        plot.model.attributes.element = { path: 'r/c.svg' };
        ink.layout();
        expect(ink.docs.get('plot')!.stale).toBe(true);
    });

    it('the eraser removes whole strokes; undo puts them back', () => {
        draw([ [ 20, 120 ], [ 220, 120 ] ]);
        draw([ [ 20, 250 ], [ 220, 250 ] ]);
        ink.setState({ tool: 'eraser' });
        draw([ [ 120, 110 ], [ 120, 125 ] ]);
        const doc = ink.docs.get('desc')!;
        expect(doc.strokes).toHaveLength(1);
        expect(lastInk()!.doc!.s).toHaveLength(1);
        const id = host.post.mock.calls.filter((c: any[]) => c[0] === 'jupwrInkAction').pop()[1].id;
        ink.undo(id);
        expect(doc.strokes).toHaveLength(2);
    });

    it('the eraser saves once per gesture, however many strokes and moves', () => {
        for (let y = 110; y < 200; y += 10)
            draw([ [ 20, y ], [ 220, y ] ]);
        host.setParam.mockClear();
        ink.setState({ tool: 'eraser' });
        const path: Array<[ number, number ]> = [];
        for (let y = 100; y < 210; y += 2)
            path.push([ 120, y ]);
        draw(path);
        expect(ink.docs.get('desc')!.strokes).toHaveLength(0);
        expect(host.setParam).toHaveBeenCalledTimes(1);
        expect(lastInk()!.doc).toBeNull();
    });

    it('works out the content fingerprint once per version of the element data', () => {
        const stringify = vi.spyOn(JSON, 'stringify');
        draw([ [ 20, 120 ], [ 220, 120 ] ]);
        stringify.mockClear();
        for (let i = 0; i < 20; i++)
            ink.layout();
        expect(stringify).not.toHaveBeenCalled();
        table.model.attributes.element = { rows: [ [ 3 ] ] };
        ink.layout();
        expect(stringify).toHaveBeenCalledTimes(1);
        stringify.mockRestore();
    });

    it('clear removes all the ink of the analysis under the given id', () => {
        draw([ [ 20, 120 ], [ 220, 120 ] ]);
        under = plot;
        draw([ [ 220, 570 ], [ 300, 600 ] ]);
        ink.clear('c1');
        expect(ink.docs.get('desc')!.strokes).toHaveLength(0);
        expect(ink.docs.get('plot')!.strokes).toHaveLength(0);
        ink.undo('c1');
        expect(ink.docs.get('desc')!.strokes).toHaveLength(1);
        expect(ink.docs.get('plot')!.strokes).toHaveLength(1);
    });

    it('touch scrolls the results panel instead of drawing', () => {
        draw([ [ 50, 300 ], [ 50, 200 ] ], { pointerType: 'touch', screenX: 50, screenY: 300 });
        expect(ink.docs.size).toBe(0);
        expect(host.post).toHaveBeenCalledWith('jupwrInkScroll', expect.any(Object));
    });

    it('sends Escape and Ctrl+Z to the main window while the pen is on', () => {
        window.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', cancelable: true }));
        window.dispatchEvent(new KeyboardEvent('keydown', { key: 'z', code: 'KeyZ', ctrlKey: true, cancelable: true }));
        expect(host.post).toHaveBeenCalledWith('jupwrInkKey', { key: 'escape' });
        expect(host.post).toHaveBeenCalledWith('jupwrInkKey', { key: 'undo' });
        ink.setState({ active: false });
        host.post.mockClear();
        window.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape' }));
        expect(host.post).not.toHaveBeenCalled();
    });
});
