'use strict';

// jUPWR: handwriting saved with the results -- the results frame side.
//
// The pen is switched on and its tool chosen in the main window
// (main/jupwrink.ts), which tells every results frame. While it is on, a
// transparent overlay over this frame takes the pointer: a stroke belongs to
// the results element it starts on (table, plot, note, heading...), is
// stored relative to that element and saved as the element's 'ink' results
// option (jupwrinkdata.ts). Touch and the wheel scroll the results panel.
//
// When the element's content is no longer what was drawn on (a re-run with
// other options), its ink is faded and marked, with "keep" / "remove".

import { flatten, unflatten } from '../common/utils/addresses';
import {
    INK_OPTION, InkKind, InkStroke, decodeInk, encodeInk, inkKeyFromOption,
    contentFingerprint, distanceToSegment,
} from './jupwrinkdata';

const SVG_NS = 'http://www.w3.org/2000/svg';
const PEN_WIDTH = 3;
const HIGHLIGHTER_WIDTH = 18;
const HIGHLIGHTER_OPACITY = 0.4;
const ERASER_RADIUS = 10;
const STALE_OPACITY = 0.35;

export type InkTool = 'pen' | 'highlighter' | 'eraser';

export interface InkState {
    active: boolean;
    tool: InkTool;
    colour: string;
    highlighter: string;
}

// what the frame needs from the results view, kept narrow for the tests
export interface InkHost {
    results(): any | null;                       // root results View
    setParam(address: string[], options: any): void;
    post(type: string, data?: any): void;        // to the main window
}

interface RStroke extends InkStroke {
    el: SVGGElement;
}

interface Doc {
    key: string;
    address: string[];
    fp: string;
    n: 0 | 1;
    strokes: RStroke[];
    g: SVGGElement;
    badge: HTMLElement | null;
    stale: boolean;
}

type Action = { type: 'add' | 'remove', items: { doc: Doc, stroke: RStroke }[] };

type Anchor = { left: number, top: number, sx: number, sy: number, n: 0 | 1, fp: string };

// the fingerprint of an element's data, once per version of the data: the
// results replace the element object when its content changes, and a table
// or the whole analysis is costly to serialise on every layout
const fingerprints = new WeakMap<object, string>();
function cachedFingerprint(type: string, element: any): string {
    if (element === null || typeof element !== 'object')
        return contentFingerprint(type, element);
    let fp = fingerprints.get(element);
    if (fp === undefined) {
        fp = contentFingerprint(type, element);
        fingerprints.set(element, fp);
    }
    return fp;
}

let actionCounter = 0;
const newActionId = () => `${ Date.now().toString(36) }-${ ++actionCounter }`;

export class FrameInk {

    host: InkHost;
    state: InkState = { active: false, tool: 'pen', colour: '#d62728', highlighter: '#ffd400' };

    layer: SVGSVGElement;
    overlay: HTMLElement;
    docs: Map<string, Doc> = new Map();
    actions: Map<string, Action> = new Map();
    resized: Set<string> = new Set();

    private current: { doc: Doc, stroke: RStroke, anchor: Anchor, last: { x: number, y: number, w: number } | null } | null = null;
    private erased: { doc: Doc, stroke: RStroke }[] = [];
    private eraseAnchors: Map<Doc, Anchor> | null = null;
    // per stroke, while erasing: its bounding box on the page, widened by
    // the eraser, so the strokes far from the pointer are skipped at once
    private eraseBoxes: Map<RStroke, [ number, number, number, number ]> = new Map();
    private pointerId: number | null = null;
    private touch: { x: number, y: number } | null = null;
    private layoutQueued = false;

    constructor(host: InkHost) {
        this.host = host;

        this.layer = document.createElementNS(SVG_NS, 'svg');
        this.layer.classList.add('jupwr-ink-layer', 'ignore-html');
        this.layer.setAttribute('aria-hidden', 'true');
        document.body.append(this.layer);

        this.overlay = document.createElement('div');
        this.overlay.className = 'jupwr-ink-overlay ignore-html';
        this.overlay.hidden = true;
        document.body.append(this.overlay);

        this.overlay.addEventListener('pointerdown', e => this.onPointerDown(e));
        this.overlay.addEventListener('pointermove', e => this.onPointerMove(e));
        this.overlay.addEventListener('pointerup', e => this.onPointerUp(e));
        this.overlay.addEventListener('pointercancel', e => this.onPointerUp(e));
        this.overlay.addEventListener('wheel', e => {
            e.preventDefault();
            const scale = e.deltaMode === 1 ? 16 : e.deltaMode === 2 ? 400 : 1;
            this.host.post('jupwrInkScroll', { dx: e.deltaX * scale, dy: e.deltaY * scale });
        }, { passive: false });
        window.addEventListener('keydown', e => this.onKeyDown(e), true);

        // a plot resized by hand gets a new image file; that is not a new
        // content, so its ink keeps its fingerprint (image.ts sends this)
        document.addEventListener('jupwr-image-resized', (e: Event) => {
            const address = (e as CustomEvent).detail?.address;
            if (Array.isArray(address))
                this.resized.add(flatten(address));
        });

        if (typeof ResizeObserver !== 'undefined')
            new ResizeObserver(() => this.queueLayout()).observe(document.body);
        window.addEventListener('resize', () => this.queueLayout());
    }

    // --- from the main window ---

    setState(state: Partial<InkState>) {
        Object.assign(this.state, state);
        this.overlay.hidden = ! this.state.active;
        this.overlay.dataset.tool = this.state.tool;
        if ( ! this.state.active)
            this.endStroke();
        this.layout();
    }

    undo(id: string) {
        const action = this.actions.get(id);
        if ( ! action)
            return;
        this.actions.delete(id);
        if (action.type === 'add')
            this.detach(action.items);
        else
            this.attach(action.items);
    }

    clear(id: string) {
        const items: { doc: Doc, stroke: RStroke }[] = [];
        for (const doc of this.docs.values())
            for (const stroke of doc.strokes)
                items.push({ doc, stroke });
        if (items.length === 0)
            return;
        this.detach(items);
        this.actions.set(id, { type: 'remove', items });
    }

    // --- from the results ---

    // the ink options of the analysis, after each render of the results;
    // only elements not yet known are read: the frame's own copy is newer
    sync(options: { [name: string]: any } | null | undefined) {
        if (options) {
            for (const name in options) {
                const key = inkKeyFromOption(name);
                if (key === null || this.docs.has(key))
                    continue;
                const data = decodeInk(options[name]);
                if (data === null || data.s.length === 0)
                    continue;
                const doc = this.createDoc(key, data.n, data.fp);
                for (const s of data.s)
                    this.addStroke(doc, { c: s.c, k: s.k, p: s.p.slice() });
            }
        }
        this.layout();
    }

    queueLayout() {
        if (this.layoutQueued)
            return;
        this.layoutQueued = true;
        requestAnimationFrame(() => {
            this.layoutQueued = false;
            this.layout();
        });
    }

    layout() {
        const root = document.documentElement;
        this.overlay.style.width = `${ Math.max(root.scrollWidth, window.innerWidth) }px`;
        this.overlay.style.height = `${ Math.max(root.scrollHeight, window.innerHeight) }px`;

        for (const doc of this.docs.values()) {
            const view = this.viewFor(doc.key);
            const anchor = view ? this.anchorOf(view) : null;
            if ( ! anchor || doc.strokes.length === 0) {
                doc.g.style.display = 'none';
                this.setBadge(doc, null);
                continue;
            }
            doc.g.style.display = '';
            doc.g.setAttribute('transform', `translate(${ anchor.left } ${ anchor.top }) scale(${ anchor.sx } ${ anchor.sy })`);

            if (doc.fp !== anchor.fp && this.resized.has(doc.key) && doc.n === 1) {
                // the new image after a resize by hand
                this.resized.delete(doc.key);
                doc.fp = anchor.fp;
                this.save(doc);
            }
            doc.stale = doc.fp !== anchor.fp;
            doc.g.setAttribute('opacity', doc.stale ? String(STALE_OPACITY) : '1');
            this.setBadge(doc, doc.stale ? anchor : null);
        }
    }

    // --- elements ---

    viewFor(key: string): any | null {
        const root = this.host.results();
        if ( ! root)
            return null;
        if (key === '')
            return root;
        try {
            return root.get(unflatten(key)) ?? null;
        }
        catch (e) {
            return null;
        }
    }

    // the results element under a point: the deepest one
    viewAt(x: number, y: number): any | null {
        const stack = document.elementsFromPoint(x, y);
        for (const el of stack) {
            if (el === this.overlay || this.layer.contains(el))
                continue;
            for (let node: Element | null = el; node; node = node.parentElement) {
                const v = node as any;
                if (typeof v.address === 'function' && v.model && typeof v.type === 'function')
                    return v;
            }
        }
        return this.host.results();
    }

    anchorOf(view: any): Anchor | null {
        const isImage = view.type() === 'Image' && view.$image;
        const el: HTMLElement = isImage ? view.$image : view;
        if ( ! el.isConnected || el.getClientRects().length === 0)
            return null;
        const rect = el.getBoundingClientRect();
        const left = rect.left + window.scrollX;
        const top = rect.top + window.scrollY;
        const fp = cachedFingerprint(view.type(), view.model?.attributes?.element);
        if (isImage && rect.width > 0 && rect.height > 0)
            return { left, top, sx: rect.width, sy: rect.height, n: 1, fp };
        return { left, top, sx: 1, sy: 1, n: 0, fp };
    }

    createDoc(key: string, n: 0 | 1, fp: string): Doc {
        const g = document.createElementNS(SVG_NS, 'g');
        this.layer.append(g);
        const doc: Doc = { key, address: key === '' ? [] : unflatten(key), fp, n, strokes: [], g, badge: null, stale: false };
        this.docs.set(key, doc);
        return doc;
    }

    // --- drawing ---

    private width(e: PointerEvent): number {
        if (this.state.tool === 'highlighter')
            return HIGHLIGHTER_WIDTH;
        if (e.pointerType === 'pen' && e.pressure > 0)
            return PEN_WIDTH * (0.4 + 1.2 * e.pressure);
        return PEN_WIDTH;
    }

    private onPointerDown(e: PointerEvent) {
        if (this.pointerId !== null)
            return;
        this.pointerId = e.pointerId;
        this.overlay.setPointerCapture?.(e.pointerId);
        if (e.pointerType === 'touch') {
            this.touch = { x: e.screenX, y: e.screenY };
            return;
        }
        if (e.button !== 0) {
            this.pointerId = null;
            return;
        }
        e.preventDefault();

        if (this.state.tool === 'eraser') {
            this.erased = [];
            // where the ink is does not change while erasing
            this.eraseAnchors = new Map();
            for (const doc of this.docs.values()) {
                const view = doc.strokes.length > 0 ? this.viewFor(doc.key) : null;
                const anchor = view ? this.anchorOf(view) : null;
                if ( ! anchor)
                    continue;
                this.eraseAnchors.set(doc, anchor);
                for (const stroke of doc.strokes) {
                    const p = stroke.p;
                    let x0 = Infinity, y0 = Infinity, x1 = -Infinity, y1 = -Infinity, w = 0;
                    for (let i = 0; i < p.length; i += 3) {
                        const x = anchor.left + p[i] * anchor.sx;
                        const y = anchor.top + p[i + 1] * anchor.sy;
                        if (x < x0) x0 = x;
                        if (x > x1) x1 = x;
                        if (y < y0) y0 = y;
                        if (y > y1) y1 = y;
                        if (p[i + 2] > w) w = p[i + 2];
                    }
                    const pad = ERASER_RADIUS + w / 2;
                    this.eraseBoxes.set(stroke, [ x0 - pad, y0 - pad, x1 + pad, y1 + pad ]);
                }
            }
            this.eraseAt(e.pageX, e.pageY);
            return;
        }

        const view = this.viewAt(e.clientX, e.clientY);
        const anchor = view ? this.anchorOf(view) : null;
        if ( ! view || ! anchor) {
            this.pointerId = null;
            return;
        }
        const key = flatten(view.address());
        let doc = this.docs.get(key);
        if ( ! doc)
            doc = this.createDoc(key, anchor.n, anchor.fp);
        if (doc.strokes.length === 0 || doc.n !== anchor.n) {
            doc.n = anchor.n;
            doc.fp = anchor.fp;
        }
        // drawing on a marked element means its content is the one in view
        doc.fp = anchor.fp;
        doc.stale = false;
        doc.g.setAttribute('opacity', '1');
        this.setBadge(doc, null);
        doc.g.style.display = '';
        doc.g.setAttribute('transform', `translate(${ anchor.left } ${ anchor.top }) scale(${ anchor.sx } ${ anchor.sy })`);

        const kind: InkKind = this.state.tool === 'highlighter' ? 'h' : 'p';
        const colour = kind === 'h' ? this.state.highlighter : this.state.colour;
        const stroke = this.addStroke(doc, { c: colour, k: kind, p: [] });
        this.current = { doc, stroke, anchor, last: null };
        this.addPoint(e);
    }

    private onPointerMove(e: PointerEvent) {
        if (e.pointerId !== this.pointerId)
            return;
        if (this.touch) {
            this.host.post('jupwrInkScroll', { dx: this.touch.x - e.screenX, dy: this.touch.y - e.screenY });
            this.touch = { x: e.screenX, y: e.screenY };
            return;
        }
        const events = typeof e.getCoalescedEvents === 'function' ? e.getCoalescedEvents() : [];
        for (const ev of (events.length > 0 ? events : [ e ])) {
            if (this.state.tool === 'eraser')
                this.eraseAt(ev.pageX, ev.pageY);
            else
                this.addPoint(ev);
        }
    }

    private onPointerUp(e: PointerEvent) {
        if (e.pointerId !== this.pointerId)
            return;
        this.endStroke();
    }

    private endStroke() {
        if (this.current) {
            const { doc, stroke } = this.current;
            this.current = null;
            if (stroke.p.length === 0) {
                stroke.el.remove();
                doc.strokes.splice(doc.strokes.indexOf(stroke), 1);
            }
            else {
                this.record({ type: 'add', items: [ { doc, stroke } ] });
                this.save(doc);
            }
        }
        if (this.erased.length > 0) {
            // removed from view while erasing; saved once, now
            for (const doc of new Set(this.erased.map(item => item.doc)))
                this.save(doc);
            this.record({ type: 'remove', items: this.erased });
            this.erased = [];
            this.queueLayout();
        }
        this.eraseAnchors = null;
        this.eraseBoxes.clear();
        this.pointerId = null;
        this.touch = null;
    }

    private addPoint(e: PointerEvent) {
        const cur = this.current;
        if ( ! cur)
            return;
        const { anchor, stroke } = cur;
        const x = (e.pageX - anchor.left) / anchor.sx;
        const y = (e.pageY - anchor.top) / anchor.sy;
        const last = cur.last;
        if (last && Math.hypot((x - last.x) * anchor.sx, (y - last.y) * anchor.sy) < 1.5)
            return;
        const raw = this.width(e);
        const w = last ? last.w * 0.6 + raw * 0.4 : raw;
        stroke.p.push(x, y, w);
        this.appendSegment(stroke, last ?? { x, y, w }, { x, y, w });
        cur.last = { x, y, w };
    }

    private appendSegment(stroke: RStroke, a: { x: number, y: number, w: number }, b: { x: number, y: number, w: number }) {
        const line = document.createElementNS(SVG_NS, 'line');
        line.setAttribute('x1', String(a.x));
        line.setAttribute('y1', String(a.y));
        line.setAttribute('x2', String(b.x));
        line.setAttribute('y2', String(b.y));
        line.setAttribute('stroke-width', ((a.w + b.w) / 2).toFixed(2));
        // widths stay in pixels when a plot's ink is scaled to its size
        line.setAttribute('vector-effect', 'non-scaling-stroke');
        stroke.el.append(line);
    }

    addStroke(doc: Doc, data: InkStroke): RStroke {
        const el = document.createElementNS(SVG_NS, 'g');
        el.setAttribute('stroke', data.c);
        el.setAttribute('stroke-linecap', 'round');
        el.setAttribute('fill', 'none');
        if (data.k === 'h')
            el.setAttribute('opacity', String(HIGHLIGHTER_OPACITY));
        const stroke: RStroke = { ...data, el };
        const p = data.p;
        for (let i = 0; i < p.length; i += 3) {
            const b = { x: p[i], y: p[i + 1], w: p[i + 2] };
            const a = i === 0 ? b : { x: p[i - 3], y: p[i - 2], w: p[i - 1] };
            this.appendSegment(stroke, a, b);
        }
        doc.g.append(el);
        doc.strokes.push(stroke);
        return stroke;
    }

    private eraseAt(px: number, py: number) {
        const anchors = this.eraseAnchors;
        if ( ! anchors)
            return;
        const hits: { doc: Doc, stroke: RStroke }[] = [];
        for (const [ doc, anchor ] of anchors) {
            for (const stroke of doc.strokes) {
                const box = this.eraseBoxes.get(stroke);
                if (box && (px < box[0] || px > box[2] || py < box[1] || py > box[3]))
                    continue;
                const p = stroke.p;
                for (let i = 0; i < p.length; i += 3) {
                    const j = i + 3 < p.length ? i + 3 : i;
                    const d = distanceToSegment(px, py,
                        anchor.left + p[i] * anchor.sx, anchor.top + p[i + 1] * anchor.sy,
                        anchor.left + p[j] * anchor.sx, anchor.top + p[j + 1] * anchor.sy);
                    if (d <= ERASER_RADIUS + Math.max(p[i + 2], p[j + 2]) / 2) {
                        hits.push({ doc, stroke });
                        break;
                    }
                }
            }
        }
        if (hits.length === 0)
            return;
        for (const { doc, stroke } of hits) {
            stroke.el.remove();
            doc.strokes.splice(doc.strokes.indexOf(stroke), 1);
        }
        this.erased.push(...hits);
    }

    // --- changes ---

    private record(action: Action) {
        const id = newActionId();
        this.actions.set(id, action);
        this.host.post('jupwrInkAction', { id });
    }

    private attach(items: { doc: Doc, stroke: RStroke }[]) {
        const docs = new Set<Doc>();
        for (const { doc, stroke } of items) {
            doc.g.append(stroke.el);
            doc.strokes.push(stroke);
            docs.add(doc);
        }
        for (const doc of docs)
            this.save(doc);
        this.queueLayout();
    }

    private detach(items: { doc: Doc, stroke: RStroke }[]) {
        const docs = new Set<Doc>();
        for (const { doc, stroke } of items) {
            stroke.el.remove();
            const i = doc.strokes.indexOf(stroke);
            if (i !== -1)
                doc.strokes.splice(i, 1);
            docs.add(doc);
        }
        for (const doc of docs)
            this.save(doc);
        this.queueLayout();
    }

    save(doc: Doc) {
        const value = encodeInk({ fp: doc.fp, n: doc.n, s: doc.strokes.map(({ c, k, p }) => ({ c, k, p })) });
        this.host.setParam(doc.address, { [INK_OPTION]: value });
        // marks the file as changed, as editing a note does
        this.host.post('annotationChanged');
    }

    // --- the mark on ink drawn on an earlier version of the element ---

    private setBadge(doc: Doc, anchor: Anchor | null) {
        if ( ! anchor) {
            if (doc.badge)
                doc.badge.hidden = true;
            return;
        }
        if ( ! doc.badge) {
            const badge = document.createElement('div');
            badge.className = 'jupwr-ink-stale ignore-html';
            badge.setAttribute('role', 'note');
            const text = document.createElement('span');
            text.textContent = 'Pismo do wcześniejszej wersji wyniku';
            const keep = document.createElement('button');
            keep.type = 'button';
            keep.textContent = 'Zachowaj';
            keep.title = 'Pismo pasuje do obecnego wyniku';
            keep.addEventListener('click', () => {
                const view = this.viewFor(doc.key);
                const now = view ? this.anchorOf(view) : null;
                if (now) {
                    doc.fp = now.fp;
                    this.save(doc);
                    this.layout();
                }
            });
            const remove = document.createElement('button');
            remove.type = 'button';
            remove.textContent = 'Usuń';
            remove.title = 'Usuń pismo z tego elementu';
            remove.addEventListener('click', () => {
                const items = doc.strokes.map(stroke => ({ doc, stroke }));
                if (items.length === 0)
                    return;
                this.detach(items);
                this.record({ type: 'remove', items });
            });
            badge.append(text, keep, remove);
            document.body.append(badge);
            doc.badge = badge;
        }
        doc.badge.hidden = false;
        doc.badge.style.left = `${ anchor.left }px`;
        doc.badge.style.top = `${ Math.max(0, anchor.top - 2) }px`;
    }

    private onKeyDown(e: KeyboardEvent) {
        if ( ! this.state.active)
            return;
        if (e.key === 'Escape') {
            e.preventDefault();
            e.stopPropagation();
            this.host.post('jupwrInkKey', { key: 'escape' });
        }
        else if ((e.ctrlKey || e.metaKey) && ! e.shiftKey && ! e.altKey && e.code === 'KeyZ') {
            e.preventDefault();
            e.stopPropagation();
            this.host.post('jupwrInkKey', { key: 'undo' });
        }
    }
}
