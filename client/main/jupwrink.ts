'use strict';

// jUPWR: handwriting on the results, saved with them -- the main window side.
//
// The strokes are drawn and stored by each results frame
// (resultsview/jupwrink.ts), anchored to the results element they start on
// and saved as that element's 'ink' results option, so they are in the .omv.
// This side owns what is shared by all the frames: the pen on/off, the tool,
// the toolbar, and the undo history across the analyses.

export type InkTool = 'pen' | 'highlighter' | 'eraser';

export const INK_COLOURS = [
    { name: 'czerwony', value: '#d62728' },
    { name: 'niebieski', value: '#1f5fbf' },
    { name: 'czarny', value: '#222222' },
];

const HIGHLIGHTER_COLOUR = '#ffd400';
const CONFIRM_MS = 3000;

type HistoryEntry = { id: string, sources: Window[] };

export class InkBoard {

    readonly results: HTMLElement;
    readonly panel: HTMLElement;
    readonly toolbar: HTMLElement;

    active = false;
    tool: InkTool = 'pen';
    colour = INK_COLOURS[0].value;

    history: HistoryEntry[] = [];

    private listeners: Array<(active: boolean) => void> = [];
    private toolButtons: Map<string, HTMLButtonElement> = new Map();
    private clearButton: HTMLButtonElement | null = null;
    private clearArmed: ReturnType<typeof setTimeout> | null = null;
    private clearCount = 0;
    private resizeObserver: ResizeObserver | null = null;

    constructor(results: HTMLElement, panel: HTMLElement) {
        this.results = results;
        this.panel = panel;

        this.toolbar = this.buildToolbar();
        this.toolbar.hidden = true;
        this.results.append(this.toolbar);

        this.onKeyDown = this.onKeyDown.bind(this);
        this.onMessage = this.onMessage.bind(this);
        this.place = this.place.bind(this);
        window.addEventListener('message', this.onMessage);
        if (typeof ResizeObserver !== 'undefined') {
            this.resizeObserver = new ResizeObserver(this.place);
            this.resizeObserver.observe(this.panel);
        }
    }

    destroy() {
        this.setActive(false);
        window.removeEventListener('message', this.onMessage);
        this.resizeObserver?.disconnect();
        this.toolbar.remove();
    }

    onActiveChanged(listener: (active: boolean) => void) {
        this.listeners.push(listener);
    }

    toggle() {
        this.setActive( ! this.active);
    }

    setActive(active: boolean) {
        if (active === this.active)
            return;
        this.active = active;
        this.toolbar.hidden = ! active;
        this.results.classList.toggle('jupwr-ink-active', active);
        if (active) {
            this.place();
            window.addEventListener('keydown', this.onKeyDown, true);
        }
        else {
            window.removeEventListener('keydown', this.onKeyDown, true);
            this.disarmClear();
        }
        this.broadcastState();
        for (const listener of this.listeners)
            listener(active);
    }

    setTool(tool: InkTool, colour?: string) {
        this.tool = tool;
        if (colour)
            this.colour = colour;
        for (const [ key, button ] of this.toolButtons) {
            const on = key === (tool === 'pen' ? `pen:${ this.colour }` : tool);
            button.classList.toggle('active', on);
            button.setAttribute('aria-pressed', on ? 'true' : 'false');
        }
        this.broadcastState();
    }

    undo() {
        const entry = this.history.pop();
        if ( ! entry)
            return;
        for (const source of entry.sources)
            this.post(source, 'jupwrInkUndo', { id: entry.id });
    }

    // in every analysis: asks for a second click, and can be undone
    clear() {
        if (this.clearArmed === null) {
            this.clearArmed = setTimeout(() => this.disarmClear(), CONFIRM_MS);
            if (this.clearButton) {
                this.clearButton.textContent = 'Na pewno?';
                this.clearButton.classList.add('armed');
            }
            return;
        }
        this.disarmClear();
        const id = `clear-${ Date.now().toString(36) }-${ ++this.clearCount }`;
        const frames = this.frames();
        for (const frame of frames)
            this.post(frame, 'jupwrInkClear', { id });
        this.history.push({ id, sources: frames });
    }

    place() {
        this.toolbar.style.top = `${ this.panel.offsetTop + 8 }px`;
        this.toolbar.style.right = `${ this.results.clientWidth - this.panel.offsetLeft - this.panel.clientWidth + 8 }px`;
    }

    // --- the frames ---

    frames(): Window[] {
        return Array.from(this.panel.querySelectorAll('iframe'))
            .map(iframe => iframe.contentWindow)
            .filter((w): w is Window => w !== null);
    }

    state() {
        return { active: this.active, tool: this.tool, colour: this.colour, highlighter: HIGHLIGHTER_COLOUR };
    }

    private post(target: Window, type: string, data: any) {
        target.postMessage({ type, data }, '*');
    }

    private broadcastState() {
        const state = this.state();
        for (const frame of this.frames())
            this.post(frame, 'jupwrInkState', state);
    }

    private onMessage(event: MessageEvent) {
        const message = event.data;
        if ( ! message || typeof message.type !== 'string' || ! message.type.startsWith('jupwrInk'))
            return;
        const source = event.source as Window | null;
        if ( ! source || ! this.frames().includes(source))
            return;
        const data = message.data || { };
        switch (message.type) {
            case 'jupwrInkHello':
                this.post(source, 'jupwrInkState', this.state());
                break;
            case 'jupwrInkAction':
                if (typeof data.id === 'string')
                    this.history.push({ id: data.id, sources: [ source ] });
                break;
            case 'jupwrInkScroll':
                this.panel.scrollBy(Number(data.dx) || 0, Number(data.dy) || 0);
                break;
            case 'jupwrInkKey':
                if (data.key === 'escape')
                    this.setActive(false);
                else if (data.key === 'undo')
                    this.undo();
                break;
        }
    }

    private onKeyDown(event: KeyboardEvent) {
        if (event.key === 'Escape') {
            event.preventDefault();
            event.stopPropagation();
            this.setActive(false);
        }
        else if ((event.ctrlKey || event.metaKey) && ! event.shiftKey && ! event.altKey && event.code === 'KeyZ') {
            event.preventDefault();
            event.stopPropagation();
            this.undo();
        }
    }

    private disarmClear() {
        if (this.clearArmed !== null)
            clearTimeout(this.clearArmed);
        this.clearArmed = null;
        if (this.clearButton) {
            this.clearButton.textContent = 'Wyczyść';
            this.clearButton.classList.remove('armed');
        }
    }

    private buildToolbar(): HTMLElement {
        const bar = document.createElement('div');
        bar.className = 'jupwr-ink-toolbar';
        bar.setAttribute('role', 'toolbar');
        bar.setAttribute('aria-label', 'Pisanie po wynikach');

        const button = (key: string | null, title: string, content: string | HTMLElement, action: () => void) => {
            const b = document.createElement('button');
            b.type = 'button';
            b.title = title;
            b.setAttribute('aria-label', title);
            if (typeof content === 'string')
                b.textContent = content;
            else
                b.append(content);
            b.addEventListener('click', action);
            if (key !== null)
                this.toolButtons.set(key, b);
            bar.append(b);
            return b;
        };
        const swatch = (colour: string, wide = false) => {
            const s = document.createElement('span');
            s.className = wide ? 'jupwr-ink-swatch wide' : 'jupwr-ink-swatch';
            s.style.backgroundColor = colour;
            return s;
        };
        const separator = () => {
            const s = document.createElement('span');
            s.className = 'jupwr-ink-separator';
            bar.append(s);
        };

        for (const colour of INK_COLOURS)
            button(`pen:${ colour.value }`, `Pisak: ${ colour.name }`, swatch(colour.value), () => this.setTool('pen', colour.value));
        button('highlighter', 'Zakreślacz', swatch(HIGHLIGHTER_COLOUR, true), () => this.setTool('highlighter'));
        button('eraser', 'Gumka (usuwa całą kreskę)', '⌫', () => this.setTool('eraser'));
        separator();
        button(null, 'Cofnij (Ctrl+Z)', '↶', () => this.undo());
        this.clearButton = button(null, 'Wyczyść pismo we wszystkich analizach', 'Wyczyść', () => this.clear());
        this.clearButton.classList.add('jupwr-ink-clear');
        separator();
        button(null, 'Zakończ pisanie (Esc)', '✕', () => this.setActive(false));

        this.setTool(this.tool, this.colour);
        return bar;
    }
}
