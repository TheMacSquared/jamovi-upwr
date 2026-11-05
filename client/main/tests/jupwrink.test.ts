// @vitest-environment jsdom

import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { InkBoard, INK_COLOURS } from '../jupwrink';

let results: HTMLElement;
let panel: HTMLElement;
let board: InkBoard;
let frames: { postMessage: ReturnType<typeof vi.fn> }[];

function fromFrame(frame: any, type: string, data?: any) {
    window.dispatchEvent(new MessageEvent('message', { data: { type, data }, source: frame }));
}

const sent = (frame: any, type: string) => frame.postMessage.mock.calls.filter((c: any[]) => c[0].type === type).map((c: any[]) => c[0].data);

beforeEach(() => {
    vi.useFakeTimers();
    vi.stubGlobal('ResizeObserver', class { observe() {} unobserve() {} disconnect() {} });
    results = document.createElement('div');
    panel = document.createElement('div');
    results.append(panel);
    document.body.append(results);
    board = new InkBoard(results, panel);
    frames = [ { postMessage: vi.fn() }, { postMessage: vi.fn() } ];
    vi.spyOn(board, 'frames').mockImplementation(() => frames as any);
});

afterEach(() => {
    board.destroy();
    document.body.replaceChildren();
    vi.useRealTimers();
    vi.unstubAllGlobals();
});

describe('InkBoard (main window)', () => {

    it('switches the pen on and off and tells every results frame', () => {
        const states: boolean[] = [];
        board.onActiveChanged(a => states.push(a));
        board.toggle();
        expect(board.toolbar.hidden).toBe(false);
        expect(results.classList.contains('jupwr-ink-active')).toBe(true);
        for (const f of frames)
            expect(sent(f, 'jupwrInkState').pop()).toMatchObject({ active: true, tool: 'pen', colour: INK_COLOURS[0].value });
        board.toggle();
        expect(states).toEqual([ true, false ]);
        expect(sent(frames[0], 'jupwrInkState').pop()).toMatchObject({ active: false });
    });

    it('sends the chosen tool to the frames and marks it in the toolbar', () => {
        board.setTool('pen', INK_COLOURS[1].value);
        expect(sent(frames[1], 'jupwrInkState').pop()).toMatchObject({ tool: 'pen', colour: INK_COLOURS[1].value });
        const pressed = board.toolbar.querySelectorAll('button[aria-pressed="true"]');
        expect(pressed).toHaveLength(1);
        expect(pressed[0].getAttribute('aria-label')).toContain(INK_COLOURS[1].name);
        board.setTool('eraser');
        expect(sent(frames[1], 'jupwrInkState').pop()).toMatchObject({ tool: 'eraser' });
    });

    it('answers a new frame with the current state', () => {
        board.setActive(true);
        frames[1].postMessage.mockClear();
        fromFrame(frames[1], 'jupwrInkHello');
        expect(sent(frames[1], 'jupwrInkState')).toEqual([ expect.objectContaining({ active: true }) ]);
    });

    it('ignores messages from anything but its frames', () => {
        const stranger = { postMessage: vi.fn() };
        fromFrame(stranger, 'jupwrInkHello');
        fromFrame(stranger, 'jupwrInkAction', { id: 'x' });
        expect(stranger.postMessage).not.toHaveBeenCalled();
        expect(board.history).toHaveLength(0);
    });

    it('undoes across the analyses, last change first', () => {
        fromFrame(frames[0], 'jupwrInkAction', { id: 'a' });
        fromFrame(frames[1], 'jupwrInkAction', { id: 'b' });
        board.undo();
        expect(sent(frames[1], 'jupwrInkUndo')).toEqual([ { id: 'b' } ]);
        expect(sent(frames[0], 'jupwrInkUndo')).toEqual([]);
        board.undo();
        expect(sent(frames[0], 'jupwrInkUndo')).toEqual([ { id: 'a' } ]);
    });

    it('clears all the analyses only on a second click, as one undoable step', () => {
        board.clear();
        expect(sent(frames[0], 'jupwrInkClear')).toEqual([]);
        expect(board.toolbar.querySelector('.jupwr-ink-clear')!.textContent).toBe('Na pewno?');
        board.clear();
        const [ cleared ] = sent(frames[0], 'jupwrInkClear');
        expect(sent(frames[1], 'jupwrInkClear')).toEqual([ cleared ]);
        board.undo();
        expect(sent(frames[0], 'jupwrInkUndo')).toEqual([ cleared ]);
        expect(sent(frames[1], 'jupwrInkUndo')).toEqual([ cleared ]);
    });

    it('the second click must come soon', () => {
        board.clear();
        vi.advanceTimersByTime(3500);
        expect(board.toolbar.querySelector('.jupwr-ink-clear')!.textContent).toBe('Wyczyść');
        board.clear();
        expect(sent(frames[0], 'jupwrInkClear')).toEqual([]);
    });

    it('scrolls the panel and takes keys sent by a frame', () => {
        panel.scrollBy = vi.fn() as any;
        fromFrame(frames[0], 'jupwrInkScroll', { dx: 0, dy: 40 });
        expect(panel.scrollBy).toHaveBeenCalledWith(0, 40);
        board.setActive(true);
        fromFrame(frames[0], 'jupwrInkAction', { id: 'a' });
        fromFrame(frames[0], 'jupwrInkKey', { key: 'undo' });
        expect(sent(frames[0], 'jupwrInkUndo')).toEqual([ { id: 'a' } ]);
        fromFrame(frames[0], 'jupwrInkKey', { key: 'escape' });
        expect(board.active).toBe(false);
    });

    it('Escape and Ctrl+Z in the main window while the pen is on', () => {
        board.setActive(true);
        fromFrame(frames[0], 'jupwrInkAction', { id: 'a' });
        window.dispatchEvent(new KeyboardEvent('keydown', { key: 'z', code: 'KeyZ', ctrlKey: true, cancelable: true }));
        expect(sent(frames[0], 'jupwrInkUndo')).toEqual([ { id: 'a' } ]);
        window.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', cancelable: true }));
        expect(board.active).toBe(false);
        const undo = new KeyboardEvent('keydown', { key: 'z', code: 'KeyZ', ctrlKey: true, cancelable: true });
        window.dispatchEvent(undo);
        expect(undo.defaultPrevented).toBe(false);
    });
});
