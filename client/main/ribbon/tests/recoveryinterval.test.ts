// @vitest-environment jsdom

import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import type { RibbonModel } from '../../ribbon';

vi.mock('../../host', () => ({ default: { on: vi.fn(), version: Promise.resolve('28.17') } }));
vi.mock('../../../common/i18n', () => ({ default: {
    get: () => ({ availableLanguages: () => [], getDisplayName: (code: string) => code }),
} }));
vi.mock('../../../common/interactionmanager', () => ({ default: {
    nextAriaId: vi.fn(() => `label-${Math.random()}`),
    registerLoop: () => ({ on: vi.fn(), activate: vi.fn(), deactivate: vi.fn() }),
} }));
vi.mock('../../settings', () => ({
    Theme: { JUPWR_JASNY: 'jupwrJasny' }, defaultPaletteFor: () => 'jupwrJasny',
}));

import AppMenuButton from '../appmenu';

beforeEach(() => {
    (globalThis as any)._ = (text: string, values?: Record<string, string>) =>
        text.replace('{n}', values?.n ?? '');
});
afterEach(() => document.body.replaceChildren());

function menu(initial?: number) {
    const values = new Map<string, unknown>();
    if (initial !== undefined)
        values.set('recoveryInterval', initial);
    const listeners = new Map<string, () => void>();
    const settings = {
        getSetting: (name: string, fallback: unknown) => values.has(name) ? values.get(name) : fallback,
        setSetting: vi.fn((name: string, value: unknown) => {
            values.set(name, value);
            listeners.get(`change:${name}`)?.();
        }),
        on: (event: string, callback: () => void) => listeners.set(event, callback),
    };
    const element = new AppMenuButton({ settings: () => settings } as unknown as RibbonModel);
    document.body.append(element);
    return { element, settings };
}

describe('recovery interval preferences', () => {
    it('defaults to five minutes and offers selectable intervals', () => {
        const { element } = menu();
        expect(element.$recoveryIntervalList.value).toBe('300');
        expect([...element.$recoveryIntervalList.options].map(option => option.value))
            .toEqual(['0', '60', '120', '300', '600', '900']);
    });

    it.each(['0', '60', '900'])('saves numeric preference %s when changed', value => {
        const { element, settings } = menu();
        element.$recoveryIntervalList.value = value;
        element.$recoveryIntervalList.dispatchEvent(new Event('change'));
        expect(settings.setSetting).toHaveBeenCalledWith('recoveryInterval', Number(value));
    });

    it('displays saved settings and synchronizes updates from another window', () => {
        const { element, settings } = menu(120);
        expect(element.$recoveryIntervalList.value).toBe('120');
        settings.setSetting('recoveryInterval', 600);
        expect(element.$recoveryIntervalList.value).toBe('600');
    });

    it('keeps a custom administrator interval visible without overriding it', () => {
        const { element, settings } = menu(45);
        expect(element.$recoveryIntervalList.value).toBe('45');
        expect(element.$recoveryIntervalList.selectedOptions[0].textContent).toBe('45 s');
        settings.setSetting('recoveryInterval', 60);
        expect(element.$recoveryIntervalList.value).toBe('60');
        expect(element.$recoveryIntervalList.querySelector('[data-custom-interval]')).toBeNull();
    });
});
