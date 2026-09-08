// @vitest-environment jsdom
import { beforeAll, expect, it, vi } from 'vitest';
vi.mock('../create', () => ({ AnalysisStatus: { ANALYSIS_COMPLETE: 3 } }));
import { Model, View } from '../table';
import RefTable from '../refs';

beforeAll(() => { globalThis._ = (text: string) => text; });

it('renders and clears notes through populated, empty and restored tables', () => {
    const model = new Model();
    model.attributes.refTable = new RefTable();
    model.attributes.refs = [];
    const view = new View(model, { fmt: {}, update: () => true, mode: 'rich' });
    const column = { name: 'x', title: 'X', type: 'text', format: '', visible: 0,
        cells: [], sortable: false, hasSortKeys: false, superTitle: '', combineBelow: false };
    const render = (values: string[], note?: string, columns = true) => {
        column.cells = values.map(s => ({ cellType: 's', s, format: 0, footnotes: [], symbols: [] }));
        model.attributes.element.columns = columns ? [column] : [];
        model.attributes.element.notes = note ? [{ key: 'error', note, init: false }] : [];
        model.initialize();
        view.render();
    };
    render(['wynik'], 'poprawny');
    expect(view.$tableBody.textContent).toContain('wynik');
    render([], 'Nie można obliczyć: 3 grupy.');
    expect(view.$tableBody.textContent).not.toContain('wynik');
    expect(view.$tableFooter.textContent).toContain('3 grupy');
    render([]);
    expect(view.$tableFooter.textContent).toBe('');
    render([], 'Brak kategorii', false);
    expect(view.$tableFooter.textContent).toContain('Brak kategorii');
    expect(view.$tableFooter.querySelector('td')?.colSpan).toBe(1);
    render(['nowy wynik']);
    expect(view.$tableBody.textContent).toContain('nowy wynik');
    expect(view.$tableFooter.textContent).toBe('');
});
