'use strict';

// jUPWR: the document title in the tab row and the headers of the data and
// results cards. They exist in every interface style but are shown only by
// the styles other than classic (hidden in main/jupwr-ui-a.css), so the
// classic look stays that of jamovi.

import { h } from '../common/htmlelementcreator';
import ActionHub from './actionhub';
import { findDataset } from './datasetdocs';
import type Instance from './instance';
import type { DataSetModel } from './dataset';

interface CardHeaderOptions {
    ribbonHeader: HTMLElement;
    mainTable: HTMLElement;
    results: HTMLElement;
    copyResults: () => void;
    exportResults: () => void;
    toggleEditResults: () => void;
}

// Polish plural of "wiersz" (1 wiersz, 2-4 wiersze, 5+ / 12-14 wierszy)
function rowsLabel(n: number): string {
    if (n === 1)
        return '1 wiersz';
    const lastDigit = n % 10;
    const lastTwo = n % 100;
    if (lastDigit >= 2 && lastDigit <= 4 && (lastTwo < 12 || lastTwo > 14))
        return `${ n } wiersze`;
    return `${ n } wierszy`;
}

export function setupCardHeaders(instance: Instance, dataSetModel: DataSetModel, options: CardHeaderOptions) {

    const $docTitle = h('div', { class: 'jupwr-doc-title app-dragable', role: 'none' });
    options.ribbonHeader.querySelector('.ribbon-tabs').after($docTitle);

    const $dataInfo = h('span', { class: 'jupwr-card-subtitle' });
    const $about = h('button', { class: 'jupwr-card-button', type: 'button' }, 'O zbiorze');
    $about.addEventListener('click', () => ActionHub.get('datasetInfo').do());
    const $dataHeader = h('div', { class: 'jupwr-card-header', role: 'none' },
        h('span', { class: 'jupwr-card-title' }, _('Data')),
        $dataInfo,
        h('span', { class: 'jupwr-card-spacer' }),
        $about);
    options.mainTable.prepend($dataHeader);

    // students did not find that the results take text: a visible way into
    // the edit mode (the "Edytuj" tab), marked while it is on
    const $write = h('button', { class: 'jupwr-card-button jupwr-write-button', type: 'button' }, '✎ Pisz w wynikach');
    $write.addEventListener('click', () => options.toggleEditResults());
    const updateWrite = () => $write.classList.toggle('active', !! instance.get('editState'));
    instance.on('change:editState', updateWrite);
    updateWrite();

    const $copy = h('button', { class: 'jupwr-card-button', type: 'button' }, _('Copy'));
    $copy.addEventListener('click', () => options.copyResults());
    const $export = h('button', { class: 'jupwr-card-button', type: 'button' }, _('Export'));
    $export.addEventListener('click', () => options.exportResults());
    const $resultsHeader = h('div', { class: 'jupwr-card-header', role: 'none' },
        h('span', { class: 'jupwr-card-title' }, _('Results')),
        h('span', { class: 'jupwr-card-spacer' }),
        $write,
        $copy,
        $export);
    options.results.prepend($resultsHeader);

    let datasetName = '';

    const update = () => {
        const title = datasetName || instance.attributes.title || '';
        const rows = dataSetModel.attributes.rowCount;
        const filters = dataSetModel.filterCount(false);
        const columns = dataSetModel.attributes.columnCount - filters;
        $docTitle.textContent = title ? `${ title } · ${ rowsLabel(rows) }` : '';
        $dataInfo.textContent = `${ title ? title + ' · ' : '' }${ rows } × ${ columns } · filtry ${ dataSetModel.filterCount(true) }`;
    };

    const updateDataset = async () => {
        const found = await findDataset(instance);
        datasetName = found !== null ? found.dataset.name : '';
        update();
    };

    const aboutAction = ActionHub.get('datasetInfo');
    const updateAbout = () => { $about.hidden = ! aboutAction.get('enabled'); };
    aboutAction.on('change:enabled', updateAbout);
    updateAbout();

    instance.on('change:title change:path', updateDataset);
    dataSetModel.on('dataSetLoaded columnsChanged columnsInserted columnsDeleted rowsInserted rowsDeleted', update);
    dataSetModel.on('change:rowCount change:columnCount', update);
    updateDataset();
}

// The variable editor (also filters, computed and transformed variables)
// is a strip above the whole work area in the classic style; the other
// styles move it into the data card, under its header, so the results stay
// in view. The spreadsheet then needs telling that its height changed.
export function setupEditorPlacement(editor: HTMLElement, mainView: HTMLElement, mainTable: HTMLElement, spreadsheet: HTMLElement, dataSetModel: DataSetModel) {

    const place = () => {
        const inCard = document.documentElement.getAttribute('data-ui-style') !== 'classic';
        if (inCard && editor.parentElement !== mainTable)
            mainTable.querySelector('.jupwr-card-header').after(editor);
        else if ( ! inCard && editor.parentElement !== mainView)
            mainView.prepend(editor);
        spreadsheet.dispatchEvent(new CustomEvent('resized'));
    };

    new MutationObserver(place).observe(document.documentElement, { attributes: true, attributeFilter: [ 'data-ui-style' ] });
    place();

    dataSetModel.on('change:editingVar', () => {
        if (editor.parentElement === mainTable)
            setTimeout(() => spreadsheet.dispatchEvent(new CustomEvent('resized')), 250);
    });
}
