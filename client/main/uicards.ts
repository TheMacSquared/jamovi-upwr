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
import type { InkBoard } from './jupwrink';

interface CardHeaderOptions {
    ribbonHeader: HTMLElement;
    mainTable: HTMLElement;
    results: HTMLElement;
    copyResults: () => void;
    exportResults: () => void;
    toggleEditResults: () => void;
    ink: InkBoard;
}

export function setupCardHeaders(instance: Instance, dataSetModel: DataSetModel, options: CardHeaderOptions) {

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

    // handwriting saved with the results (jupwrink.ts); in the classic style only its
    // shortcut, Ctrl+Shift+P, since the card headers are hidden there
    const $draw = h('button', { class: 'jupwr-card-button jupwr-draw-button', type: 'button', title: 'Pisz odręcznie po wynikach, pismo zapisuje się w pliku (Ctrl+Shift+P)' }, 'Rysuj');
    $draw.addEventListener('click', () => options.ink.toggle());
    options.ink.onActiveChanged(active => $draw.classList.toggle('active', active));

    const $copy = h('button', { class: 'jupwr-card-button', type: 'button' }, _('Copy'));
    $copy.addEventListener('click', () => options.copyResults());
    const $export = h('button', { class: 'jupwr-card-button', type: 'button' }, _('Export'));
    $export.addEventListener('click', () => options.exportResults());
    const $resultsHeader = h('div', { class: 'jupwr-card-header', role: 'none' },
        h('span', { class: 'jupwr-card-title' }, _('Results')),
        h('span', { class: 'jupwr-card-spacer' }),
        $write,
        $draw,
        $copy,
        $export);
    options.results.prepend($resultsHeader);

    let datasetName = '';

    const update = () => {
        const title = datasetName || instance.attributes.title || '';
        const rows = dataSetModel.attributes.rowCount;
        const filters = dataSetModel.filterCount(false);
        const columns = dataSetModel.attributes.columnCount - filters;
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
