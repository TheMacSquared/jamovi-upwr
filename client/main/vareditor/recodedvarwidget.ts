
'use strict';

import dropdown from './dropdown';
import TransformList from './transformlist';
import VariableList from './variablelist';
import MeasureList from './measurelist';
import ColourPalette from '../editors/colourpalette';
import Notify from '../notification';
import VariableModel from './variablemodel';
import ConditionsBuilder from './conditionsbuilder';
import { h }  from '../../common/htmlelementcreator';
import { Column, ColumnType, MeasureType, Transform } from '../dataset';
import interactionManager from '../../common/interactionmanager';

let instanceID = 0;

// jUPWR: older files can have several columns on one transform (picked from
// the old transform list). A transform is shared when a column outside the
// ones being edited uses it -- writing to it would change those columns too.
export function isTransformShared(columns: Pick<Column, 'id' | 'transform'>[],
        transformId: number, editedIds: number[]): boolean {
    return columns.some((column) => column.transform === transformId
        && ! editedIds.includes(column.id));
}

class RecodedVarWidget extends HTMLElement {

    model: VariableModel;
    attached: boolean = false;
    _editNote: Notify = new Notify({ duration: 3000 });

    $variableIcon: HTMLElement;
    $variableList: HTMLSelectElement;
    $transformIcon: HTMLElement;
    $transformList: HTMLSelectElement;
    $editTransform: HTMLButtonElement;
    $errorMessage: HTMLElement;
    $measureList: HTMLSelectElement;
    $measureIcon: HTMLElement;

    variableList: VariableList;
    transformList: TransformList;
    measureList: MeasureList;
    builder: ConditionsBuilder;

    // guards the builder's onChange while we are the ones writing the formula
    _applying: boolean = false;

    constructor(model: VariableModel) {
        super();

        this.model = model;

        instanceID += 1;
        
        dropdown.init();

        this.classList.add('jmv-variable-recoded-widget', 'RecodedVarWidget');
        let $top = h('div', { class: 'jmv-variable-recoded-top' });
        this.append($top);

        // The source variable is kept out of sight too. It never reaches the
        // result -- RECODE only falls back to it when there is no else branch,
        // and the builder always writes one -- but the server blanks the
        // formula when parentId is 0 (column.py, ColumnType.RECODED), so one
        // still has to be set. _ensureParent picks it.
        let id1 = `transform-var-list-${instanceID}`;
        this.$variableIcon = h('div', { class: 'variable-type-icon single-variable-support' });
        this.$variableList = h('select', { id: id1, class: 'recoded-from single-variable-support' });

        // the transform picker stays in the DOM but out of sight: the rules now
        // belong to the variable, so there is nothing to pick. The list widgets
        // are still wired up below, which keeps the (untouched) transform editor
        // and its list working for anyone who opens them.
        let id2 = `transform-list-${instanceID}`;
        this.$transformIcon = h('div', { class: 'transform-icon' });
        this.$transformList = h('select', { id: id2 },
            h('option', { value: 'None' }, _('None')));
        this.$editTransform = h('button', { class: 'edit-button' }, _('Edit...'));

        let id3 = `recoded-measure-type-${instanceID}`;
        $top.append(h('label', { for: id3, class: 'measure-label' }, _('Measure type')));
        this.$measureIcon = h('div', { class: 'recoded-measure-icon' });
        $top.append(this.$measureIcon);
        // jUPWR: no 'Auto' -- the type is always a visible, concrete choice
        this.$measureList = h('select', { id: id3 },
            h('option', { value: 'nominal' }, _('Nominal')),
            h('option', { value: 'ordinal' }, _('Ordinal')),
            h('option', { value: 'continuous' }, _('Continuous')));
        $top.append(this.$measureList);
        this.$measureList.value = 'nominal';

        this.$errorMessage = h('div', { class: 'error-msg' }, _('This transform is in error and should be edited.'));
        $top.append(this.$errorMessage);

        this.builder = new ConditionsBuilder(
            () => this.model.dataset.get('columns') || [],
            () => this.model.get('name'),
            (formula) => this._applyFormula(formula));
        // the builder is always on here, so its add-rule button lives in the
        // top bar rather than being toggled with a mode
        $top.append(this.builder.$addRow);
        this.append(this.builder);

        this._updateChannelList();
        this._setupMeasureList();

        this.variableList = new VariableList();
        this.$variableList.setAttribute('aria-owns', this.variableList.id);
        this.$variableList.addEventListener('mousedown', (event) => {
            if (dropdown.isVisible() === true && dropdown.focusedOn() === this.$variableList)
                dropdown.hide();
            else
            {
                this.variableList.setParent(this.$variableList);
                dropdown.show(this.$variableList, this.variableList);
            }
            event.preventDefault();
            event.stopPropagation();
            this.$variableList.focus();
        });

        this.$variableList.addEventListener('change', event => {
            this.model.set('parentId', parseInt(this.$variableList.value));
        });

        this.$variableList.addEventListener('keydown', event => {
            if (event.key === 'Enter' || event.key === ' ') {
                // This dropdown is opened/closed by hand rather than through
                // a focus-loop exit key, so it never moves DOM focus off
                // $variableList (it already has it, from the mouse click
                // that opened it) and the interactionManager never sees a
                // focus transition to react to. Without this, a key handled
                // here leaves focus mode stuck on whatever it was before, so
                // no focus ring ever appears.
                interactionManager.setMode('keyboard', { noTransfer: true, silent: false });

                if (dropdown.isVisible() === true && dropdown.focusedOn() === this.$variableList)
                    dropdown.hide();
                else
                {
                    this.variableList.setParent(this.$variableList);
                    dropdown.show(this.$variableList, this.variableList);
                }
                event.stopPropagation();
                event.preventDefault();
                this.$variableList.focus();
            }
            else if (event.key === 'Escape') {
                event.preventDefault();
                event.stopPropagation();

                const wasOpen = dropdown.isVisible() === true && dropdown.focusedOn() === this.$variableList;
                if (wasOpen)
                    dropdown.hide();

                if (interactionManager.getMode() === 'keyboard') {
                    // Already visibly keyboard-driven: while the dropdown is
                    // open, Escape only closes it - closing is the whole
                    // action. Only once it's already closed does a further
                    // Escape cancel all the way out to the spreadsheet, same
                    // as dismissing any other transient control. Setting the
                    // mode itself moves DOM focus to the default focus
                    // control, so that case must not be followed by
                    // refocusing $variableList.
                    if (!wasOpen)
                        interactionManager.setMode('default');
                }
                else {
                    // Opened via mouse, so mode never became visible in the
                    // first place - make the now-focused control visible
                    // rather than jumping straight past it to the
                    // spreadsheet.
                    interactionManager.setMode('keyboard', { noTransfer: true, silent: false });
                    this.$variableList.focus();
                }
            }
        });

        this.variableList.addEventListener('selected-variable', (event: CustomEvent<Column>) => {
            let variable = event.detail;
            this.model.set('parentId', variable.id);
            dropdown.hide();
        });

        this.transformList = new TransformList();
        this.$transformList.setAttribute('aria-owns', this.transformList.id);
        this.$transformList.addEventListener('mousedown', (event) => {
            if (dropdown.isVisible() === true && dropdown.focusedOn() === this.$transformList)
                dropdown.hide();
            else
                dropdown.show(this.$transformList, this.transformList);
            event.preventDefault();
            event.stopPropagation();
            this.$transformList.focus();
        });

        this.$transformList.addEventListener('keydown', event => {
            if (event.key === 'Enter' || event.key === ' ') {
                if (dropdown.isVisible() === true && dropdown.focusedOn() === this.$transformList)
                    dropdown.hide();
                else
                    dropdown.show(this.$transformList, this.transformList);
                event.stopPropagation();
                event.preventDefault();
                this.$transformList.focus();
            }
        });


        this.transformList.addEventListener('selected-transform', (event: CustomEvent<Transform>) => {
            let transform = event.detail;
            this.model.set('transform', transform.id);
            this._updateTransformColour();
            dropdown.hide();
        });

        this.transformList.addEventListener('edit-transform', (event: CustomEvent<Transform>) => {
            let transform = event.detail;
            this.dispatchEvent(new CustomEvent('edit:transform', { detail: transform.id, bubbles: true }));
            dropdown.hide();
        });

        this.transformList.addEventListener('duplicate-transform', (event: CustomEvent<Transform>) => {
            let transform = event.detail;
            let copy = {
                name: transform.name,
                description: transform.description,
                suffix: transform.suffix,
                formula: transform.formula,
                measureType: transform.measureType
            };
            this._createTransform(copy);
        });

        this.transformList.addEventListener('remove-transform', (event: CustomEvent<Transform>) => {
            let transform = event.detail;
            let dataset = this.model.dataset;
            dataset.removeTransforms([transform.id]).catch((error) => {
                this._notifyEditProblem({
                    title: error.message,
                    message: error.cause,
                    type: 'error',
                });
            });
        });

        this.transformList.addEventListener('create-transform', (event) => {
            this._createTransform();
        });

        this.model.on('change:transform', event => {
            if (this.attached === false)
                return;

            this.$errorMessage.classList.remove('show');
            let transformId = this.model.get('transform');
            if (transformId === null) {
                this.$transformList.value = '';
                this.$editTransform.classList.add('disabled');
            }
            else if (transformId === 0) {
                this.$transformList.value = 'None';
                this.$editTransform.classList.add('disabled');
            }
            else {
                let transform = this.model.dataset.getTransformById(transformId);
                if (transform ===undefined)
                {
                    this.$transformList.value = 'None';
                    this.$editTransform.classList.add('disabled');
                }
                else {
                    this.$transformList.value = transform.name;
                    this.$editTransform.classList.remove('disabled');
                    for (let msg of transform.formulaMessage) {
                        if (msg !== '') {
                            this.$errorMessage.classList.add('show');
                            break;
                        }
                    }
                }
            }
            this._updateTransformColour();
            this._loadFromTransform();
        });

        // the formula may change without the transform id changing (our own
        // writes come back this way, as do edits made in the transform editor)
        this.model.dataset.on('transformsChanged', () => {
            if (this.attached)
                this._loadFromTransform();
        });

        this.model.on('change:parentId', event => {
            if (this.attached === false)
                return;

            let dataset = this.model.dataset;
            let parentId = this.model.get('parentId');
            let column = dataset.getColumnById(parentId);
            if (column) {
                this.$variableList.value = column.id.toString();
                this.$variableIcon.setAttribute('variable-type', column.measureType);
                this.$variableIcon.setAttribute('data-type', column.dataType);
            }
            else {
                this.$variableList.value = 'None';
                this.$variableIcon.setAttribute('variable-type', 'none');
                this.$variableIcon.setAttribute('data-type', 'none');
            }
        });

        this.model.dataset.on('transformsChanged transformRemoved', this._updateTransformList, this);
        this.model.dataset.on('dataSetLoaded', this._onDatasetLoaded, this);
        this.model.dataset.on('columnsChanged', this._updateChannelList, this);
        // a transform saved with 'Auto' shows the type the server resolved
        this.model.on('change:measureType', () => {
            if (this.attached)
                this._showMeasureType();
        });

    }

    _updateTransformColour() {
        let transformId = this.model.get('transform');
        if (transformId === null || transformId === 0)
            this.$transformIcon.style.opacity = '0';
        else {
            let transform = this.model.dataset.getTransformById(transformId);
            this.$transformIcon.style.backgroundColor = ColourPalette.get(transform.colourIndex);
            this.$transformIcon.style.opacity = '1';
        }
    }

    // Write the builder's formula to this variable's transform, creating one on
    // first use. A single-element formula array is deliberate: the server only
    // prefixes the source column onto elements before the last one
    // (transform.py produce_formula), so a lone element reaches the parser
    // untouched and may reference any variable, not just $source.
    // The source column is a formality (see the constructor), but it has to
    // point somewhere or the server drops the formula. Anything real will do,
    // so take the first data column that isn't this variable.
    _ensureParent() {
        // jUPWR: the model has no 'id' (that was always undefined, so this
        // variable itself could become its own source); it edits 'ids'
        let ids = this.model.get('ids');
        let ownId = ids ? ids[0] : undefined;

        let parentId = this.model.get('parentId');
        if (parentId !== null && parentId !== 0 && parentId !== ownId
                && this.model.dataset.getColumnById(parentId) !== undefined)
            return;

        let columns = this.model.dataset.get('columns') || [];
        for (let column of columns) {
            if (column.id === ownId)
                continue;
            if (column.columnType === ColumnType.FILTER
                    || column.columnType === ColumnType.NONE)
                continue;
            this.model.set('parentId', column.id);
            return;
        }
    }

    _applyFormula(formula: string) {
        if (this._applying)
            return;

        this._ensureParent();

        let dataset = this.model.dataset;
        let transformId = this.model.get('transform');
        let transform = (transformId !== null && transformId !== 0)
            ? dataset.getTransformById(transformId)
            : undefined;

        if (transform === undefined) {
            this._createOwnTransform(formula);
            return;
        }

        if (this._isShared(transform)) {
            this._createOwnTransform(formula, transform.measureType);
            return;
        }

        dataset.setTransforms([ { id: transform.id, values: { formula: [ formula ] } } ])
            .catch((error) => {
                this._notifyEditProblem({
                    title: error.message,
                    message: error.cause,
                    type: 'error',
                });
            });
    }

    _isShared(transform: Transform): boolean {
        return isTransformShared(this.model.dataset.get('columns') || [],
            transform.id, this.model.get('ids') || []);
    }

    // a transform owned by this variable alone; it is not meant to be picked
    // from the transform list, so it carries the variable's name and no suffix
    // (the server makes the name unique). A string array keeps an old
    // multi-element formula intact when only the type is changed.
    _createOwnTransform(formula: string | string[], measureType?: MeasureType) {
        let dataset = this.model.dataset;
        let values: Partial<Transform> = {
            name: this.model.get('name'),
            description: '',
            suffix: '',
            formula: Array.isArray(formula) ? formula.slice() : [ formula ],
        };
        // jUPWR: new rules start as nominal (there is no 'Auto' to fall back on)
        values.measureType = measureType !== undefined ? measureType : MeasureType.NOMINAL;
        dataset.setTransforms([ { id: 0, values: values } ]).then(() => {
            let transforms = dataset.get('transforms');
            let transformId = transforms[transforms.length - 1].id;
            this.model.set('transform', transformId);
        }).catch((error) => {
            this._notifyEditProblem({
                title: error.message,
                message: error.cause,
                type: 'error',
            });
        });
    }

    // Pull the transform's state into the builder and the measure-type control.
    // Formulas written by the old transform editor are multi-element arrays of
    // source-relative fragments; those cannot be shown as rules, so the builder
    // falls back to its "cannot be shown as conditions" warning.
    _loadFromTransform() {
        let transformId = this.model.get('transform');
        let transform = (transformId !== null && transformId !== 0)
            ? this.model.dataset.getTransformById(transformId)
            : undefined;

        let formula = '';
        let legacy = false;
        if (transform !== undefined && transform.formula.length > 1) {
            // jUPWR: shown as unrepresentable (not as an empty builder),
            // so the warning says editing here replaces it
            legacy = true;
            formula = transform.formula.join('\n');
        }
        else if (transform !== undefined && transform.formula.length === 1)
            formula = transform.formula[0] === '$source' ? '' : transform.formula[0];

        this._applying = true;
        try {
            let ok;
            if (legacy) {
                this.builder.setUnrepresentable(formula);
                ok = false;
            }
            else {
                ok = this.builder.setFormula(formula);
            }
            this.builder.classList.toggle('not-representable', ! ok && formula.trim() !== '');
        }
        finally {
            this._applying = false;
        }

        this._showMeasureType();
    }

    // jUPWR: the list has no 'Auto'. A transform saved with it (older files)
    // shows the type the server actually gave the column; nothing is written
    // until a type is picked.
    _showMeasureType() {
        let transformId = this.model.get('transform');
        let transform = (transformId !== null && transformId !== 0)
            ? this.model.dataset.getTransformById(transformId)
            : undefined;

        let measureType: string = transform === undefined ? 'none' : transform.measureType;
        if (measureType === 'none')
            measureType = this.model.get('measureType') || 'none';
        if (measureType !== 'nominal' && measureType !== 'ordinal' && measureType !== 'continuous')
            measureType = 'nominal';

        this.$measureList.value = measureType;
        this.$measureIcon.setAttribute('measure-type', measureType);
    }

    _setupMeasureList() {
        // no ID here: a recode produces categories or numbers, never a row
        // identifier. No Auto either (jUPWR): it hid the type actually used.
        this.measureList = new MeasureList(false, false);
        this.$measureList.setAttribute('aria-owns', this.measureList.id);
        this.$measureList.addEventListener('mousedown', (event) => {
            if (dropdown.isVisible() === true && dropdown.focusedOn() === this.$measureList)
                dropdown.hide();
            else
                dropdown.show(this.$measureList, this.measureList);
            event.preventDefault();
            event.stopPropagation();
            this.$measureList.focus();
        });

        this.measureList.addEventListener('selected-measure-type', (event: CustomEvent<MeasureType>) => {
            let measureType = event.detail;
            dropdown.hide();

            // show the choice straight away; the reload below confirms it
            this.$measureList.value = measureType;
            this.$measureIcon.setAttribute('measure-type', measureType);

            // the type can be picked before any rule exists, so there may be
            // no transform to write to yet
            this._ensureParent();
            let transformId = this.model.get('transform');
            let transform = (transformId !== null && transformId !== 0)
                ? this.model.dataset.getTransformById(transformId)
                : undefined;

            if (transform === undefined) {
                this._createOwnTransform('', measureType);
                return;
            }

            if (this._isShared(transform)) {
                this._createOwnTransform(transform.formula, measureType);
                return;
            }

            this.model.dataset.setTransforms([ { id: transform.id, values: { measureType: measureType } } ])
                .catch((error) => {
                    this._notifyEditProblem({
                        title: error.message,
                        message: error.cause,
                        type: 'error',
                    });
                });
        });
    }

    _createTransform(values?: Partial<Transform>) {
        if (values === undefined)
            values = { description: '', formula: ['$source'] };
        let dataset = this.model.dataset;
        dataset.setTransforms([ { id: 0, values: values } ]).then(() => {
            this.dispatchEvent(new CustomEvent('transform-selected'));
            let transforms = dataset.get('transforms');
            let transformId = transforms[transforms.length - 1].id;
            this.model.set('transform', transformId);
            this.dispatchEvent(new CustomEvent('edit:transform', { detail: transformId, bubbles: true }));
        }).then(() => {
            dropdown.hide();
        }).catch((error) => {
            this._notifyEditProblem({
                title: error.message,
                message: error.cause,
                type: 'error',
            });
        });
    }

    _notifyEditProblem(details) {
        this._editNote.set(details);
        this.dispatchEvent(new CustomEvent('notification', { detail: this._editNote, bubbles: true }));
    }

    _onDatasetLoaded() {
        this._updateChannelList();
        this._updateTransformList();
    }

    _updateChannelList(event?) {
        if (this.attached === false)
            return;

        let currentColumnId = this.model.attributes.ids[0];
        let dataset = this.model.dataset;
        let currentColumnName = dataset.getColumnById(currentColumnId).name;
        if (event && event.changed.length === 1 && event.changed[0] === currentColumnName)
            return;
        
        let columns = [];
        for (let column of dataset.attributes.columns) {
            if (column.id !== currentColumnId && column.columnType !== 'none' && column.columnType !== 'filter')
                columns.push(column);
        }
        this.variableList.populate(columns);

        this.$variableList.innerHTML = '';
        this.$variableList.append(h('option', { value: '0' }, _('None')));
        for (let i = 0; i < columns.length; i++)
            this.$variableList.append(h('option', { value: columns[i].id.toString() }, columns[i].name));

        let parentId = this.model.get('parentId');
        let column = dataset.getColumnById(parentId);
        if (column) {
            this.$variableList.value = column.id.toString();
            this.$variableIcon.setAttribute('variable-type', column.measureType);
            this.$variableIcon.setAttribute('data-type', column.dataType);
        }
        else {
            this.$variableList.value = '0';
            this.$variableIcon.setAttribute('variable-type', 'none');
            this.$variableIcon.setAttribute('data-type', 'none');
        }

        this._updateErrorMessage();
    }

    _updateErrorMessage() {
        this.$errorMessage.classList.remove('show');

        let errorMsg = this.model.get('formulaMessage');
        if (errorMsg === '') {
            let transformId = this.model.get('transform');
            if (transformId !== null && transformId !== 0) {
                let transform = this.model.dataset.getTransformById(transformId);
                for (let msg of transform.formulaMessage) {
                    if (msg !== '') {
                        errorMsg = _('The selected transform is in error and should be edited.');

                        break;
                    }
                }
            }
        }

        if (errorMsg !== '') {
            this.$errorMessage.textContent = errorMsg;
            this.$errorMessage.classList.add('show');
        }
    }

    _updateTransformList() {
        if (this.attached === false)
            return;

        let transforms = this.model.dataset.get('transforms');
        this.transformList.populate(transforms);

        this.$transformList.innerHTML = '';
        this.$transformList.append(h('option', { value: 'None' }, _('None')));
        for (let transform of transforms)
            this.$transformList.append(h('option', { value: transform.name }, transform.name));

        let transformId = this.model.get('transform');

        if (transformId === null) {
            this.$transformList.value = '';
            this.$editTransform.classList.add('disabled');
        }
        else if (transformId === 0) {
            this.$transformList.value = 'None';
            this.$editTransform.classList.add('disabled');
        }
        else {
            let transform = this.model.dataset.getTransformById(transformId);
            if (transform ===undefined) {
                this.$transformList.value = 'None';
                this.$editTransform.classList.add('disabled');
            }
            else {
                this.$transformList.value = transform.name;
                this.$editTransform.classList.remove('disabled');
            }
        }

        this._updateErrorMessage();
    }

    // the retain-levels caption is shared with the other variable editors, so
    // it is shortened only while this editor (which reuses its line) is up
    _setStatusCaption(text: string) {
        let $caption = this.closest('.jmv-variable-editor-main')?.querySelector('.status-caption');
        if ($caption !== null && $caption !== undefined)
            $caption.textContent = text;
    }

    detach() {
        if ( ! this.attached)
            return;

        this._setStatusCaption(_('Retain unused levels in analyses'));
        this.attached = false;
    }

    attach() {
        this.attached = true;

        // the toggle shares the top row here, so it needs a short caption
        this._setStatusCaption(_('Retain unused levels'));
        this._updateChannelList();
        this._updateTransformList();
        this._updateTransformColour();
        this._updateErrorMessage();
        this._loadFromTransform();
    }

}

customElements.define('jmv-recode-variable-editor', RecodedVarWidget);

export default RecodedVarWidget;
