'use strict';

// jUPWR: structured "if / else use" builder for computed variables.
// Rows of clauses are rendered as a form and serialised to a plain
// nested IF(...) formula, so nothing changes server-side or in the
// file format. A formula in that canonical shape is parsed back into
// rows when the editor is opened.

import { h } from '../../common/htmlelementcreator';
import { Column, ColumnType } from '../dataset';

export type Join = 'and' | 'or';

export interface Clause {
    variable: string;
    op: string;
    value: string;
}

export interface Row {
    clauses: Clause[];
    joins: Join[];       // joins[i] sits between clauses[i] and clauses[i + 1]
    value: string;
}

export interface Chain {
    rows: Row[];
    elseValue: string;
}

const OPERATORS = ['==', '!=', '<', '<=', '>', '>='];

function isNumeric(text: string): boolean {
    return /^[-+]?(\d+\.?\d*|\.\d+)([eE][-+]?\d+)?$/.test(text.trim());
}

function isQuoted(text: string): boolean {
    let t = text.trim();
    return t.length >= 2 && ((t[0] === '"' && t[t.length - 1] === '"') || (t[0] === "'" && t[t.length - 1] === "'"));
}

function quoteName(name: string): string {
    if (/^[A-Za-z_][A-Za-z0-9_.]*$/.test(name))
        return name;
    return '`' + name + '`';
}

function unquoteName(name: string): string {
    let t = name.trim();
    if (t.length >= 2 && t[0] === '`' && t[t.length - 1] === '`')
        return t.slice(1, -1);
    return t;
}

// split `text` on top-level occurrences of any of `seps` (ignoring content
// inside quotes, backticks and parentheses)
function splitTopLevel(text: string, seps: string[]): { parts: string[], seps: string[] } {
    let parts: string[] = [];
    let found: string[] = [];
    let depth = 0;
    let quote: string | null = null;
    let start = 0;
    let i = 0;
    while (i < text.length) {
        let c = text[i];
        if (quote !== null) {
            if (c === quote)
                quote = null;
            i++;
            continue;
        }
        if (c === '"' || c === "'" || c === '`') {
            quote = c;
            i++;
            continue;
        }
        if (c === '(') depth++;
        else if (c === ')') depth--;
        else if (depth === 0) {
            let matched = false;
            for (let sep of seps) {
                if (text.substr(i, sep.length) === sep) {
                    parts.push(text.slice(start, i));
                    found.push(sep.trim());
                    i += sep.length;
                    start = i;
                    matched = true;
                    break;
                }
            }
            if (matched)
                continue;
        }
        i++;
    }
    parts.push(text.slice(start));
    return { parts, seps: found };
}

export function formatValue(value: string, columnNames: string[]): string {
    let t = value.trim();
    if (t === '')
        return 'NA';
    if (t === 'NA' || isNumeric(t) || isQuoted(t))
        return t;
    if (columnNames.includes(t))
        return quoteName(t);
    if (t[0] === '`' && t[t.length - 1] === '`')
        return t;
    return '"' + t.replace(/"/g, '\\"') + '"';
}

function displayValue(value: string): string {
    let t = value.trim();
    if (t === 'NA')
        return '';
    if (isQuoted(t)) {
        let inner = t.slice(1, -1);
        if ( ! isNumeric(inner) && inner.indexOf('"') === -1 && inner.indexOf("'") === -1)
            return inner;
    }
    return t;
}

export function chainToFormula(chain: Chain, columnNames: string[]): string {
    let conds: string[] = [];
    let vals: string[] = [];
    for (let row of chain.rows) {
        let pieces: string[] = [];
        let any = false;
        for (let i = 0; i < row.clauses.length; i++) {
            let cl = row.clauses[i];
            if (cl.variable === '' || cl.value.trim() === '')
                continue;
            if (any)
                pieces.push(row.joins[i - 1] || 'and');
            pieces.push(quoteName(cl.variable) + ' ' + cl.op + ' ' + formatValue(cl.value, columnNames));
            any = true;
        }
        if ( ! any)
            continue;
        conds.push(pieces.join(' '));
        vals.push(formatValue(row.value, columnNames));
    }
    let formula = formatValue(chain.elseValue, columnNames);
    if (conds.length === 0)
        return chain.elseValue.trim() === '' ? '' : formula;
    for (let i = conds.length - 1; i >= 0; i--)
        formula = 'IF(' + conds[i] + ', ' + vals[i] + ', ' + formula + ')';
    return formula;
}

function parseClause(text: string): Clause | null {
    let m = /^\s*(`[^`]+`|[A-Za-z_][A-Za-z0-9_.]*)\s*(==|!=|<=|>=|<|>)\s*(.+?)\s*$/.exec(text);
    if (m === null)
        return null;
    if (m[1] === 'NA')
        return null;
    return { variable: unquoteName(m[1]), op: m[2], value: displayValue(m[3]) };
}

function parseCondition(text: string): { clauses: Clause[], joins: Join[] } | null {
    let split = splitTopLevel(text, [' and ', ' or ']);
    let clauses: Clause[] = [];
    for (let part of split.parts) {
        let cl = parseClause(part);
        if (cl === null)
            return null;
        clauses.push(cl);
    }
    return { clauses, joins: split.seps as Join[] };
}

// returns null when the formula is not a plain IF chain
export function formulaToChain(formula: string): Chain | null {
    let rows: Row[] = [];
    let text = formula.trim();
    if (text === '')
        return { rows: [], elseValue: '' };
    while (true) {
        if (/^IF\s*\(/i.test(text) && text[text.length - 1] === ')') {
            let inner = text.slice(text.indexOf('(') + 1, -1);
            let args = splitTopLevel(inner, [',']).parts;
            if (args.length !== 3)
                return null;
            let cond = parseCondition(args[0]);
            if (cond === null)
                return null;
            rows.push({ clauses: cond.clauses, joins: cond.joins, value: displayValue(args[1]) });
            text = args[2].trim();
        }
        else {
            if (rows.length === 0)
                return null;
            let cond = parseCondition(text);
            if (cond !== null)
                return null; // a bare comparison is a formula, not an else value
            if (/[()]/.test(text))
                return null;
            return { rows, elseValue: displayValue(text) };
        }
    }
}

export class ConditionsBuilder extends HTMLElement {

    chain: Chain = { rows: [], elseValue: '' };
    columns: () => Column[];
    currentName: () => string;
    onChange: (formula: string) => void;
    $rows: HTMLElement;
    $else: HTMLInputElement;
    lastFormula: string | null = null;

    constructor(columns: () => Column[], currentName: () => string, onChange: (formula: string) => void) {
        super();
        this.columns = columns;
        this.currentName = currentName;
        this.onChange = onChange;
        this.classList.add('jmv-conditions-builder');
        this.setAttribute('data-warning', _('The current formula cannot be shown as conditions; editing here will replace it.'));

        let $add = h('button', { class: 'add-row' }, h('span', { class: 'plus' }), _('Add rule'));
        $add.addEventListener('click', () => {
            this.chain.rows.push(this._emptyRow());
            this.render();
            this._commit();
        });
        this.append($add);

        this.$rows = h('div', { class: 'rows' });
        this.append(this.$rows);

        let $elseRow = h('div', { class: 'row else-row' });
        $elseRow.append(h('span', { class: 'tag' }, _('else use')));
        this.$else = h('input', { type: 'text', class: 'value', placeholder: _('e.g. "other"') }) as HTMLInputElement;
        this.$else.addEventListener('change', () => { this.chain.elseValue = this.$else.value; this._commit(); });
        this.$else.addEventListener('keydown', (e: KeyboardEvent) => { if (e.key === 'Enter') this.$else.blur(); });
        $elseRow.append(this.$else);
        this.append($elseRow);

        this.append(h('div', { class: 'hint' },
            _('Values: numbers as is, text in quotes (added automatically), or a variable name. Within a row "and" binds tighter than "or".')));
    }

    _emptyRow(): Row {
        return { clauses: [ { variable: '', op: '==', value: '' } ], joins: [], value: '' };
    }

    _columnNames(): string[] {
        return this.columns().map(c => c.name);
    }

    // load a formula; returns false when it could not be represented as rows
    setFormula(formula: string): boolean {
        if (formula === this.lastFormula)
            return true;
        let chain = formulaToChain(formula);
        if (chain === null) {
            this.chain = { rows: [ this._emptyRow() ], elseValue: '' };
            this.lastFormula = formula;
            this.render();
            return false;
        }
        if (chain.rows.length === 0)
            chain.rows.push(this._emptyRow());
        this.chain = chain;
        this.lastFormula = formula;
        this.render();
        return true;
    }

    _commit() {
        let formula = chainToFormula(this.chain, this._columnNames());
        if (formula === this.lastFormula)
            return;
        this.lastFormula = formula;
        this.onChange(formula);
    }

    render() {
        this.$rows.replaceChildren();
        this.chain.rows.forEach((row, ri) => this.$rows.append(this._renderRow(row, ri)));
        this.$else.value = this.chain.elseValue;
    }

    _renderRow(row: Row, ri: number): HTMLElement {
        let $row = h('div', { class: 'row' });
        let $clauses = h('div', { class: 'clauses' });
        $row.append($clauses);

        row.clauses.forEach((clause, ci) => {
            let $line = h('div', { class: 'clause' });
            if (ci === 0)
                $line.append(h('span', { class: 'tag' }, _('if')));
            else {
                let $join = h('select', { class: 'join' }) as HTMLSelectElement;
                for (let j of ['and', 'or']) {
                    let $o = h('option', { value: j }, j) as HTMLOptionElement;
                    if (row.joins[ci - 1] === j) $o.selected = true;
                    $join.append($o);
                }
                $join.addEventListener('change', () => { row.joins[ci - 1] = $join.value as Join; this._commit(); });
                $line.append($join);
            }

            let $var = h('select', { class: 'variable' }) as HTMLSelectElement;
            $var.append(h('option', { value: '' }, _('variable…')));
            let selected: Column | null = null;
            for (let col of this.columns()) {
                if ( ! col.name || col.columnType === ColumnType.FILTER || col.name === this.currentName())
                    continue;
                let $o = h('option', { value: col.name }, col.name) as HTMLOptionElement;
                if (col.name === clause.variable) { $o.selected = true; selected = col; }
                $var.append($o);
            }
            if (clause.variable !== '' && selected === null) {
                let $o = h('option', { value: clause.variable }, clause.variable) as HTMLOptionElement;
                $o.selected = true;
                $var.append($o);
            }
            $line.append($var);

            let $op = h('select', { class: 'op' }) as HTMLSelectElement;
            for (let op of OPERATORS) {
                let $o = h('option', { value: op }, op) as HTMLOptionElement;
                if (op === clause.op) $o.selected = true;
                $op.append($o);
            }
            $line.append($op);

            let listId = 'jmv-cond-levels-' + ri + '-' + ci;
            let $val = h('input', { type: 'text', class: 'value', placeholder: _('value'), list: listId }) as HTMLInputElement;
            $val.value = clause.value;
            let $list = h('datalist', { id: listId });
            let fillLevels = (col: Column | null) => {
                $list.replaceChildren();
                if (col && col.levels)
                    for (let lvl of col.levels)
                        $list.append(h('option', { value: lvl.label }));
            };
            fillLevels(selected);
            $line.append($val, $list);

            $var.addEventListener('change', () => {
                clause.variable = $var.value;
                fillLevels(this.columns().find(c => c.name === $var.value) || null);
                this._commit();
            });
            $op.addEventListener('change', () => { clause.op = $op.value; this._commit(); });
            $val.addEventListener('change', () => { clause.value = $val.value; this._commit(); });
            $val.addEventListener('keydown', (e: KeyboardEvent) => { if (e.key === 'Enter') $val.blur(); });

            if (row.clauses.length > 1) {
                let $rm = h('button', { class: 'remove-clause', title: _('Remove condition part') }, '×');
                $rm.addEventListener('click', () => {
                    row.clauses.splice(ci, 1);
                    row.joins.splice(Math.max(ci - 1, 0), 1);
                    this.render();
                    this._commit();
                });
                $line.append($rm);
            }
            $clauses.append($line);
        });

        let $addClause = h('button', { class: 'add-clause' }, '+ ' + _('and / or condition'));
        $addClause.addEventListener('click', () => {
            row.clauses.push({ variable: '', op: '==', value: '' });
            row.joins.push('and');
            this.render();
        });
        $clauses.append($addClause);

        let $use = h('div', { class: 'use' });
        $use.append(h('span', { class: 'tag' }, _('use')));
        let $useVal = h('input', { type: 'text', class: 'value', placeholder: _('e.g. "high"') }) as HTMLInputElement;
        $useVal.value = row.value;
        $useVal.addEventListener('change', () => { row.value = $useVal.value; this._commit(); });
        $useVal.addEventListener('keydown', (e: KeyboardEvent) => { if (e.key === 'Enter') $useVal.blur(); });
        $use.append($useVal);
        let $rmRow = h('button', { class: 'remove-row', title: _('Remove rule') }, '×');
        $rmRow.addEventListener('click', () => {
            this.chain.rows.splice(ri, 1);
            this.render();
            this._commit();
        });
        $use.append($rmRow);
        $row.append($use);
        return $row;
    }
}

customElements.define('jmv-conditions-builder', ConditionsBuilder);

export default ConditionsBuilder;
