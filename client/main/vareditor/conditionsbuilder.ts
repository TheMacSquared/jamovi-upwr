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

// a parenthesised sub-condition; nesting lets the user override the
// default precedence ("and" binds tighter than "or") explicitly
export interface Group {
    terms: Term[];
    joins: Join[];
}

export type Term = Clause | Group;

export function isGroup(term: Term): term is Group {
    return (term as Group).terms !== undefined;
}

export interface Row {
    clauses: Term[];
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

// serialise a list of terms joined by `joins`; returns '' when nothing in the
// list is complete enough to contribute. `wrap` parenthesises a multi-term
// result, which is what makes an explicit group override precedence.
function termsToText(terms: Term[], joins: Join[], columnNames: string[], wrap: boolean): string {
    let pieces: string[] = [];
    let count = 0;
    for (let i = 0; i < terms.length; i++) {
        let term = terms[i];
        let text: string;
        if (isGroup(term))
            text = termsToText(term.terms, term.joins, columnNames, true);
        else if (term.variable === '' || term.value.trim() === '')
            text = '';
        else
            text = quoteName(term.variable) + ' ' + term.op + ' ' + formatValue(term.value, columnNames);
        if (text === '')
            continue;
        if (count > 0)
            pieces.push(joins[i - 1] || 'and');
        pieces.push(text);
        count++;
    }
    if (count === 0)
        return '';
    let text = pieces.join(' ');
    return (wrap && count > 1) ? '(' + text + ')' : text;
}

export function chainToFormula(chain: Chain, columnNames: string[]): string {
    let conds: string[] = [];
    let vals: string[] = [];
    for (let row of chain.rows) {
        let cond = termsToText(row.clauses, row.joins, columnNames, false);
        if (cond === '')
            continue;
        conds.push(cond);
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

// strip one layer of redundant wrapping parentheses, e.g. "(a == 1 or b == 2)"
function unwrap(text: string): string | null {
    let t = text.trim();
    if (t.length < 2 || t[0] !== '(' || t[t.length - 1] !== ')')
        return null;
    // make sure the leading '(' actually closes at the very end, so that
    // "(a == 1) or (b == 2)" is not mistaken for one wrapped group
    let inner = t.slice(1, -1);
    let depth = 0;
    let quote: string | null = null;
    for (let i = 0; i < inner.length; i++) {
        let c = inner[i];
        if (quote !== null) {
            if (c === quote) quote = null;
            continue;
        }
        if (c === '"' || c === "'" || c === '`') quote = c;
        else if (c === '(') depth++;
        else if (c === ')') { depth--; if (depth < 0) return null; }
    }
    return depth === 0 ? inner : null;
}

function parseCondition(text: string): { clauses: Term[], joins: Join[] } | null {
    let split = splitTopLevel(text, [' and ', ' or ']);
    let terms: Term[] = [];
    for (let part of split.parts) {
        let inner = unwrap(part);
        if (inner !== null) {
            let sub = parseCondition(inner);
            if (sub === null)
                return null;
            // a group of one is just the clause itself
            if (sub.clauses.length === 1 && ! isGroup(sub.clauses[0]))
                terms.push(sub.clauses[0]);
            else
                terms.push({ terms: sub.clauses, joins: sub.joins });
            continue;
        }
        let cl = parseClause(part);
        if (cl === null)
            return null;
        terms.push(cl);
    }
    return { clauses: terms, joins: split.seps as Join[] };
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
    $addRow: HTMLButtonElement;
    $elseRow: HTMLElement;
    $else: HTMLInputElement;
    lastFormula: string | null = null;

    constructor(columns: () => Column[], currentName: () => string, onChange: (formula: string) => void) {
        super();
        this.columns = columns;
        this.currentName = currentName;
        this.onChange = onChange;
        this.classList.add('jmv-conditions-builder');
        this.setAttribute('data-warning', _('The current formula cannot be shown as conditions; editing here will replace it.'));

        // the host places this in the mode bar, so it costs no vertical space here
        this.$addRow = h('button', { class: 'add-row' }, h('span', { class: 'plus' }), _('Add rule')) as HTMLButtonElement;
        this.$addRow.addEventListener('click', () => {
            this.chain.rows.push(this._emptyRow());
            this.render();
            this._commit();
        });

        this.$rows = h('div', { class: 'rows' });
        this.append(this.$rows);

        // the else row scrolls with the conditions rather than being pinned
        this.$elseRow = h('div', { class: 'row else-row' });
        this.$elseRow.append(h('span', { class: 'tag' }, _('else use')));
        this.$else = h('input', { type: 'text', class: 'value', placeholder: _('e.g. "other"') }) as HTMLInputElement;
        this.$else.addEventListener('change', () => { this.chain.elseValue = this.$else.value; this._commit(); });
        this.$else.addEventListener('keydown', (e: KeyboardEvent) => { if (e.key === 'Enter') this.$else.blur(); });
        this.$elseRow.append(this.$else);
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
        this.$rows.append(this.$elseRow);
        this.$else.value = this.chain.elseValue;
    }

    // renders one term list (a row body, or the inside of a group) with its
    // join selectors; `path` keeps datalist ids unique across nesting levels
    _renderTerms(terms: Term[], joins: Join[], path: string, isRoot: boolean): HTMLElement {
        let $terms = h('div', { class: 'clauses' });

        let removeAt = (i: number) => {
            terms.splice(i, 1);
            joins.splice(Math.max(i - 1, 0), 1);
            this.render();
            this._commit();
        };

        terms.forEach((term, ti) => {
            let $lead = h('div', { class: 'lead' });
            if (ti === 0)
                $lead.append(h('span', { class: 'tag' }, isRoot ? _('if') : ''));
            else {
                let $join = h('select', { class: 'join' }) as HTMLSelectElement;
                for (let j of ['and', 'or']) {
                    let $o = h('option', { value: j }, j) as HTMLOptionElement;
                    if (joins[ti - 1] === j) $o.selected = true;
                    $join.append($o);
                }
                $join.addEventListener('change', () => { joins[ti - 1] = $join.value as Join; this._commit(); });
                $lead.append($join);
            }

            if (isGroup(term)) {
                let $group = h('div', { class: 'group' });
                $group.append($lead);
                let $inner = h('div', { class: 'group-body' });
                $inner.append(this._renderTerms(term.terms, term.joins, path + '-' + ti, false));
                $group.append($inner);
                let $ungroup = h('button', { class: 'remove-clause', title: _('Remove bracket (keep its parts)') }, '⌦');
                $ungroup.addEventListener('click', () => {
                    // splice the group's terms back into the parent list
                    let args: any[] = [ti, 1, ...term.terms];
                    terms.splice.apply(terms, args);
                    joins.splice(ti, 0, ...term.joins);
                    this.render();
                    this._commit();
                });
                $group.append($ungroup);
                let $rm = h('button', { class: 'remove-clause', title: _('Remove condition part') }, '×');
                $rm.addEventListener('click', () => removeAt(ti));
                $group.append($rm);
                $terms.append($group);
                return;
            }

            let clause = term as Clause;
            let $line = h('div', { class: 'clause' });
            $line.append($lead);

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

            let listId = 'jmv-cond-levels-' + path + '-' + ti;
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

            if (terms.length > 1) {
                let $rm = h('button', { class: 'remove-clause', title: _('Remove condition part') }, '×');
                $rm.addEventListener('click', () => removeAt(ti));
                $line.append($rm);
            }
            $terms.append($line);
        });

        let $actions = h('div', { class: 'term-actions' });
        let $addClause = h('button', { class: 'add-clause' }, '+ ' + _('and / or condition'));
        $addClause.addEventListener('click', () => {
            terms.push({ variable: '', op: '==', value: '' });
            joins.push('and');
            this.render();
        });
        $actions.append($addClause);

        let $addGroup = h('button', { class: 'add-clause' }, '+ ' + _('bracket'));
        $addGroup.addEventListener('click', () => {
            terms.push({ terms: [ { variable: '', op: '==', value: '' }, { variable: '', op: '==', value: '' } ], joins: [ 'or' ] });
            joins.push('and');
            this.render();
        });
        $actions.append($addGroup);
        $terms.append($actions);
        return $terms;
    }

    _renderRow(row: Row, ri: number): HTMLElement {
        let $row = h('div', { class: 'row' });
        $row.append(this._renderTerms(row.clauses, row.joins, String(ri), true));

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
