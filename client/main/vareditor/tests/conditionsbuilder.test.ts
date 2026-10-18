// @vitest-environment jsdom

import { beforeAll, describe, expect, it } from 'vitest';

beforeAll(() => {
    // the module is part of the app UI and uses the global gettext stub
    (globalThis as any)._ = (s: string) => s;
});

const cols = ['a', 'b', 'c'];

async function mod() {
    return await import('../conditionsbuilder');
}

describe('chainToFormula', () => {

    it('serialises a flat row without brackets', async () => {
        const { chainToFormula } = await mod();
        expect(chainToFormula({ rows: [ {
            clauses: [ { variable: 'a', op: '==', value: '1' }, { variable: 'b', op: '==', value: '2' } ],
            joins: [ 'or' ], value: 'x' } ], elseValue: 'y' }, cols))
            .toBe('IF(a == 1 or b == 2, "x", "y")');
    });

    it('brackets an explicit group so it overrides precedence', async () => {
        const { chainToFormula } = await mod();
        expect(chainToFormula({ rows: [ {
            clauses: [
                { terms: [ { variable: 'a', op: '==', value: '1' }, { variable: 'b', op: '==', value: '2' } ], joins: [ 'or' ] },
                { variable: 'c', op: '==', value: '3' } ],
            joins: [ 'and' ], value: 'x' } ], elseValue: 'y' }, cols))
            .toBe('IF((a == 1 or b == 2) and c == 3, "x", "y")');
    });

    it('does not bracket a group holding a single term', async () => {
        const { chainToFormula } = await mod();
        expect(chainToFormula({ rows: [ {
            clauses: [ { terms: [ { variable: 'a', op: '==', value: '1' } ], joins: [] } ],
            joins: [], value: 'x' } ], elseValue: 'y' }, cols))
            .toBe('IF(a == 1, "x", "y")');
    });

    it('prunes incomplete parts inside a group', async () => {
        const { chainToFormula } = await mod();
        expect(chainToFormula({ rows: [ {
            clauses: [
                { terms: [ { variable: 'a', op: '==', value: '1' }, { variable: '', op: '==', value: '' } ], joins: [ 'or' ] },
                { variable: 'c', op: '==', value: '3' } ],
            joins: [ 'and' ], value: 'x' } ], elseValue: 'y' }, cols))
            .toBe('IF(a == 1 and c == 3, "x", "y")');
    });
});

describe('formulaToChain', () => {

    it('round trips a bracketed condition', async () => {
        const { chainToFormula, formulaToChain } = await mod();
        const formula = 'IF((a == 1 or b == 2) and c == 3, "x", "y")';
        const chain = formulaToChain(formula);
        expect(chain).not.toBeNull();
        expect(chainToFormula(chain!, cols)).toBe(formula);
    });

    it('round trips nested brackets', async () => {
        const { chainToFormula, formulaToChain } = await mod();
        const formula = 'IF((a == 1 or (b == 2 and c == 3)) and c == 4, "x", "y")';
        const chain = formulaToChain(formula);
        expect(chain).not.toBeNull();
        expect(chainToFormula(chain!, cols)).toBe(formula);
    });

    it('reads sibling brackets as two terms, not one group', async () => {
        const { formulaToChain } = await mod();
        const chain = formulaToChain('IF((a == 1) or (b == 2), "x", "y")');
        expect(chain).not.toBeNull();
        expect(chain!.rows[0].clauses).toHaveLength(2);
        expect(chain!.rows[0].joins).toEqual([ 'or' ]);
    });

    it('rejects a formula that is not an IF chain', async () => {
        const { formulaToChain } = await mod();
        expect(formulaToChain('a + b')).toBeNull();
    });

    it('rejects a bracketed else value', async () => {
        const { formulaToChain } = await mod();
        expect(formulaToChain('IF(a == 1, "x", (b))')).toBeNull();
    });

    it('keeps a text equal to a column name as text', async () => {
        const { chainToFormula, formulaToChain } = await mod();
        const formula = 'IF(a == 1, "b", "other")';
        expect(chainToFormula(formulaToChain(formula, cols)!, cols)).toBe(formula);
    });

    it('keeps the texts "NA" and "3" as text', async () => {
        const { chainToFormula, formulaToChain } = await mod();
        const formula = 'IF(a == "NA", "3", "other")';
        expect(chainToFormula(formulaToChain(formula, cols)!, cols)).toBe(formula);
    });

    it('still shows a plain text without quotes', async () => {
        const { formulaToChain } = await mod();
        const chain = formulaToChain('IF(a == "x", "high", "low")', cols);
        expect(chain!.rows[0].value).toBe('high');
        expect(chain!.elseValue).toBe('low');
    });

    it('rejects an expression it would save as text', async () => {
        const { formulaToChain } = await mod();
        expect(formulaToChain('IF(a == 1, 2 + 3, 0)', cols)).toBeNull();
        expect(formulaToChain('IF(a == 1, "x", b * 2)', cols)).toBeNull();
        expect(formulaToChain('IF(a == 2 + 3, "x", "y")', cols)).toBeNull();
    });

    it('rejects a name that is not a column', async () => {
        const { formulaToChain } = await mod();
        expect(formulaToChain('IF(a == 1, zz, "y")', cols)).toBeNull();
    });

    it('accepts numbers, NA, columns and single-quoted text', async () => {
        const { chainToFormula, formulaToChain } = await mod();
        expect(chainToFormula(formulaToChain('IF(a == -1.5, b, NA)', cols)!, cols))
            .toBe('IF(a == -1.5, b, NA)');
        expect(chainToFormula(formulaToChain("IF(a == 1, 'x', 'y')", cols)!, cols))
            .toBe('IF(a == 1, "x", "y")');
    });
});

describe('ConditionsBuilder', () => {

    it('keeps reporting an unrepresentable formula on reload', async () => {
        const { ConditionsBuilder } = await mod();
        const b = new ConditionsBuilder(() => [], () => '', () => {});
        expect(b.setFormula('a + b')).toBe(false);
        expect(b.setFormula('a + b')).toBe(false);
    });

    it('reports a legacy transform until the builder replaces it', async () => {
        const { ConditionsBuilder } = await mod();
        let written: string[] = [];
        const b = new ConditionsBuilder(() => [], () => '', (f) => written.push(f));
        b.setUnrepresentable('$source == 1\n"x"\n$source');
        expect(b.lastRepresentable).toBe(false);
        b.chain.elseValue = 'y';
        b._commit();
        expect(written).toEqual([ '"y"' ]);
        expect(b.setFormula('"y"')).toBe(true);
    });
});
