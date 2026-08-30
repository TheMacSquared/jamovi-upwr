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
});
