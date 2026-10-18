// @vitest-environment jsdom

import { beforeAll, describe, expect, it } from 'vitest';

beforeAll(() => {
    // the module is part of the app UI and uses the global gettext stub
    (globalThis as any)._ = (s: string) => s;
});

async function mod() {
    return await import('../recodedvarwidget');
}

describe('isTransformShared', () => {

    const columns = [
        { id: 1, transform: 0 },
        { id: 2, transform: 7 },
        { id: 3, transform: 7 },
        { id: 4, transform: 9 },
    ];

    it('is shared when another column uses the transform', async () => {
        const { isTransformShared } = await mod();
        expect(isTransformShared(columns, 7, [ 2 ])).toBe(true);
    });

    it('is not shared when only the edited column uses it', async () => {
        const { isTransformShared } = await mod();
        expect(isTransformShared(columns, 9, [ 4 ])).toBe(false);
    });

    it('is not shared when every user of it is being edited together', async () => {
        const { isTransformShared } = await mod();
        expect(isTransformShared(columns, 7, [ 2, 3 ])).toBe(false);
    });
});
