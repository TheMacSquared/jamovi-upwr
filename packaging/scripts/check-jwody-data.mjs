#!/usr/bin/env node
// Verify the data library actually shipped in a .jmo, including its documentation.
import assert from 'node:assert/strict';
import fs from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { createRequire } from 'node:module';
const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
const require = createRequire(path.join(root, 'jamovi-compiler/package.json'));
const yaml = require('js-yaml');
const JSZip = require('jszip');
const source = yaml.load(await fs.readFile(path.join(root, 'jWody/jamovi/0000.yaml'), 'utf8'));
const archives = process.argv.slice(2);
assert(archives.length > 0, 'Podaj co najmniej jeden plik .jmo');
assert.equal(source.datasets.length, 3);
for (const archive of archives) {
    const zip = await JSZip.loadAsync(await fs.readFile(archive), { checkCRC32: true });
    for (const manifest of ['jamovi.yaml', 'jamovi-full.yaml']) {
        const entry = zip.file(`jWody/${manifest}`);
        assert(entry, `Brak ${manifest}`);
        const bundled = yaml.load(await entry.async('string'));
        assert.equal(bundled.version, source.version);
        assert.deepEqual(bundled.datasets, source.datasets, 'Metadane biblioteki różnią się od źródeł');
    }
    for (const dataset of source.datasets) {
        const bytes = await fs.readFile(path.join(root, 'jWody/data', dataset.path));
        const entry = zip.file(`jWody/data/${dataset.path}`);
        assert(entry, `Brak pliku biblioteki: ${dataset.path}`);
        assert.deepEqual(await entry.async('nodebuffer'), bytes, 'Dane w archiwum różnią się od źródeł');
        const lines = bytes.toString('utf8').trim().split(/\r?\n/);
        const headers = lines[0].split(',').map(s => s.replace(/^"|"$/g, ''));
        assert.deepEqual(dataset.documentation.variables.map(v => v.name), headers);
        assert(dataset.documentation.variables.every(v => v.description.length > 15));
        assert(dataset.documentation.details.includes('Hydrologia →'));
        assert.equal(dataset.documentation.provenance.package, 'jWody');
        assert(dataset.documentation.changes.length > 0);
        assert(dataset.documentation.source.includes('syntetyczne'));
        console.log(`${path.basename(archive)}: ${dataset.path} — ${lines.length - 1} wierszy, pełny opis, zgodność danych OK`);
    }
}
