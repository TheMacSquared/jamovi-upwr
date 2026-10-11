#!/usr/bin/env node
// Verify that every jDane data set documents its license and that the license
// texts it points to ship with the client. Exits non-zero on any gap.
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { createRequire } from 'node:module';
const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '../..');
const require = createRequire(path.join(root, 'jamovi-compiler/package.json'));
const yaml = require('js-yaml');
const module_ = yaml.load(fs.readFileSync(path.join(root, 'jDane/jamovi/0000.yaml'), 'utf8'));
const textsDir = path.join(root, 'client/public/assets/licenses');
const problems = [];
for (const ds of module_.datasets ?? []) {
    const who = ds.path;
    const doc = ds.documentation ?? {};
    const lic = doc.license;
    if (!fs.existsSync(path.join(root, 'jDane/data', ds.path)))
        problems.push(`${who}: brak pliku w jDane/data`);
    if (!lic) { problems.push(`${who}: brak pola license`); continue; }
    for (const key of ['name', 'notice', 'url'])
        if (!lic[key]) problems.push(`${who}: license.${key} puste`);
    if (!Array.isArray(lic.texts) || lic.texts.length === 0)
        problems.push(`${who}: license.texts puste`);
    for (const id of lic.texts ?? [])
        if (!fs.existsSync(path.join(textsDir, `${id}.txt`)))
            problems.push(`${who}: brak client/public/assets/licenses/${id}.txt`);
    if (doc.provenance && doc.provenance.package !== 'jDane' && !doc.provenance.version)
        problems.push(`${who}: provenance.version puste`);
    const changed = (doc.changes ?? []).some(c => !/bez zmian/i.test(c));
    if (changed && !doc.changesDate)
        problems.push(`${who}: zmiany względem źródła bez changesDate`);
}
if (problems.length) {
    console.error('Licencje zbiorów jDane — problemy:\n  ' + problems.join('\n  '));
    process.exit(1);
}
console.log(`Licencje zbiorów jDane: OK (${module_.datasets.length} zbiorów)`);
