'use strict';

// jUPWR: documentation of the data sets shipped in module libraries
// (`datasets:` with `documentation:` in a module's 0000.yaml), shared by the
// "O zbiorze" panel and the bibliography.

import * as yaml from 'js-yaml';

import type Instance from './instance';
import type { IReference } from './references';

export interface VariableDocumentation {
    name: string;
    description: string;
}

export interface DatasetLicense {
    name: string;
    notice: string;
    url?: string;
    // ids of the full license texts shipped with the program (client/public/assets/licenses/<id>.txt)
    texts?: string[];
}

// a bibliography entry, in the format of a module's 00refs.yaml
export interface DatasetCitation {
    type: string;
    author: string;
    year: number | string;
    title: string;
    publisher: string;
    url?: string;
    volume?: string | number;
    issue?: string | number;
    pages?: string;
}

export interface DatasetDocumentation {
    summary: string;
    details?: string;
    variables?: VariableDocumentation[];
    source?: string;
    license?: DatasetLicense;
    references?: string[];
    citations?: DatasetCitation[];
    provenance?: {
        package?: string;
        version?: string;
        dataset?: string;
        url?: string;
        note?: string;
        extraUrl?: string;
        extraUrlLabel?: string;
    };
    changes?: string[];
    changesDate?: string;
}

export interface DatasetDefinition {
    name: string;
    path: string;
    documentation?: DatasetDocumentation;
}

interface ModuleDefinition {
    datasets?: DatasetDefinition[];
}

export interface FoundDataset {
    module: string;
    dataset: DatasetDefinition;
}

const LIBRARY_PATH = /^\{\{Examples\}\}\/([^/]+)\/([^/]+)$/;
const INSTALLED_PATH = /(?:^|\/)modules\/([^/]+)\/data\/([^/]+)$/;

// data sets saved elsewhere are only recognised by their title, and only
// those of jDane (as before other modules had documented data sets)
const TITLE_MODULE = 'jDane';

const modules = new Map<string, Promise<ModuleDefinition | null>>();

function loadModule(name: string): Promise<ModuleDefinition | null> {
    let module = modules.get(name);
    if (module === undefined) {
        module = fetch('../modules/' + encodeURIComponent(name))
            .then(async (response) => {
                if ( ! response.ok)
                    return null;
                return yaml.load(await response.text()) as ModuleDefinition;
            })
            .catch(() => null);
        modules.set(name, module);
    }
    return module;
}

export async function findDataset(instance: Instance): Promise<FoundDataset | null> {
    const filePath = (instance.attributes.path || '').replace(/\\/g, '/');
    const title = instance.attributes.title || '';
    const match = LIBRARY_PATH.exec(filePath) || INSTALLED_PATH.exec(filePath);

    const moduleName = match ? match[1] : TITLE_MODULE;
    const module = await loadModule(moduleName);
    const dataset = module?.datasets?.find(item =>
        (match !== null && item.path === match[2])
        || (title !== '' && item.name === title));

    if ( ! dataset?.documentation)
        return null;
    return { module: moduleName, dataset };
}

export function datasetReferences(found: FoundDataset | null): IReference[] {
    if (found === null)
        return [ ];
    const citations = found.dataset.documentation?.citations ?? [];
    const key = found.dataset.path.replace(/\.[^.]*$/, '').replace(/[^A-Za-z0-9]+/g, '-');
    return citations.map((citation, i) => {
        const year = String(citation.year);
        const ref: IReference = {
            name: `${ found.module }-${ key }-${ i + 1 }`,
            type: citation.type,
            authors: { complete: citation.author },
            year: /^[0-9]+$/.test(year) ? parseInt(year) : 0,
            year2: year,
            title: citation.title,
            publisher: citation.publisher,
            url: citation.url ?? '',
        };
        if (citation.volume !== undefined)
            ref.volume = String(citation.volume);
        if (citation.issue !== undefined)
            ref.issue = String(citation.issue);
        if (citation.pages !== undefined)
            ref.pages = citation.pages;
        return ref;
    });
}
