'use strict';

import yaml from 'js-yaml';

import ActionHub from './actionhub';
import { h, url } from '../common/htmlelementcreator';
import type Instance from './instance';

interface VariableDocumentation {
    name: string;
    description: string;
}

interface DatasetDocumentation {
    summary: string;
    details?: string;
    variables?: VariableDocumentation[];
    source?: string;
    references?: string[];
    provenance?: {
        package?: string;
        dataset?: string;
        url?: string;
    };
    changes?: string[];
}

interface DatasetDefinition {
    name: string;
    path: string;
    documentation?: DatasetDocumentation;
}

interface ModuleDefinition {
    datasets?: DatasetDefinition[];
}

const JDANE_LIBRARY_PATH = /^\{\{Examples\}\}\/jDane\/([^/]+)$/;
const JDANE_INSTALLED_PATH = /(?:^|\/)modules\/jDane\/data\/([^/]+)$/;

function datasetPathFromInstancePath(filePath: string): string | null {
    const normalisedPath = (filePath || '').replace(/\\/g, '/');
    const match = JDANE_LIBRARY_PATH.exec(normalisedPath)
        || JDANE_INSTALLED_PATH.exec(normalisedPath);
    return match?.[1] || null;
}

class DatasetInfo extends HTMLElement {
    private instance: Instance;
    private activeDataset: DatasetDefinition | null = null;
    private closeButton: HTMLButtonElement;
    private content: HTMLElement;
    private title: HTMLElement;

    constructor(instance: Instance) {
        super();
        this.instance = instance;
        this.classList.add('jmv-dataset-info');
        this.setAttribute('role', 'dialog');
        this.setAttribute('aria-modal', 'false');
        this.setAttribute('aria-labelledby', 'dataset-info-title');
        this.setAttribute('aria-hidden', 'true');

        this.closeButton = h('button', {
            class: 'jmv-dataset-info-close',
            type: 'button',
            'aria-label': 'Zamknij'
        }, '\u00d7') as HTMLButtonElement;
        this.content = h('div', { class: 'jmv-dataset-info-content' });
        this.title = h('h2', { id: 'dataset-info-title' }, 'O zbiorze');

        this.append(
            h('div', { class: 'jmv-dataset-info-header' },
                this.title,
                this.closeButton),
            this.content);

        this.closeButton.addEventListener('click', () => this.hide());
        this.addEventListener('keydown', (event: KeyboardEvent) => {
            if (event.key === 'Escape') {
                this.hide();
                event.stopPropagation();
            }
        });

        const action = ActionHub.get('datasetInfo');
        action.set('enabled', false);
        action.on('request', () => this.show());

        this.instance.on('change:path change:title', () => this.updateForDataset());
        if (this.instance.attributes.path || this.instance.attributes.title)
            this.updateForDataset();
    }

    private async updateForDataset() {
        try {
            const response = await fetch('../modules/jDane');
            if ( ! response.ok)
                throw new Error(`Unable to load jDane metadata (${response.status})`);

            const module = yaml.load(await response.text()) as ModuleDefinition;
            const datasetPath = datasetPathFromInstancePath(this.instance.attributes.path);
            const datasetTitle = this.instance.attributes.title;
            const dataset = module.datasets?.find(item =>
                (datasetPath !== null && item.path === datasetPath)
                || (datasetTitle !== '' && item.name === datasetTitle));
            if (dataset?.documentation) {
                this.activeDataset = dataset;
                this.render(dataset);
                ActionHub.get('datasetInfo').set('enabled', true);
            }
            else {
                console.info('No jDane documentation for data set', {
                    path: this.instance.attributes.path,
                    title: datasetTitle,
                });
            }
        }
        catch (error) {
            console.warn('Unable to load data set documentation', error);
        }
    }

    private addSection(title: string, ...children: Node[]) {
        this.content.append(h('section', {}, h('h3', {}, title), ...children));
    }

    private render(dataset: DatasetDefinition) {
        const documentation = dataset.documentation;
        if (documentation === undefined)
            return;
        this.title.textContent = dataset.name;
        this.content.replaceChildren(h('p', { class: 'jmv-dataset-info-summary' }, documentation.summary));

        if (documentation.details)
            this.addSection('Szczegóły', h('p', {}, documentation.details));

        if (documentation.variables?.length) {
            const body = h('tbody');
            for (const variable of documentation.variables) {
                body.append(h('tr', {},
                    h('th', { scope: 'row' }, h('code', {}, variable.name)),
                    h('td', {}, variable.description)));
            }
            this.addSection('Zmienne', h('table', { class: 'jmv-dataset-info-variables' }, body));
        }

        if (documentation.source)
            this.addSection('Źródło', h('p', {}, documentation.source));

        if (documentation.references?.length) {
            const list = h('ul');
            for (const reference of documentation.references)
                list.append(h('li', {}, reference));
            this.addSection('Bibliografia', list);
        }

        if (documentation.changes?.length) {
            const list = h('ul');
            for (const change of documentation.changes)
                list.append(h('li', {}, change));
            this.addSection('Zmiany w jUPWR', list);
        }

        if (documentation.provenance) {
            const provenance = documentation.provenance;
            const label = [provenance.package, provenance.dataset].filter(Boolean).join(' \u2014 ');
            const paragraph = h('p', {}, label);
            if (provenance.url) {
                paragraph.append(' \u00b7 ', h('a', {
                    href: url(provenance.url),
                    target: '_blank',
                    rel: 'noopener noreferrer'
                }, 'Oryginalna dokumentacja'));
            }
            this.addSection('Pochodzenie', paragraph);
        }
    }

    show() {
        if (this.activeDataset === null)
            return;
        this.classList.add('open');
        this.setAttribute('aria-hidden', 'false');
        this.closeButton.focus();
    }

    hide() {
        this.classList.remove('open');
        this.setAttribute('aria-hidden', 'true');
    }
}

customElements.define('jmv-dataset-info', DatasetInfo);

export default DatasetInfo;
