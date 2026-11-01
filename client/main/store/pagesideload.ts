//
// Copyright (C) 2016 Jonathon Love
//

'use strict';

import host from '../host';
import { Modules } from '../modules';
import Notify from '../notification';
import { h }  from '../../common/htmlelementcreator';

class PageSideload extends HTMLElement {
    model: Modules;
    $body: HTMLElement;
    $drop: HTMLButtonElement;
    private _installing = false;

    constructor(model: Modules) {
        super();

        this.model = model;
        this.classList.add('PageSideload');
        this.classList.add('jmv-store-page-sideload');
        this.setAttribute('role', 'tabpanel');
        this.$body = h('div', { class: 'jmv-store-body' });
        this.append(this.$body);
        this.$drop = h('button', { class: 'jmv-store-page-installed-drop', tabindex: '-1' }, h('span', { class: 'mif-file-upload' }));
        this.$body.append(this.$drop);
        this.$drop.addEventListener('click', event => this._dropClicked());
        this.addEventListener('drop', event => this._filesDropped(event));
    }

    async _dropClicked() {
        if (host.isElectron) {

            let filters = [ { name: _('jamovi modules'), extensions: ['jmo']} ];
            let result = await host.showOpenDialog({ filters });

            if ( ! result.cancelled) {
                const path = result.paths[0];
                await this._install([path]);
            }

            this.$drop.focus();
        }
    }

    async _filesDropped(event: DragEvent) {
        if ( ! host.isElectron)
            return;

        // Keep module packages out of the document-level dataset opener.
        event.preventDefault();
        event.stopPropagation();
        try {
            const paths = Array.from(event.dataTransfer.files, file => {
                const path = host.getPathForFile(file);
                if ( ! path)
                    throw new Error(_('Unable to open'));
                return path;
            });
            if (paths.length > 0)
                await this._install(paths);
        }
        catch (e) {
            this._installFailure(e);
        }
    }

    private async _install(paths: string[]) {
        if (this._installing)
            return;
        this._installing = true;
        this.$drop.disabled = true;
        try {
            // Install one at a time; close the panel after the whole batch.
            for (const path of paths)
                await this.model.install(path);
            this._installSuccess();
        }
        catch (e) {
            this._installFailure(e);
        }
        finally {
            this._installing = false;
            this.$drop.disabled = false;
        }
    }

    _installSuccess() {
        this.dispatchEvent(new CustomEvent('notification', { detail: new Notify({
            title: _('Module installed successfully'),
            message: '',
            duration: 3000,
            type: 'success'
        }), bubbles: true}));
        this.dispatchEvent(new CustomEvent('close'));
    }

    _installFailure(error) {
        this.dispatchEvent(new CustomEvent('notification', { detail: new Notify({
            message: error.message,
            title: _('Unable to install module'),
            duration: 4000,
            type: 'error'
        }), bubbles: true}));
    }
}

customElements.define('jmv-sideload', PageSideload);

export default PageSideload;
