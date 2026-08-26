
'use strict';

import RibbonMenu from './ribbonmenu';
import RibbonTab, { RibbonItem } from './ribbontab';
import Placeholder from './placeholder';
import interactionManager from '../../common/interactionmanager';
import { Modules } from '../modules';
import Settings from '../settings';
import { RibbonModel } from '../ribbon';
import Store from '../store';

//import Store from '../store';

class PlotsTab extends RibbonTab {
    buttons: RibbonItem[] = [ ];
    settings: Settings;
    modules: Modules;
    _moduleCount = 0;
    _analysesList = { };

    constructor(modules: Modules, model: RibbonModel, public store: Store) {
        super('plots', 'P', _('Plots'));
        this.modules = modules;
        this.settings = model.settings();

        this.modules.on('moduleVisibilityChanged', this._onModuleVisibilityChanged, this);
        this.modules.on('modulesChanged', this.update, this);

        this.populate();
    }

    private _onModuleVisibilityChanged(module) {
        if (module.visible)
            this._showModule(module.name);
        else
            this._hideModule(module.name);
    }

    private _hideModule(name) {
        for (let i = 0; i < this.buttons.length; i++) {
            let button = this.buttons[i];
            if (button instanceof RibbonMenu)
                button.hideModule(name);
        }
    }

    private _showModule(name: string) {
        for (let i = 0; i < this.buttons.length; i++) {
            let button = this.buttons[i];
            if (button instanceof RibbonMenu)
                button.showModule(name);
        }
    }

    protected override async getRibbonItems() {
        this.buttons = [ ];
        if ( ! this.modules)
            return this.buttons;

        let moduleList = [];
        this._analysesList = { };
        this._moduleCount = 0;
        let modules = this.modules.get('modules');
        for (let module of modules) {
            let _translate = await module.getTranslator;
            if (module.analyses.length > 0) {
                if (this._analysesList[module.name] === undefined) {
                    this._analysesList[module.name] = { version: module.version, analyses: [] };
                    this._moduleCount += 1;
                }
                let subtitle = module.title;
                // This regex is used to trim off any leading shortname (as well as seperators) from the title
                // E.G The module title 'GAMLj - General Analyses for Linear Models' will be trimmed to 'General Analyses for Linear Models'.
                let re = new RegExp('^' + module.name + '([ :-]{1,3})', 'i');
                subtitle = subtitle.replace(re, '');
                let moduleItem = { name : module.name, title : _translate(module.name), subtitle: _translate(subtitle), ns : 'installed', type: 'module', checked: module.visible  };
                let analyses = { name: 'plots', title: _('Plots'), type: 'group', items: [ ] };
                for (let analysis of module.analyses) {
                    if (analysis.category === 'plots') {
                        this._analysesList[module.name].analyses.push(analysis.name);
                        let analysisItem = {
                            name: analysis.name,
                            ns: analysis.ns,
                            title: _translate(analysis.menuTitle),
                            subtitle: _translate(analysis.menuSubtitle),
                            moduleName: module.name,
                            resultsTitle: _translate(analysis.title)
                        };
                        analyses.items.push(analysisItem);
                    }
                }
                if (analyses.items.length > 0) {
                    moduleItem.analyses = analyses;
                    moduleList.push(moduleItem);
                }
                else {
                    delete this._analysesList[module.name];
                    this._moduleCount -= 1;
                }
            }
        }

        let buttonId = interactionManager.nextAriaId('button');
        let  button = new RibbonMenu(_('Modules'), 'modules', 'M', [
            { name : 'modules', title : _('jamovi library'), ns : 'app' },
            { name : 'manageMods', title : _('Manage installed'), ns : 'app' },
            { name: 'installedList', title: _('Installed Modules'), type: 'group', items: moduleList }
        ], true, false);
        button.setAttribute('id', buttonId);
        button.classList.add('jmv-modules-menu-item');
        button.style.position = 'sticky';
        button.style.insetInlineEnd =  '0px';
        button.style.insetInlineStart =  '0px';
        this.buttons.push(button);

        let menus = { };
        let lastSub = null;

        for (let module of modules) {
            let _translate = await module.getTranslator;
            let isNew = module.new;
            for (let analysis of module.analyses) {
                if (analysis.category !== 'plots')
                    continue;

                let groupName = analysis.menuGroup;
                if (groupName === '.' || groupName === 'More' || groupName === 'Other plots')
                    groupName = 'Other';
                let subgroup = analysis.menuSubgroup;
                let menu = groupName in menus ? menus[groupName] : { _title: _translate(groupName) };
                if (analysis.ns === 'jmv' || menu.ns !== 'jmv')
                    menu.ns = analysis.ns;

                menu._new = isNew;
                let submenu = { name };
                if (subgroup in menu)
                    submenu = menu[subgroup];
                else
                    submenu = { name: subgroup, title: _translate(subgroup), items: [ ] };
                let item = {
                    name: analysis.name,
                    ns: analysis.ns,
                    title: _translate(analysis.menuTitle),
                    subtitle: _translate(analysis.menuSubtitle),
                    moduleName: module.name,
                    new: isNew,
                    hidden: module.visible === false,
                    resultsTitle: analysis.title
                };
                submenu.items.push(item);
                menu[subgroup] = submenu;
                menus[groupName] = menu;
            }
        }

        // fixed didactic ordering of the plot categories; groups not listed
        // here (from third-party modules) sort in just before 'Other'
        const groupOrder = [ 'Distribution', 'Comparison', 'Ranking', 'Correlation', 'Evolution', 'Composition', 'Other' ];
        let groupNames = Object.keys(menus).sort((a, b) => {
            let ai = groupOrder.indexOf(a);
            let bi = groupOrder.indexOf(b);
            if (ai === -1) ai = groupOrder.length - 1.5;
            if (bi === -1) bi = groupOrder.length - 1.5;
            return ai - bi;
        });

        for (let groupName of groupNames) {
            let menu = menus[groupName];
            let flattened = [ ];
            let containsNew = menu._new;
            for (let subgroup in menu) {
                if (subgroup === '_new' || subgroup === '_title' || subgroup === 'ns')
                    continue;
                flattened.push({
                    name: subgroup,
                    title: menu[subgroup].title,
                    type: 'group',
                    items: menu[subgroup].items });
            }

            if (flattened.length > 0 && flattened[0].name === '') {
                let items = flattened.shift().items;
                flattened = items.concat(flattened);
            }

            let button = new RibbonMenu(menu._title, groupName, null, flattened, false, containsNew);
            this.buttons.push(button);
        }

        if (this.settings.attributes.settingsRecieved === false) {
            this.buttons.push(new Placeholder('exploration', _('Exploration')));
            this.buttons.push(new Placeholder('t-tests', _('T-Tests')));
            this.buttons.push(new Placeholder('anova', _('ANOVA')));
            this.buttons.push(new Placeholder('regression', _('Regression')));
            this.buttons.push(new Placeholder('frequencies', _('Frequencies')));
            this.buttons.push(new Placeholder('factor', _('Factor')));
        }

        return this.buttons;
    }

    private _analysisSelected(analysis: { name: string, ns: string, title: string, checked?: boolean }) {
       if (analysis.name === 'modules' && analysis.ns === 'app')
            this.store.show(1, 'plot::');
        else if (analysis.name === 'manageMods' && analysis.ns === 'app')
            this.store.show(0, 'plot::');
        else if (analysis.ns === 'installed')
            this.modules.setModuleVisibility(analysis.name, analysis.checked);
        else
            this.emit('analysisSelected', analysis);
    }
}

export default PlotsTab;
