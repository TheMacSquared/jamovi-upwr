'use strict';

// Conditional enabling in the repeated-measures panel: Friedman/Page need a
// single within factor and nothing else; ART and interaction cells need 2+
// factors (within + between) and no covariates.

const count = function(ctrl) {
    let v = ctrl.value();
    return Array.isArray(v) ? v.length : (v ? 1 : 0);
};

const refresh = function(ui) {
    let nWithin = count(ui.within);
    let nBetween = count(ui.between);
    let nCovs = count(ui.covs);
    let oneWithin = nWithin === 1 && nBetween === 0 && nCovs === 0;
    let factorial = nWithin + nBetween >= 2;

    ui.friedman.setPropertyValue('enable', oneWithin);
    ui.page.setPropertyValue('enable', oneWithin);
    ui.npPostHoc.setPropertyValue('enable', oneWithin && ui.friedman.value());
    ui.art.setPropertyValue('enable', factorial && nCovs === 0);
    ui.phBetween.setPropertyValue('enable', nBetween >= 1);
    ui.phInter.setPropertyValue('enable', factorial);
    ui.plotInteraction.setPropertyValue('enable', factorial);
};

module.exports = {
    view_updated: refresh,
    within_changed: refresh,
    between_changed: refresh,
    covs_changed: refresh,
    friedman_changed: refresh
};
