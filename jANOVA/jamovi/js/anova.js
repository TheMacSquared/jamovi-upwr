'use strict';

// Conditional enabling in the ANOVA panel: Welch needs a factors-only model;
// interaction-related controls need at least two factors.

const count = function(ctrl) {
    let v = ctrl.value();
    return Array.isArray(v) ? v.length : (v ? 1 : 0);
};

const refresh = function(ui) {
    let nFactors = count(ui.factors);
    let extra = count(ui.blocks) + count(ui.covs);
    let factorial = nFactors >= 2;
    ui.welch.setPropertyValue('enable', nFactors >= 1 && extra === 0);
    ui.nonpar.setPropertyValue('enable', nFactors >= 1 && extra === 0);
    ui.interactions.setPropertyValue('enable', factorial);
    ui.phInter.setPropertyValue('enable', factorial && ui.interactions.value());
    ui.plotInteraction.setPropertyValue('enable', factorial);
};

module.exports = {
    view_updated: refresh,
    factors_changed: refresh,
    blocks_changed: refresh,
    covs_changed: refresh,
    interactions_changed: refresh
};
