'use strict';

// Conditional enabling of controls in the ANOVA panel: one-factor tests
// (Welch, Kruskal-Wallis, Jonckheere, median) only make sense for a single
// factor without blocks/covariates; ART and interaction cells need 2+ factors.

const count = function(ctrl) {
    let v = ctrl.value();
    return Array.isArray(v) ? v.length : (v ? 1 : 0);
};

const refresh = function(ui) {
    let nFactors = count(ui.factors);
    let extra = count(ui.blocks) + count(ui.covs);
    let oneFactor = nFactors === 1 && extra === 0;
    let factorial = nFactors >= 2;

    ui.welch.setPropertyValue('enable', nFactors >= 1 && extra === 0);
    ui.kruskal.setPropertyValue('enable', oneFactor);
    ui.jonckheere.setPropertyValue('enable', oneFactor);
    ui.medianTest.setPropertyValue('enable', oneFactor);
    ui.npPostHoc.setPropertyValue('enable', oneFactor && ui.kruskal.value());

    ui.interactions.setPropertyValue('enable', factorial);
    ui.art.setPropertyValue('enable', factorial && extra === 0);
    ui.phInter.setPropertyValue('enable', factorial && ui.interactions.value());
    ui.plotInteraction.setPropertyValue('enable', factorial);
};

module.exports = {
    view_updated: refresh,
    factors_changed: refresh,
    blocks_changed: refresh,
    covs_changed: refresh,
    kruskal_changed: refresh,
    interactions_changed: refresh
};
