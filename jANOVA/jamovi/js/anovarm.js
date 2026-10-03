'use strict';

// Conditional enabling in the repeated-measures panel.

const count = function(ctrl) {
    let v = ctrl.value();
    return Array.isArray(v) ? v.length : (v ? 1 : 0);
};

const refresh = function(ui) {
    let nWithin = count(ui.within);
    let nBetween = count(ui.between);
    let nCovs = count(ui.covs);
    let factorial = nWithin + nBetween >= 2;
    ui.nonpar.setPropertyValue('enable', nWithin >= 1 && nCovs === 0);
    ui.phInter.setPropertyValue('enable', factorial);
    ui.plotInteraction.setPropertyValue('enable', factorial);
    // one within factor, nonparametric: Nemenyi's test replaces the list
    let friedman = ui.nonpar.value() && nCovs === 0 && nWithin === 1 && nBetween === 0;
    ui.postHoc.setPropertyValue('enable', ! friedman);
    ui.plotMeans.setPropertyValue('enable', ! friedman);
};

module.exports = {
    view_updated: refresh,
    within_changed: refresh,
    between_changed: refresh,
    covs_changed: refresh,
    nonpar_changed: refresh
};
