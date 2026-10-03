'use strict';

// Conditional enabling in the ANOVA panel: Welch needs a factors-only model;
// interaction-related controls need at least two factors.
//
// Post-hoc list follows the variance assumption: with Welch and one factor
// all pairs are compared by Games-Howell instead of Tukey (R decides the same
// way from the options, see phEffectiveMethod in R/utils.R).

const POST_HOC_EQUAL = [
    { name: 'tukey', title: 'Tukeya (HSD)' },
    { name: 'dunnett', title: 'Dunnetta (vs kontrola)' },
    { name: 'none', title: 'Brak (tylko średnie)' }
];
const POST_HOC_UNEQUAL = [
    { name: 'gamesHowell', title: 'Gamesa-Howella' },
    { name: 'dunnett', title: 'Dunnetta (vs kontrola)' },
    { name: 'none', title: 'Brak (tylko średnie)' }
];
const SWAP = { tukey: 'gamesHowell', gamesHowell: 'tukey' };

const count = function(ctrl) {
    let v = ctrl.value();
    return Array.isArray(v) ? v.length : (v ? 1 : 0);
};

const refresh = function(ui) {
    let nFactors = count(ui.factors);
    let extra = count(ui.blocks) + count(ui.covs);
    let factorial = nFactors >= 2;
    let factorsOnly = extra === 0;
    let oneFactor = nFactors === 1 && factorsOnly;
    ui.welch.setPropertyValue('enable', nFactors >= 1 && factorsOnly);
    ui.nonpar.setPropertyValue('enable', nFactors >= 1 && factorsOnly);
    ui.interactions.setPropertyValue('enable', factorial);
    ui.phInter.setPropertyValue('enable', factorial && ui.interactions.value());
    ui.plotInteraction.setPropertyValue('enable', factorial);

    let welch = ui.welch.value() && factorsOnly;
    let nonpar = ui.nonpar.value() && factorsOnly;
    let list = welch && oneFactor ? POST_HOC_UNEQUAL : POST_HOC_EQUAL;
    ui.postHoc.setPropertyValue('options', list);
    let current = ui.postHoc.value();
    if ( ! list.some(o => o.name === current) && SWAP[current])
        ui.postHoc.setValue(SWAP[current]);
    // unused: Dunn's test replaces it (nonparametric, one factor); no pairwise
    // comparisons under Welch with several factors unless ART needs the list
    let unused = (nonpar && oneFactor) || (welch && factorial && ! nonpar);
    ui.postHoc.setPropertyValue('enable', ! unused);
    ui.plotMeans.setPropertyValue('enable', ! (nonpar && oneFactor));
};

module.exports = {
    view_updated: refresh,
    factors_changed: refresh,
    blocks_changed: refresh,
    covs_changed: refresh,
    interactions_changed: refresh,
    welch_changed: refresh,
    nonpar_changed: refresh
};
