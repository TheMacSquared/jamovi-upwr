'use strict';

// Single source of truth for the jUPWR distribution version.
// Independent of the upstream jamovi version (which lives in the root `version`
// file and is shown separately in the UI). Bump on each jUPWR release:
//   MAJOR — curriculum-defining / breaking changes
//   MINOR — new modules or features
//   PATCH — fixes
export const JUPWR_VERSION = '0.9.5.5';

// Upstream jmv analyses hidden from the ribbon menus in jUPWR. The analyses
// still exist (files saved elsewhere open and re-run), they are only not
// offered from the menu because a jUPWR module replaces them:
//   jANOVA (ANOVA, ANOVA powtórzonych pomiarów) replaces the jmv ANOVA family,
//   including the non-parametric one-way (Kruskal-Wallis) and repeated-measures
//   (Friedman) analyses, which jANOVA offers as switches in the same panels.
// Key: `${ns}::${name}`.
export const JUPWR_HIDDEN_ANALYSES: ReadonlySet<string> = new Set([
    // NB: jmv analysis names are camelCase (name: in the .a.yaml), not file names.
    'jmv::anovaOneW',
    'jmv::anova',
    'jmv::ancova',
    'jmv::anovaRM',
    'jmv::anovaNP',
    'jmv::anovaRMNP',
    // jTestyT replaces the jmv t-tests.
    'jmv::ttestIS',
    'jmv::ttestPS',
    'jmv::ttestOneS',
    // jCzest replaces the jmv frequency analyses; jmv::logLinear stays visible
    // as the only tool for 3+ dimensional tables (the MANCOVA case).
    'jmv::contTables',
    'jmv::contTablesPaired',
    'jmv::propTest2',
    'jmv::propTestN',
]);

// Upstream analyses kept in the menu but listed after the jUPWR ones in the
// same submenu (the ribbon builds menus in module order, jmv first).
export const JUPWR_MENU_LAST: ReadonlySet<string> = new Set([
    'jmv::mancova',
]);
