'use strict';

// Single source of truth for the jUPWR distribution version.
// Independent of the upstream jamovi version (which lives in the root `version`
// file and is shown separately in the UI). Bump on each jUPWR release:
//   MAJOR — curriculum-defining / breaking changes
//   MINOR — new modules or features
//   PATCH — fixes
export const JUPWR_VERSION = '0.9.2.2';

// Upstream jmv analyses hidden from the ribbon menus in jUPWR. The analyses
// still exist (files saved elsewhere open and re-run), they are only not
// offered from the menu because a jUPWR module replaces them:
//   jANOVA (ANOVA, ANOVA powtórzonych pomiarów) replaces the jmv ANOVA family.
// Key: `${ns}::${name}`.
export const JUPWR_HIDDEN_ANALYSES: ReadonlySet<string> = new Set([
    'jmv::anovaonew',
    'jmv::anova',
    'jmv::ancova',
    'jmv::anovarm',
]);
