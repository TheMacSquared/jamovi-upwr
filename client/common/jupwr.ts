'use strict';

// Single source of truth for the jUPWR distribution version.
// Independent of the upstream jamovi version (which lives in the root `version`
// file and is shown separately in the UI). Bump on each jUPWR release:
//   MAJOR — curriculum-defining / breaking changes
//   MINOR — new modules or features
//   PATCH — fixes
export const JUPWR_VERSION = '0.5.2';
