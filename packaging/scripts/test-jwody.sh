#!/bin/bash
# Validate a clean source copy and generated jamovi contracts without packaging.
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/jupwr-wody-tests.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT
mkdir -p "$WORK/jWody"
cp "$ROOT/jWody/DESCRIPTION" "$ROOT/jWody/NAMESPACE" "$WORK/jWody/"
cp -R "$ROOT/jWody/R" "$ROOT/jWody/jamovi" "$ROOT/jWody/data" "$WORK/jWody/"
find "$WORK/jWody/R" -name '*.h.R' -delete
BASE_R="${JWODY_RLIBS:-$ROOT/packaging/build/stage/jamovi/modules/base/R}"
export R_LIBS="$BASE_R${R_LIBS:+:$R_LIBS}"
Rscript -e 'stopifnot(requireNamespace("jmvcore",quietly=TRUE),requireNamespace("testthat",quietly=TRUE),requireNamespace("ggplot2",quietly=TRUE),requireNamespace("ragg",quietly=TRUE))'
node "$ROOT/jamovi-compiler/index.js" --prepare "$WORK/jWody" \
    --rhome "$(R RHOME)" --rlibs "$BASE_R" --assume-app-version "$(cat "$ROOT/version")"
export JWODY_SOURCE_TESTS=1 JWODY_ROOT="$WORK/jWody"
Rscript - "$ROOT" <<'RSCRIPT'
root <- commandArgs(trailingOnly=TRUE)[1]
testthat::test_dir(file.path(root,"jWody","tests","testthat"),reporter="summary",stop_on_failure=TRUE,stop_on_warning=TRUE)
RSCRIPT
