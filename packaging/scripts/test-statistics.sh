#!/bin/bash
# Compile and test source copies so jmc never changes the checkout.
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/jupwr-statistics.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT
mkdir -p "$WORK/library" "$WORK/modules" "$WORK/source" "$WORK/app/bin"
# Linux jmc locates a launcher even with --rhome; --assume-app-version means
# it is not executed. Use the real project launcher for this compile-only home.
cp "$ROOT/platform/jamovi" "$WORK/app/bin/jamovi"
R CMD INSTALL --library="$WORK/library" "$ROOT/jmvcore"
RHOME="$(R RHOME)"
RLIBS="$(Rscript -e 'cat(paste(.libPaths(), collapse=.Platform$path.sep))')"
for module in jperm jCI jRegr; do
    mkdir -p "$WORK/source/$module"
    # Only sources; stale generated headers/build directories must not affect CI.
    cp "$ROOT/$module/DESCRIPTION" "$WORK/source/$module/"
    cp -R "$ROOT/$module/R" "$ROOT/$module/jamovi" "$WORK/source/$module/"
    find "$WORK/source/$module/R" -name '*.h.R' -delete
    node "$ROOT/jamovi-compiler/index.js" --install "$WORK/source/$module" \
        --to "$WORK/modules" --home "$WORK/app" --rhome "$RHOME" --rlibs "$WORK/library:$RLIBS" \
        --assume-app-version "$(cat "$ROOT/version")" --skip-deps
done
Rscript "$ROOT/packaging/scripts/test-statistics.R" "$ROOT" "$WORK"
