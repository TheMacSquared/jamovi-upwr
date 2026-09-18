#!/bin/bash
# Compile a clean source copy and run calculations plus integration tests.
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/jupwr-pomiar.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT
mkdir -p "$WORK/library" "$WORK/modules" "$WORK/source/jPomiar" "$WORK/app/bin"
cp "$ROOT/platform/jamovi" "$WORK/app/bin/jamovi"
R CMD INSTALL --library="$WORK/library" "$ROOT/jmvcore"
RHOME="$(R RHOME)"
RLIBS="$(Rscript -e 'cat(paste(.libPaths(), collapse=.Platform$path.sep))')"
cp "$ROOT/jPomiar/DESCRIPTION" "$ROOT/jPomiar/NAMESPACE" "$WORK/source/jPomiar/"
cp -R "$ROOT/jPomiar/R" "$ROOT/jPomiar/jamovi" "$ROOT/jPomiar/data" "$WORK/source/jPomiar/"
find "$WORK/source/jPomiar/R" -name '*.h.R' -delete
node "$ROOT/jamovi-compiler/index.js" --install "$WORK/source/jPomiar" \
    --to "$WORK/modules" --home "$WORK/app" --rhome "$RHOME" \
    --rlibs "$WORK/library:$RLIBS" --assume-app-version "$(cat "$ROOT/version")" --skip-deps
Rscript - "$ROOT" "$WORK" <<'RSCRIPT'
args <- commandArgs(trailingOnly = TRUE)
.libPaths(c(file.path(args[2], "library"), file.path(args[2], "modules", "jPomiar", "R"), .libPaths()))
loadNamespace("jPomiar")
testthat::test_dir(file.path(args[1], "jPomiar", "tests", "testthat"),
                   reporter = "summary", stop_on_failure = TRUE)
RSCRIPT
