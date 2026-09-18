#!/bin/bash
# Optional measurement uncertainty module; not installed into the base payload.
. "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

JMC="$REPO_ROOT/jamovi-compiler/index.js"
BASE_R="$PAYLOAD/modules/base/R"
[ -d "$BASE_R/jmvcore" ] || die "Brak jmvcore w $BASE_R — uruchom najpierw 20-modules.sh"
[ -d "$REPO_ROOT/jamovi-compiler/node_modules" ] || ( cd "$REPO_ROOT/jamovi-compiler" && npm install )

POMIAR_VERSION="$(sed -n 's/^version: *//p' "$REPO_ROOT/jPomiar/jamovi/0000.yaml")"
POMIAR_DESC="$(sed -n 's/^Version: *//p' "$REPO_ROOT/jPomiar/DESCRIPTION")"
[ -n "$POMIAR_VERSION" ] && [ "$POMIAR_VERSION" = "$POMIAR_DESC" ] || die "jPomiar: wersje 0000.yaml i DESCRIPTION różnią się"
JMO="$DIST/jPomiar_${POMIAR_VERSION}-macos-arm64.jmo"
mkdir -p "$DIST"
src_guard jPomiar
[ ! -f "$JMO" ] || rm "$JMO"

log "jmc --build jPomiar $POMIAR_VERSION -> $JMO ..."
node "$JMC" --build "$REPO_ROOT/jPomiar" \
    --jmo "$JMO" --rhome "$R_HOME_SYS" --rlibs "$BASE_R" \
    --assume-app-version "$JAMOVI_VERSION" --skip-deps
[ -f "$JMO" ] || die "Plik .jmo nie powstał"
log "OK — $JMO. Instalacja: Moduły → Sideload."
