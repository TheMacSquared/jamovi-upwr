#!/bin/bash
# Faza H — jRISK jako moduł OPCJONALNY: budowa pliku .jmo do sideloadu.
# jRISK nie jest preinstalowany w jUPWR (obsługuje jeden kurs — analizę ryzyka);
# studenci tego kursu instalują go przez Moduły → Sideload. Dzięki temu poprawki
# w jRISK nie wymagają reinstalacji całej aplikacji.
# UWAGA: .jmo zawiera pakiet R skompilowany pod platformę hosta (tu: macOS arm64);
# wersję dla Windows buduje packaging/scripts/windows/build.ps1 (krok 4e).
. "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

JMC="$REPO_ROOT/jamovi-compiler/index.js"
BASE_R="$PAYLOAD/modules/base/R"
[ -d "$BASE_R/jmvcore" ] || die "Brak jmvcore w $BASE_R — uruchom najpierw 20-modules.sh"
[ -d "$REPO_ROOT/jamovi-compiler/node_modules" ] || ( cd "$REPO_ROOT/jamovi-compiler" && npm install )

JRISK_VERSION="$(grep -oE '^Version: *[0-9.]+' "$REPO_ROOT/jRISK/DESCRIPTION" | grep -oE '[0-9.]+')"
JMO="$DIST/jRISK_${JRISK_VERSION}-macos-arm64.jmo"
mkdir -p "$DIST"

log "jmc --build jRISK $JRISK_VERSION -> $JMO ..."
node "$JMC" --build "$REPO_ROOT/jRISK" \
    --jmo "$JMO" \
    --rhome "$R_HOME_SYS" \
    --rlibs "$BASE_R" \
    --assume-app-version "$JAMOVI_VERSION" \
    --skip-deps
[ -f "$JMO" ] || die "Plik .jmo nie powstał"
log "OK — $JMO ($(du -h "$JMO" | cut -f1)). Instalacja w jUPWR: Moduły → Sideload."
