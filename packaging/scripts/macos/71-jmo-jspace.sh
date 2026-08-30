#!/bin/bash
# Faza H — jSpace jako moduł OPCJONALNY: budowa pliku .jmo do sideloadu.
# jSpace nie jest preinstalowany w jUPWR (obsługuje jeden kurs — statystykę
# dla inżynierii danych satelitarnych i kosmicznych); studenci instalują go
# przez Moduły → Sideload.
# UWAGA: budujemy BEZ --skip-deps — jmc doinstalowuje sf/terra/asteRisk (wraz
# z zależnościami) do jSpace/build/R<ver>-macos z przypiętego snapshotu CRAN
# i pakuje je do .jmo, więc moduł jest samowystarczalny po sideloadzie.
# Binaria CRAN sf/terra mają wbudowane GDAL/GEOS/PROJ; compilerr.js przepina
# dyliby przez install_name_tool. buildDir jest cache'owany między buildami.
# .jmo zawiera pakiet R skompilowany pod platformę hosta (tu: macOS arm64);
# wersję dla Windows buduje packaging/scripts/windows/build.ps1 (krok 4f).
. "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

JMC="$REPO_ROOT/jamovi-compiler/index.js"
BASE_R="$PAYLOAD/modules/base/R"
[ -d "$BASE_R/jmvcore" ] || die "Brak jmvcore w $BASE_R — uruchom najpierw 20-modules.sh"
[ -d "$REPO_ROOT/jamovi-compiler/node_modules" ] || ( cd "$REPO_ROOT/jamovi-compiler" && npm install )

# wersja modułu: jmc czyta ją wyłącznie z jamovi/0000.yaml (index.js:285-290)
JSPACE_VERSION="$(grep -m1 -oE '^version: *[0-9.]+' "$REPO_ROOT/jSpace/jamovi/0000.yaml" | grep -oE '[0-9.]+')"
JMO="$DIST/jSpace_${JSPACE_VERSION}-macos-arm64.jmo"
mkdir -p "$DIST"

log "jmc --build jSpace $JSPACE_VERSION -> $JMO (bez --skip-deps: bundluje sf/terra/asteRisk) ..."
node "$JMC" --build "$REPO_ROOT/jSpace" \
    --jmo "$JMO" \
    --rhome "$R_HOME_SYS" \
    --rlibs "$BASE_R" \
    --assume-app-version "$JAMOVI_VERSION"
[ -f "$JMO" ] || die "Plik .jmo nie powstał"
log "OK — $JMO ($(du -h "$JMO" | cut -f1)). Instalacja w jUPWR: Moduły → Sideload."
