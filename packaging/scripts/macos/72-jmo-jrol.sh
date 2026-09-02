#!/bin/bash
# Faza H — jRol jako moduł OPCJONALNY (doświadczalnictwo rolnicze): budowa pliku .jmo do sideloadu.
# jRol nie jest preinstalowany w jUPWR (obsługuje jeden kurs — doświadczalnictwo rolnicze);
# studenci tego kursu instalują go przez Moduły → Sideload. Dzięki temu poprawki
# w jRol nie wymagają reinstalacji całej aplikacji.
# UWAGA: .jmo zawiera pakiet R skompilowany pod platformę hosta (tu: macOS arm64);
# wersję dla Windows buduje packaging/scripts/windows/build.ps1 (krok 4g).
. "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

JMC="$REPO_ROOT/jamovi-compiler/index.js"
BASE_R="$PAYLOAD/modules/base/R"
[ -d "$BASE_R/jmvcore" ] || die "Brak jmvcore w $BASE_R — uruchom najpierw 20-modules.sh"
[ -d "$REPO_ROOT/jamovi-compiler/node_modules" ] || ( cd "$REPO_ROOT/jamovi-compiler" && npm install )

# wersja modułu: jmc czyta ją wyłącznie z jamovi/0000.yaml (index.js:299-306)
JRISK_VERSION="$(grep -m1 -oE '^version: *[0-9.]+' "$REPO_ROOT/jRol/jamovi/0000.yaml" | grep -oE '[0-9.]+')"
# Strażnik: 0000.yaml nadpisany przez wcześniejszy przebieg jmc (--patch-version)
# dałby wersję aplikacji zamiast wersji modułu, a więc .jmo o złej nazwie.
JRISK_DESC="$(grep -m1 -oE '^Version: *[0-9.]+' "$REPO_ROOT/jRol/DESCRIPTION" | grep -oE '[0-9.]+')"
[ "${JRISK_VERSION}" = "$JRISK_DESC" ] || die "jRol: 0000.yaml ma wersję ${JRISK_VERSION}, DESCRIPTION $JRISK_DESC — 0000.yaml jest nadpisany przez jmc (przywróć: git checkout -- jRol/jamovi/0000.yaml)"

JMO="$DIST/jRol_${JRISK_VERSION}-macos-arm64.jmo"
mkdir -p "$DIST"

src_guard jRol   # jmc nadpisuje pliki źródłowe — trap przywraca je po buildzie

log "jmc --build jRol $JRISK_VERSION -> $JMO ..."
node "$JMC" --build "$REPO_ROOT/jRol" \
    --jmo "$JMO" \
    --rhome "$R_HOME_SYS" \
    --rlibs "$BASE_R" \
    --assume-app-version "$JAMOVI_VERSION" \
    --skip-deps
[ -f "$JMO" ] || die "Plik .jmo nie powstał"
log "OK — $JMO ($(du -h "$JMO" | cut -f1)). Instalacja w jUPWR: Moduły → Sideload."
