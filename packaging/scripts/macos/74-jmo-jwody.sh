#!/bin/bash
# jWody jako moduł OPCJONALNY (hydrologia): budowa pliku .jmo do sideloadu.
# jWody nie jest preinstalowany w jUPWR (narzędzia hydrologiczne); instalacja przez
# Moduły → Sideload, więc poprawki w module nie wymagają reinstalacji całej aplikacji.
# UWAGA: .jmo zawiera pakiet R skompilowany pod platformę hosta (tu: macOS arm64);
# wersję dla Windows buduje packaging/scripts/windows/build.ps1 (krok 4i),
# a dla Linuksa packaging/scripts/build-jwody-docker.sh.
. "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

JMC="$REPO_ROOT/jamovi-compiler/index.js"
BASE_R="$PAYLOAD/modules/base/R"
[ -d "$BASE_R/jmvcore" ] || die "Brak jmvcore w $BASE_R — uruchom najpierw 20-modules.sh"
[ -d "$REPO_ROOT/jamovi-compiler/node_modules" ] || ( cd "$REPO_ROOT/jamovi-compiler" && npm install )

# wersja modułu: jmc czyta ją wyłącznie z jamovi/0000.yaml
WODY_VERSION="$(sed -n 's/^version: *//p' "$REPO_ROOT/jWody/jamovi/0000.yaml")"
# Strażnik: 0000.yaml nadpisany przez wcześniejszy przebieg jmc (--patch-version)
# dałby wersję aplikacji zamiast wersji modułu, a więc .jmo o złej nazwie.
WODY_DESC="$(sed -n 's/^Version: *//p' "$REPO_ROOT/jWody/DESCRIPTION")"
[ -n "$WODY_VERSION" ] && [ "$WODY_VERSION" = "$WODY_DESC" ] || die "jWody: 0000.yaml ma wersję ${WODY_VERSION}, DESCRIPTION $WODY_DESC — 0000.yaml jest nadpisany przez jmc (przywróć: git checkout -- jWody/jamovi/0000.yaml)"
JMO="$DIST/jWody_${WODY_VERSION}-macos-arm64.jmo"
mkdir -p "$DIST"
src_guard jWody   # jmc nadpisuje pliki źródłowe — trap przywraca je po buildzie
[ ! -f "$JMO" ] || rm "$JMO"

log "jmc --build jWody $WODY_VERSION -> $JMO ..."
node "$JMC" --build "$REPO_ROOT/jWody" \
    --jmo "$JMO" --rhome "$R_HOME_SYS" --rlibs "$BASE_R" \
    --assume-app-version "$JAMOVI_VERSION" --skip-deps
[ -f "$JMO" ] || die "Plik .jmo nie powstał"
log "OK — $JMO. Instalacja: Moduły → Sideload."
