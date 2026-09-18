#!/bin/bash
# jPomiar jako moduł OPCJONALNY (pomiary i niepewność): budowa pliku .jmo do sideloadu.
# jPomiar nie jest preinstalowany w jUPWR (pilotaż międzykierunkowy); instalacja przez
# Moduły → Sideload, więc poprawki w module nie wymagają reinstalacji całej aplikacji.
# UWAGA: .jmo zawiera pakiet R skompilowany pod platformę hosta (tu: macOS arm64);
# wersję dla Windows buduje packaging/scripts/windows/build.ps1 (krok 4h),
# a dla Linuksa packaging/scripts/build-jpomiar-docker.sh.
. "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

JMC="$REPO_ROOT/jamovi-compiler/index.js"
BASE_R="$PAYLOAD/modules/base/R"
[ -d "$BASE_R/jmvcore" ] || die "Brak jmvcore w $BASE_R — uruchom najpierw 20-modules.sh"
[ -d "$REPO_ROOT/jamovi-compiler/node_modules" ] || ( cd "$REPO_ROOT/jamovi-compiler" && npm install )

# wersja modułu: jmc czyta ją wyłącznie z jamovi/0000.yaml
POMIAR_VERSION="$(sed -n 's/^version: *//p' "$REPO_ROOT/jPomiar/jamovi/0000.yaml")"
# Strażnik: 0000.yaml nadpisany przez wcześniejszy przebieg jmc (--patch-version)
# dałby wersję aplikacji zamiast wersji modułu, a więc .jmo o złej nazwie.
POMIAR_DESC="$(sed -n 's/^Version: *//p' "$REPO_ROOT/jPomiar/DESCRIPTION")"
[ -n "$POMIAR_VERSION" ] && [ "$POMIAR_VERSION" = "$POMIAR_DESC" ] || die "jPomiar: 0000.yaml ma wersję ${POMIAR_VERSION}, DESCRIPTION $POMIAR_DESC — 0000.yaml jest nadpisany przez jmc (przywróć: git checkout -- jPomiar/jamovi/0000.yaml)"
JMO="$DIST/jPomiar_${POMIAR_VERSION}-macos-arm64.jmo"
mkdir -p "$DIST"
src_guard jPomiar   # jmc nadpisuje pliki źródłowe — trap przywraca je po buildzie
[ ! -f "$JMO" ] || rm "$JMO"

log "jmc --build jPomiar $POMIAR_VERSION -> $JMO ..."
node "$JMC" --build "$REPO_ROOT/jPomiar" \
    --jmo "$JMO" --rhome "$R_HOME_SYS" --rlibs "$BASE_R" \
    --assume-app-version "$JAMOVI_VERSION" --skip-deps
[ -f "$JMO" ] || die "Plik .jmo nie powstał"
log "OK — $JMO. Instalacja: Moduły → Sideload."
