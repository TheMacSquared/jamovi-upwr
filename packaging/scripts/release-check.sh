#!/bin/bash
# Kontrola spójności wydania jUPWR — uruchamiaj przed commitem wydania.
# Sprawdza: zgodność wersji (jupwr.ts vs docker-compose), identyczność list modułów
# wbudowanych w Docker/macOS/Windows, wpis w CHANGELOG.md, obecność .jmo dla każdego
# modułu opcjonalnego w bieżącej wersji (na tej platformie) i wiersz w macierzy MODULES.md.
set -u
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT"
warn=0
ok()   { printf '  \033[32mOK\033[0m   %s\n' "$1"; }
bad()  { printf '  \033[31mUWAGA\033[0m %s\n' "$1"; warn=$((warn+1)); }

JUPWR="$(grep -oE "JUPWR_VERSION *= *'[0-9.]+'" client/common/jupwr.ts | grep -oE '[0-9]+(\.[0-9]+)+')"
COMPOSE="$(grep -oE 'image: jupwr/jupwr:[0-9.]+' docker-compose.yaml | grep -oE '[0-9]+(\.[0-9]+)+')"
echo "jUPWR $JUPWR (jamovi $(cat version))"
[ "$JUPWR" = "$COMPOSE" ] && ok "docker-compose tag = $COMPOSE" || bad "docker-compose tag ($COMPOSE) != jupwr.ts ($JUPWR)"

# listy modułów wbudowanych w trzech buildach
DOCKER="$(grep -oE '^COPY \$JAMOVI_ROOT/[A-Za-z]+/ /tmp/source/' docker/jamovi-Dockerfile | grep -oE 'ROOT/[A-Za-z]+' | cut -d/ -f2 | grep -vE '^(server|client|engine|jmvcore|jamovi-compiler|readstat|platform|version|i18n)$' | sort | tr '\n' ' ')"
MAC="$(grep -oE '^MODULES=\([^)]*\)' packaging/scripts/macos/20-modules.sh | sed 's/MODULES=(//;s/)//' | tr ' ' '\n' | sort | tr '\n' ' ')"
WIN="$(grep -oE "^\\\$Modules *= *@\([^)]*\)" packaging/scripts/windows/build.ps1 | grep -oE "'[A-Za-z]+'" | tr -d "'" | sort | tr '\n' ' ')"
if [ "$DOCKER" = "$MAC" ] && [ "$MAC" = "$WIN" ]; then ok "wbudowane (Docker = macOS = Windows): $MAC"
else bad "listy wbudowanych różnią się — Docker: [$DOCKER] macOS: [$MAC] Windows: [$WIN]"; fi

grep -qE "^## $JUPWR( |$)" CHANGELOG.md && ok "CHANGELOG.md ma wpis $JUPWR" || bad "brak wpisu '## $JUPWR' w CHANGELOG.md"

# spójność wersji: jmc stempluje moduł wersją z jamovi/0000.yaml i nie zagląda
# do DESCRIPTION — rozjazd oznacza, że .jmo/moduł ma inną wersję niż deklarujemy
for d in */jamovi/0000.yaml; do
    m="${d%%/*}"
    [ -f "$m/DESCRIPTION" ] || continue
    vy="$(grep -m1 -oE '^version: *[0-9.]+' "$d" | grep -oE '[0-9.]+')"
    vd="$(grep -m1 '^Version:' "$m/DESCRIPTION" | grep -oE '[0-9.]+')"
    [ "$vy" = "$vd" ] && ok "$m: wersja $vy (0000.yaml = DESCRIPTION)" \
        || bad "$m: rozjazd wersji — 0000.yaml=$vy, DESCRIPTION=$vd (jmc użyje $vy)"
done

# moduły opcjonalne = katalogi z jamovi/0000.yaml spoza listy wbudowanych
case "$(uname -s)" in Darwin) PLAT="macos-arm64";; MINGW*|MSYS*|CYGWIN*) PLAT="win64";; *) PLAT="linux";; esac
for d in */jamovi/0000.yaml; do
    m="${d%%/*}"
    [ -f "$m/DESCRIPTION" ] || continue
    echo " $MAC " | grep -q " $m " && continue
    # jmc czyta wersję modułu wyłącznie z jamovi/0000.yaml (index.js:285-290);
    # DESCRIPTION jest tylko dla R CMD i musi się z nią zgadzać
    v="$(grep -m1 -oE '^version: *[0-9.]+' "$d" | grep -oE '[0-9.]+')"
    jmo="packaging/build/dist/${m}_${v}-${PLAT}.jmo"
    echo "moduł opcjonalny: $m $v"
    [ -f "$jmo" ] && ok "$jmo" || bad "brak $jmo — zbuduj (macOS: 70-jmo-*.sh, Windows: build.ps1)"
    grep -qE "^\| *$JUPWR *\|.*\| *$v *\|" packaging/MODULES.md && ok "MODULES.md: wiersz $JUPWR / $m $v" \
        || bad "MODULES.md: brak wiersza macierzy dla jUPWR $JUPWR z $m $v"
done

echo; [ $warn -eq 0 ] && echo "Wydanie spójne." || { echo "Ostrzeżeń: $warn"; exit 1; }
