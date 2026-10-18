#!/bin/bash
# Kontrola spójności wydania jUPWR — uruchamiaj przed commitem wydania.
# Sprawdza: zgodność wersji (jupwr.ts vs docker-compose), identyczność list modułów
# wbudowanych w Docker/macOS/Windows, wpis w CHANGELOG.md, obecność .jmo dla każdego
# modułu opcjonalnego w bieżącej wersji (na tej platformie) i wiersz w macierzy MODULES.md.
set -uo pipefail
check_artifacts=true
case "${1:-}" in
    --metadata-only) check_artifacts=false ;;
    "") ;;
    *) echo "Użycie: $0 [--metadata-only]" >&2; exit 2 ;;
esac
[ "$#" -le 1 ] || exit 2
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT"
warn=0
ok()   { printf '  \033[32mOK\033[0m   %s\n' "$1"; }
bad()  { printf '  \033[31mUWAGA\033[0m %s\n' "$1"; warn=$((warn+1)); }

JUPWR="$(grep -oE "JUPWR_VERSION *= *'[0-9.]+'" client/common/jupwr.ts | grep -oE '[0-9]+(\.[0-9]+)+')"
COMPOSE="$(grep -oE 'image: jupwr/jupwr:[0-9.]+' docker-compose.yaml | grep -oE '[0-9]+(\.[0-9]+)+')"
echo "jUPWR $JUPWR (jamovi $(cat version))"
[ -n "$JUPWR" ] && [ "$JUPWR" = "$COMPOSE" ] && ok "docker-compose tag = $COMPOSE" || bad "docker-compose tag ($COMPOSE) != jupwr.ts ($JUPWR)"

NSIS="$(awk '/^[[:space:]]*!define VERSION / {gsub(/"/, "", $3); print $3}' packaging/scripts/windows/jUPWR.nsi)"
[ -n "$JUPWR" ] && [ "$JUPWR" = "$NSIS" ] && ok "Windows NSIS = $NSIS" || bad "wersja NSIS ($NSIS) != jupwr.ts ($JUPWR)"

# linia wydań (packaging/CHANNELS.md): gałąź, seria wersji, jamovi, submoduły, poprawki ze stable
BRANCH="$(git rev-parse --abbrev-ref HEAD)"
SERIES="${JUPWR%.*}"
RELEASES="$(git branch -r --list 'origin/release/*' --format='%(refname:short)' | sed 's#^origin/release/##' | sort -V)"
# lokalna gałąź za origin (np. fetch w trakcie buildu): porównania z origin/release/*
# niżej dałyby fałszywe „bez odpowiednika”, a build nie obejmuje najnowszych commitów
if [ "$BRANCH" != HEAD ] && git rev-parse -q --verify "origin/$BRANCH" >/dev/null; then
    behind="$(git rev-list --count "HEAD..origin/$BRANCH")"
    [ "$behind" = 0 ] && ok "HEAD aktualny względem origin/$BRANCH" \
        || bad "HEAD za origin/$BRANCH o $behind commit(y/ów): git pull i powtórz build (porównania z origin/* niżej mogą fałszywie alarmować)"
fi
case "$BRANCH" in
    release/*)
        [ "$SERIES" = "${BRANCH#release/}" ] && ok "gałąź $BRANCH, wersja z serii $SERIES" \
            || bad "gałąź $BRANCH, ale wersja $JUPWR (oczekiwana seria ${BRANCH#release/}.x)" ;;
    main)
        low=""
        for s in $RELEASES; do
            top="$(printf '%s\n%s\n' "$s" "$SERIES" | sort -V | tail -1)"
            { [ "$top" != "$SERIES" ] || [ "$s" = "$SERIES" ]; } && low="$low $s"
        done
        [ -z "$low" ] && ok "gałąź main, seria $SERIES wyższa niż release/*" \
            || bad "gałąź main z wersją $JUPWR — seria nie wyższa niż release/:$low" ;;
    *) bad "gałąź $BRANCH nie jest linią wydań (main / release/*) — patrz packaging/CHANNELS.md" ;;
esac

JAMOVI="$(cat version)"
CH_JAMOVI="$(awk -F '|' -v b="\`$BRANCH\`" '
    {g=$3; gsub(/^[[:space:]]+|[[:space:]]+$/, "", g)}
    g == b {j=$5; gsub(/^[[:space:]]+|[[:space:]]+$/, "", j); print j; exit}' packaging/CHANNELS.md)"
case "$JAMOVI" in
    "$CH_JAMOVI".*) [ -n "$CH_JAMOVI" ] && ok "jamovi $JAMOVI zgodne z CHANNELS.md ($CH_JAMOVI)" \
        || bad "brak wiersza gałęzi $BRANCH w packaging/CHANNELS.md" ;;
    *) bad "jamovi $JAMOVI, a CHANNELS.md podaje dla $BRANCH: ${CH_JAMOVI:-brak wiersza}" ;;
esac

# wskaźnik submodułu musi leżeć na gałęzi, którą .gitmodules tej linii każe śledzić —
# inaczej świeży klon padnie, gdy tamta gałąź zostanie usunięta
for key in $(git config -f .gitmodules --name-only --get-regexp '^submodule\..*\.path$'); do
    name="${key#submodule.}"; name="${name%.path}"
    path="$(git config -f .gitmodules "submodule.$name.path")"
    sbranch="$(git config -f .gitmodules "submodule.$name.branch")"
    ptr="$(git ls-tree HEAD "$path" | awk '{print $3}')"
    if [ ! -e "$path/.git" ]; then bad "submoduł $path niezainicjowany (git submodule update --init)"; continue; fi
    # jawny refspec: klon submodułu z CI (actions/checkout) zna tylko gałąź domyślną,
    # więc samo „fetch origin <gałąź>” nie tworzy origin/<gałąź>
    git -C "$path" fetch -q origin "+refs/heads/$sbranch:refs/remotes/origin/$sbranch" 2>/dev/null
    # płytki klon: wskaźnik starszy niż czubek gałęzi nie byłby widoczny jako przodek
    if [ "$(git -C "$path" rev-parse --is-shallow-repository 2>/dev/null)" = true ] \
        && ! git -C "$path" merge-base --is-ancestor "$ptr" "origin/$sbranch" 2>/dev/null; then
        git -C "$path" fetch -q --unshallow origin "+refs/heads/$sbranch:refs/remotes/origin/$sbranch" 2>/dev/null
    fi
    git -C "$path" merge-base --is-ancestor "$ptr" "origin/$sbranch" 2>/dev/null \
        && ok "submoduł $path: ${ptr:0:8} na origin/$sbranch" \
        || bad "submoduł $path: ${ptr:0:8} nie leży na origin/$sbranch"
done

# na main: commity z najnowszej release/* bez odpowiednika (ten sam patch albo cherry-pick -x)
# i bez linii „Tylko-stable:” (commity zmieniające tylko CHANGELOG.md pomijane)
if [ "$BRANCH" = main ] && [ -n "$RELEASES" ]; then
    rel="origin/release/$(echo "$RELEASES" | tail -1)"
    # znacznik bywa ze skróconym hashem (cherry-pick -x po skróconym sha) — rozwijamy do pełnego
    picked="$(git log --format=%B HEAD | grep -oE 'cherry picked from commit [0-9a-f]{7,40}' | awk '{print $5}' \
        | while read -r h; do git rev-parse -q --verify "$h^{commit}" 2>/dev/null; done)"
    missing=""
    for c in $(git cherry HEAD "$rel" | awk '$1 == "+" {print $2}'); do
        body="$(git log -1 --format=%B "$c")"
        echo "$body" | grep -q '^Tylko-stable:' && continue
        # sam CHANGELOG.md: każda linia ma własne wpisy wersji, nie przenosi się ich
        [ "$(git diff-tree --no-commit-id --name-only -r "$c")" = CHANGELOG.md ] && continue
        echo "$picked" | grep -qx "$c" && continue
        # kierunek main -> stable: commit ze stable sam jest cherry-pickiem commita z main
        # (patch-id się różni, gdy przy przenoszeniu rozwiązano konflikt)
        src="$(echo "$body" | grep -oE 'cherry picked from commit [0-9a-f]{7,40}' | awk '{print $5}' | tail -1)"
        [ -n "$src" ] && git merge-base --is-ancestor "$src" HEAD 2>/dev/null && continue
        missing="$missing ${c:0:8}"
    done
    [ -z "$missing" ] && ok "poprawki z $rel przeniesione na main" \
        || bad "commity z $rel bez odpowiednika na main:$missing (cherry-pick -x albo 'Tylko-stable:')"
fi

# listy modułów wbudowanych w trzech buildach
# Only the jmv stage supplies preinstalled modules; optional artifact stages do not.
DOCKER="$(awk '/^FROM / { bundled = ($0 ~ / AS jmv$/) } bundled { print }' docker/jamovi-Dockerfile | grep -oE '^COPY \$JAMOVI_ROOT/[A-Za-z]+/ /tmp/source/' | grep -oE 'ROOT/[A-Za-z]+' | cut -d/ -f2 | sort | tr '\n' ' ')"
MAC="$(grep -oE '^MODULES=\([^)]*\)' packaging/scripts/macos/20-modules.sh | sed 's/MODULES=(//;s/)//' | tr ' ' '\n' | sort | tr '\n' ' ')"
WIN="$(grep -oE "^\\\$Modules *= *@\([^)]*\)" packaging/scripts/windows/build.ps1 | grep -oE "'[A-Za-z]+'" | tr -d "'" | sort | tr '\n' ' ')"
if [ -n "$DOCKER" ] && [ "$DOCKER" = "$MAC" ] && [ "$MAC" = "$WIN" ]; then ok "wbudowane (Docker = macOS = Windows): $MAC"
else bad "listy wbudowanych różnią się — Docker: [$DOCKER] macOS: [$MAC] Windows: [$WIN]"; fi

awk -v v="$JUPWR" '$1 == "##" && $2 == v {found=1} END {exit !found}' CHANGELOG.md && ok "CHANGELOG.md ma wpis $JUPWR" || bad "brak wpisu '## $JUPWR' w CHANGELOG.md"

# spójność wersji: jmc stempluje moduł wersją z jamovi/0000.yaml i nie zagląda
# do DESCRIPTION — rozjazd oznacza, że .jmo/moduł ma inną wersję niż deklarujemy
for d in */jamovi/0000.yaml; do
    m="${d%%/*}"
    [ -f "$m/DESCRIPTION" ] || continue
    vy="$(grep -m1 -oE '^version: *[0-9.]+' "$d" | grep -oE '[0-9.]+')"
    vd="$(grep -m1 '^Version:' "$m/DESCRIPTION" | grep -oE '[0-9.]+')"
    [ -n "$vy" ] && [ "$vy" = "$vd" ] && ok "$m: wersja $vy (0000.yaml = DESCRIPTION)" \
        || bad "$m: rozjazd wersji — 0000.yaml=$vy, DESCRIPTION=$vd (jmc użyje $vy)"
done

# moduły wewnętrzne: zamrożone, poza wydaniem — bez .jmo i bez wiersza macierzy
# (packaging/MODULES.md; Windows: build.ps1 -InternalModules)
INTERNAL=" jRol jPomiar jWody "

# moduły opcjonalne = katalogi z jamovi/0000.yaml spoza listy wbudowanych
case "$(uname -s)" in Darwin) PLAT="macos-arm64";; MINGW*|MSYS*|CYGWIN*) PLAT="win64";; *) PLAT="linux";; esac
for d in */jamovi/0000.yaml; do
    m="${d%%/*}"
    [ -f "$m/DESCRIPTION" ] || continue
    echo " $MAC " | grep -q " $m " && continue
    if echo "$INTERNAL" | grep -q " $m "; then echo "moduł wewnętrzny (poza wydaniem): $m"; continue; fi
    # jmc czyta wersję modułu wyłącznie z jamovi/0000.yaml (index.js:299-306);
    # DESCRIPTION jest tylko dla R CMD i musi się z nią zgadzać
    v="$(grep -m1 -oE '^version: *[0-9.]+' "$d" | grep -oE '[0-9.]+')"
    jmo="packaging/build/dist/${m}_${v}-${PLAT}.jmo"
    echo "moduł opcjonalny: $m $v"
    if $check_artifacts; then
        [ -f "$jmo" ] && ok "$jmo" || bad "brak $jmo — zbuduj dla platformy $PLAT"
    fi
    awk -F '|' -v module="$m" -v app="$JUPWR" -v ver="$v" '
        /^### / {section=$0; sub(/^### +/, "", section)}
        section == module && /^\|/ {
            a=$2; v=$4; gsub(/^[[:space:]]+|[[:space:]]+$/, "", a); gsub(/^[[:space:]]+|[[:space:]]+$/, "", v)
            if (a == app && v == ver) found=1
        }
        END {exit !found}
    ' packaging/MODULES.md && ok "MODULES.md: wiersz $JUPWR / $m $v" \
        || bad "MODULES.md: brak wiersza macierzy dla jUPWR $JUPWR z $m $v"
done

# licencje zbiorów jDane: każdy zbiór ma pole license, a teksty licencji są w kliencie
# (js-yaml bierze z jamovi-compiler/node_modules — w świeżym worktree go nie ma, dopóki
# build albo npm ci go nie zainstaluje; sprawdzenie samo niczego nie instaluje)
# (JUPWR_SKIP_LICENSES=1 tylko dla packaging/tests — katalog testowy nie ma jDane/ ani node_modules)
if [ "${JUPWR_SKIP_LICENSES:-}" = 1 ]; then
    :
elif [ ! -d jamovi-compiler/node_modules/js-yaml ]; then
    bad "licencje zbiorów jDane nie sprawdzone: brak jamovi-compiler/node_modules (uruchom: cd jamovi-compiler && npm ci)"
else
    node packaging/scripts/check-dataset-licenses.mjs && ok "licencje zbiorów jDane" || bad "licencje zbiorów jDane — patrz wyżej"
fi

echo; [ $warn -eq 0 ] && echo "Wydanie spójne." || { echo "Ostrzeżeń: $warn"; exit 1; }
