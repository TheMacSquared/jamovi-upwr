#!/bin/bash
# Wspólne zmienne i funkcje dla skryptów buildu macOS jUPWR.
# Sourcowane przez pozostałe skrypty: . "$(dirname "$0")/lib.sh"

set -euo pipefail

# --- katalog repo (dwa poziomy w górę od packaging/scripts/macos) ---
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"

# --- katalogi robocze (ignorowane przez git: **/build/) ---
BUILD_DIR="$REPO_ROOT/packaging/build"
STAGE="$BUILD_DIR/stage"          # payload: $STAGE/jamovi/{client,server,python,R,modules,i18n,version,bin}
DIST="$BUILD_DIR/dist"            # gotowy jUPWR.app i jUPWR.dmg
PAYLOAD="$STAGE/jamovi"           # logiczny odpowiednik /usr/lib/jamovi

# --- toolchain (Homebrew arm64) ---
BREW_PREFIX="$(brew --prefix)"
PY312="$BREW_PREFIX/bin/python3.12"
R_HOME_SYS="$(R RHOME)"           # systemowy R.framework Resources, np. /Library/Frameworks/R.framework/Resources
R_VERSION="$(Rscript -e 'cat(paste0(R.version$major,".",R.version$minor))' 2>/dev/null)"

# --- nazwa produktu / wersje ---
APP_NAME="jUPWR"
# wersja bazowego jamovi (plik version) — używana wewnętrznie przez jamovi
JAMOVI_VERSION="$(cat "$REPO_ROOT/version" 2>/dev/null || echo 0.0.0.0)"
# wersja DYSTRYBUCJI jUPWR (single source of truth: client/common/jupwr.ts) — do nazw installek
JUPWR_VERSION="$(grep -oE "JUPWR_VERSION *= *'[0-9.]+'" "$REPO_ROOT/client/common/jupwr.ts" 2>/dev/null | grep -oE "[0-9]+(\.[0-9]+)+")"
[ -n "$JUPWR_VERSION" ] || JUPWR_VERSION="$JAMOVI_VERSION"
# VERSION = wersja widoczna w nazwie installki (jUPWR)
VERSION="$JUPWR_VERSION"

log()  { printf '\033[1;36m[%s]\033[0m %s\n' "$(basename "$0")" "$*"; }
die()  { printf '\033[1;31m[BŁĄD]\033[0m %s\n' "$*" >&2; exit 1; }

mkdir -p "$BUILD_DIR"

# --- ochrona plików źródłowych przed nadpisaniem przez jmc ------------------
# jmc zapisuje część plików z powrotem do drzewa ŹRÓDŁOWEGO, nie do payloadu:
#   * <moduł>/jamovi/0000.yaml  (index.js:576-593, indexPath = <srcDir>/jamovi)
#   * <moduł>/R/00jmv.R         (index.js:552-561, tylko jmv)
# Przy regeneracji 0000.yaml: --patch-version podmienia `version` na wersję
# aplikacji, yaml.dump zmienia zawijanie i cudzysłowy, a pola nieobecne
# w źródle dostają wartości domyślne — m.in. menuTitle = title, i tak właśnie
# ginęły krótkie etykiety menu przed 0.9.2.1. 00jmv.R z kolei odtwarza cytowania
# z pakietów R ZAINSTALOWANYCH NA TEJ MASZYNIE, więc jego treść zależy od hosta.
# Jedno i drugie to artefakt builda, nie zmiana w kodzie — przywracamy bajt
# w bajt. Odpowiednik $YamlBackup + finally z packaging/scripts/windows/build.ps1.
#
# UWAGA — celowo NIE obejmujemy plików `.h.R` ani `.u.yaml` w submodule `plots`.
# Tam jmc nie tyle brudzi, co aktualizuje: dopisuje do `.u.yaml` kontrolki dla
# opcji obecnych w `.a.yaml` i regeneruje nagłówki. To bywa realną zmianą,
# którą trzeba obejrzeć i ewentualnie zacommitować w submodule, a nie zamiatać.
SRC_GUARD_DIR=""

# src_restore — idempotentne; wywoływane przez trap, także po błędzie i Ctrl-C.
src_restore() {
    [ -n "$SRC_GUARD_DIR" ] && [ -d "$SRC_GUARD_DIR" ] || return 0
    local n=0 f rel
    while IFS= read -r f; do
        rel="${f#$SRC_GUARD_DIR/}"
        cp -p "$f" "$REPO_ROOT/$rel"
        n=$((n + 1))
    done < <(find "$SRC_GUARD_DIR" -type f)
    rm -rf "$SRC_GUARD_DIR"
    SRC_GUARD_DIR=""
    [ "$n" -eq 0 ] || log "przywrócono pliki źródłowe nadpisane przez jmc ($n)"
}

# src_guard <moduł> [<moduł> ...] — zdejmij kopie PRZED wywołaniem jmc.
# cp -p (a nie przepisanie przez powłokę) zachowuje bajty, końce linii i prawa.
src_guard() {
    SRC_GUARD_DIR="$(mktemp -d "${TMPDIR:-/tmp}/jupwr-srcguard.XXXXXX")"
    local m rel
    for m in "$@"; do
        for rel in "$m/jamovi/0000.yaml" "$m/R/00jmv.R"; do
            [ -f "$REPO_ROOT/$rel" ] || continue
            mkdir -p "$SRC_GUARD_DIR/$(dirname "$rel")"
            cp -p "$REPO_ROOT/$rel" "$SRC_GUARD_DIR/$rel"
        done
    done
    trap src_restore EXIT
    trap 'src_restore; exit 130' INT TERM
}
