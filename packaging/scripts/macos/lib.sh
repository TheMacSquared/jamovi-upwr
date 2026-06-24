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
JUPWR_VERSION="$(grep -oE "JUPWR_VERSION *= *'[0-9.]+'" "$REPO_ROOT/client/common/jupwr.ts" 2>/dev/null | grep -oE "[0-9]+\.[0-9]+\.[0-9]+")"
[ -n "$JUPWR_VERSION" ] || JUPWR_VERSION="$JAMOVI_VERSION"
# VERSION = wersja widoczna w nazwie installki (jUPWR)
VERSION="$JUPWR_VERSION"

log()  { printf '\033[1;36m[%s]\033[0m %s\n' "$(basename "$0")" "$*"; }
die()  { printf '\033[1;31m[BŁĄD]\033[0m %s\n' "$*" >&2; exit 1; }

mkdir -p "$BUILD_DIR"
