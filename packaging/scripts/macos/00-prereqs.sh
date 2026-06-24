#!/bin/bash
# Faza A — prerekwizyty buildu macOS arm64.
# Instaluje narzędzia przez Homebrew i weryfikuje toolchain.
. "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

log "Instalacja pakietów Homebrew..."
brew install boost nanomsg pkg-config protobuf create-dmg python@3.12

log "Weryfikacja toolchainu:"
fail=0
check() { printf '  %-14s ' "$1"; if command -v "$2" >/dev/null 2>&1; then "$2" --version 2>/dev/null | head -1; else echo "(BRAK)"; fail=1; fi; }
check R R
check Rscript Rscript
check node node
check npm npm
check clang clang
check python3.12 "$PY312"
check pkg-config pkg-config

[ -d "$BREW_PREFIX/include/boost" ] || { echo "  boost headers: BRAK"; fail=1; }
ls "$BREW_PREFIX"/lib/libnanomsg* >/dev/null 2>&1 || { echo "  nanomsg lib: BRAK"; fail=1; }

echo
log "R_HOME = $R_HOME_SYS"
log "R_VERSION = $R_VERSION"
log "Python 3.12 = $PY312"

[ "$fail" = 0 ] && log "OK — wszystkie prerekwizyty obecne." || die "Brakuje prerekwizytów (patrz wyżej)."

# --- ZWERYFIKOWANE WERSJE (ta sesja, 2026-06, macOS 26.5 arm64) ---
#   boost 1.90.0 · nanomsg 1.2.2 · pkgconf 2.5.1 · protobuf 35.1
#   create-dmg 1.2.3 · python 3.12.13 · R 4.6.0 · node 24.17 · clang 21
