#!/bin/bash
# Faza R — uczyń jUPWR.app RELOKOWALNĄ (działającą na czystym Macu bez Homebrew i bez R).
# Operuje na gotowej .app z 50-assemble-app.sh. Trzy zależności wbudowywane:
#   1) Python  -> python-build-standalone (relokowalny)
#   2) dylib-y Homebrew (silnik + core.so) -> Resources/jamovi/libs
#   3) R.framework -> Resources/jamovi/R
# Rozwiązywanie w runtime: DYLD_FALLBACK_LIBRARY_PATH (po NAZWIE pliku łapie brakujące
# ścieżki absolutne /opt/homebrew i /Library/Frameworks/R.framework). Bez install_name_tool.
. "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

APP="$DIST/$APP_NAME.app"
RES="$APP/Contents/Resources"
JRES="$RES/jamovi"
[ -d "$APP" ] || die "Brak $APP — najpierw 50-assemble-app.sh"

PBS_VERSION="${PBS_VERSION:-3.12.13}"
PBS_TAG="${PBS_TAG:-20260623}"
PBS_DIR="$BUILD_DIR/pbs"

# ---------------------------------------------------------------------------
# 1. PYTHON relokowalny (python-build-standalone) z zależnościami z requirements
# ---------------------------------------------------------------------------
if [ ! -x "$PBS_DIR/python/bin/python3" ]; then
    log "Pobieranie python-build-standalone $PBS_VERSION ($PBS_TAG) ..."
    mkdir -p "$PBS_DIR"; ( cd "$PBS_DIR"
      URL="https://github.com/astral-sh/python-build-standalone/releases/download/${PBS_TAG}/cpython-${PBS_VERSION}+${PBS_TAG}-aarch64-apple-darwin-install_only_stripped.tar.gz"
      curl -L -f -s -o python.tar.gz "$URL" && tar -xzf python.tar.gz )
fi
PBS_PY="$PBS_DIR/python/bin/python3"
[ -x "$PBS_PY" ] || die "Brak python-build-standalone"

log "Instalacja requirements do python-build-standalone ..."
"$PBS_PY" -m pip install --upgrade pip wheel setuptools >/dev/null
"$PBS_PY" -m pip install -r "$REPO_ROOT/server/requirements.txt" >/dev/null
"$PBS_PY" -m pip install --upgrade protobuf >/dev/null   # >= gencode protoc (patrz 30-server.sh)

log "Podmiana Pythona w .app na relokowalny ..."
rm -rf "$JRES/python"
cp -R "$PBS_DIR/python" "$JRES/python"

# ---------------------------------------------------------------------------
# 2. dylib-y Homebrew (rekurencyjnie) z silnika i core.so -> Resources/jamovi/libs
# ---------------------------------------------------------------------------
LIBS="$JRES/libs"; rm -rf "$LIBS"; mkdir -p "$LIBS"
collect() {  # $1 = binarka/dylib; kopiuje zależności z /opt/homebrew ORAZ @loader_path
             # (rekurencyjnie, spłaszczone po nazwie pliku). $2 = katalog źródłowy bieżącej libki.
    local srcdir; srcdir="$(dirname "$(readlink -f "$1" 2>/dev/null || echo "$1")")"
    otool -L "$1" 2>/dev/null | tail -n +2 | awk '{print $1}' | while read -r dep; do
        local src=""
        case "$dep" in
          /opt/homebrew/*)      src="$(readlink -f "$dep" 2>/dev/null || echo "$dep")" ;;
          @loader_path/*)       src="$srcdir/${dep#@loader_path/}" ;;   # sibling w Cellar (np. boost_atomic)
          @rpath/*)             src="$srcdir/${dep#@rpath/}" ;;          # heurystyka: często obok
        esac
        [ -z "$src" ] && continue
        local leaf; leaf=$(basename "$dep")
        if [ ! -f "$LIBS/$leaf" ] && [ -f "$src" ]; then
            cp "$src" "$LIBS/$leaf"; chmod u+w "$LIBS/$leaf"
            collect "$src"
        fi
    done
}
log "Zbieranie dylibów Homebrew (silnik + core.so + .so modułów R, m.in. RProtoBuf) ..."
collect "$APP/Contents/MacOS/jamovi-engine"
CORE_SO=$(find "$JRES/server" -name "core.cpython-*-darwin.so" | head -1)
[ -n "$CORE_SO" ] && collect "$CORE_SO"
# .so pakietów R (RProtoBuf linkuje protobuf/abseil — musi dzielić je z silnikiem)
while IFS= read -r so; do collect "$so"; done < <(find "$JRES/modules" "$JRES/R/library" -name "*.so" 2>/dev/null | grep -iE "RProtoBuf" )
# nanomsg bywa pod /opt/homebrew/opt — upewnij się, że jest
[ -f "$LIBS/libnanomsg.6.dylib" ] || cp "$(readlink -f "$BREW_PREFIX/lib/libnanomsg.6.dylib")" "$LIBS/libnanomsg.6.dylib" 2>/dev/null || true
log "Zebrano $(ls "$LIBS" | wc -l | tr -d ' ') dylibów ($(du -sh "$LIBS" | cut -f1))"

# ---------------------------------------------------------------------------
# 3. R.framework -> Resources/jamovi/R  (R_HOME = ten katalog)
# ---------------------------------------------------------------------------
log "Wbudowywanie R (to potrwa, ~1 GB) ..."
R_RES_SRC="$(cd "$R_HOME_SYS" && pwd -P)"          # .../R.framework/Versions/4.x/Resources
rm -rf "$JRES/R"; mkdir -p "$JRES/R"
# kopiujemy zawartość Resources; pomijamy dokumentację/testy/html dla rozmiaru
rsync -a --delete \
    --exclude 'doc' --exclude 'tests' --exclude 'html' --exclude 'po' \
    --exclude 'library/*/help' --exclude 'library/*/doc' --exclude 'library/*/html' \
    --exclude 'library/*/tests' --exclude 'library/translations' \
    "$R_RES_SRC/" "$JRES/R/"
log "R wbudowane ($(du -sh "$JRES/R" | cut -f1))"

# ---------------------------------------------------------------------------
# 4. env.conf — wszystkie ścieżki WZGLĘDNE + DYLD_FALLBACK na wbudowane liby
# ---------------------------------------------------------------------------
log "Zapis relokowalnego env.conf ..."
# JAMOVI_R_VERSION = rVersion modułów (inaczej moduły 'incompatible' i pusta wstążka analiz).
JAMOVI_R_VERSION="$(grep -E '^rVersion:' "$JRES/modules/jmv/jamovi-full.yaml" | awk '{print $2}')"
[ -n "$JAMOVI_R_VERSION" ] || JAMOVI_R_VERSION="${R_VERSION}-arm64"
cat > "$RES/env.conf" <<EOF
[ENV]
JAMOVI_HOME=..
JAMOVI_CLIENT_PATH=../Resources/jamovi/client
JAMOVI_MODULES_PATH=../Resources/jamovi/modules
JAMOVI_I18N_PATH=../Resources/jamovi/i18n/json
JAMOVI_VERSION_PATH=../Resources/jamovi/version
JAMOVI_R_VERSION=$JAMOVI_R_VERSION
JAMOVI_SERVER_CMD=../Resources/jamovi/python/bin/python3 -m jamovi.server 0 --slave
PYTHONPATH=../Resources/jamovi/server
R_HOME=../Resources/jamovi/R
R_LIBS=../Resources/jamovi/modules/base/R
DYLD_LIBRARY_PATH=../Resources/jamovi/R/lib:../Resources/jamovi/R/library/RInside/lib:../Resources/jamovi/libs
DYLD_FALLBACK_LIBRARY_PATH=../Resources/jamovi/R/lib:../Resources/jamovi/R/library/RInside/lib:../Resources/jamovi/libs:/usr/lib:/usr/local/lib
EOF

# ---------------------------------------------------------------------------
# 5. podpis ad-hoc (bez hardened runtime, by DYLD_* działało)
# ---------------------------------------------------------------------------
log "Podpis ad-hoc (--force --deep) ..."
codesign --force --deep --sign - "$APP" >/dev/null 2>&1 || log "(codesign ad-hoc: ostrzeżenie)"

log "OK — $APP jest teraz relokowalna ($(du -sh "$APP" | cut -f1))"
log "Weryfikacja: żadna zależność nie powinna wskazywać /opt/homebrew po ustawieniu DYLD."
