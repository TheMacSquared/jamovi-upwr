#!/bin/bash
# Faza R — uczyń jUPWR.app RELOKOWALNĄ (działającą na czystym Macu bez Homebrew i bez R).
# Operuje na gotowej .app z 50-assemble-app.sh. Trzy zależności wbudowywane:
#   1) Python  -> python-build-standalone (relokowalny)
#   2) dylib-y Homebrew (silnik + core.so) -> Resources/jamovi/libs
#   3) R.framework -> Resources/jamovi/R
# Zależności Mach-O są przepisywane przez install_name_tool na @rpath/<nazwa>.
# Centralny katalog Resources/jamovi/libs zawiera dyliby Homebrew i runtime R.
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
# 4. install names Mach-O — wszystkie zależności wewnątrz bundla
# ---------------------------------------------------------------------------
# Rewrite Mach-O dependencies to paths inside the app bundle. This removes
# runtime dependencies on Homebrew and the system R.framework.
log "Przepisywanie zależności Mach-O (/opt/homebrew i R.framework -> @rpath) ..."

# R runtime libraries referenced by absolute framework paths join the central dylib set.
for rlib in libR.dylib libRblas.dylib libRlapack.dylib libgcc_s.1.1.dylib libgfortran.5.dylib libomp.dylib libquadmath.0.dylib; do
    [ -e "$JRES/R/lib/$rlib" ] || die "Brak biblioteki R: $rlib"
    cp -L "$JRES/R/lib/$rlib" "$LIBS/$rlib"
done
rinside="$JRES/R/library/RInside/lib/libRInside.dylib"
[ -e "$rinside" ] || die "Brak biblioteki RInside: $rinside"
cp -L "$rinside" "$LIBS/libRInside.dylib"

MACHO_LIST="$BUILD_DIR/macho-files.txt"
: > "$MACHO_LIST"
while IFS= read -r -d '' candidate; do
    if file -b "$candidate" 2>/dev/null | grep -q 'Mach-O'; then
        printf '%s\n' "$candidate" >> "$MACHO_LIST"
    fi
done < <(find "$APP/Contents" -type f -print0)

rewrite_count=0
while IFS= read -r macho; do
    chmod u+w "$macho" 2>/dev/null || true
    while IFS= read -r dep; do
        [ -n "$dep" ] || continue
        leaf="${dep##*/}"
        target=""
        case "$dep" in
            /opt/homebrew/*) target="$LIBS/$leaf" ;;
            /Library/Frameworks/R.framework/*) target="$LIBS/$leaf" ;;
            libRInside.dylib) target="$LIBS/$leaf" ;;
            *) continue ;;
        esac
        [ -e "$target" ] || die "Brak wbudowanej biblioteki dla $dep (oczekiwano: $target)"
        replacement="@rpath/$leaf"
        install_name_tool -change "$dep" "$replacement" "$macho"
        rewrite_count=$((rewrite_count + 1))
    done < <(otool -L "$macho" 2>/dev/null | tail -n +2 | awk '{print $1}')
done < "$MACHO_LIST"

# Each process that loads bundled extensions needs the central library directory in its run-path stack.
ensure_rpath() {
    local binary="$1" rpath="$2"
    [ -f "$binary" ] || return 0
    if ! otool -l "$binary" 2>/dev/null | awk '/cmd LC_RPATH/{f=1} f&&/path /{print $2;f=0}' | grep -Fxq "$rpath"; then
        install_name_tool -add_rpath "$rpath" "$binary"
    fi
}
ensure_rpath "$APP/Contents/MacOS/jamovi-engine" "@executable_path/../Resources/jamovi/libs"
python_exe="$JRES/python/bin/python3"
if [ -L "$python_exe" ]; then
    python_exe="$(dirname "$python_exe")/$(readlink "$python_exe")"
fi
ensure_rpath "$python_exe" "@executable_path/../../libs"
ensure_rpath "$JRES/R/bin/exec/R" "@executable_path/../../../libs"

# Give bundled dylibs portable IDs as well.
while IFS= read -r dylib; do
    file -b "$dylib" 2>/dev/null | grep -q 'Mach-O.*dynamically linked shared library' || continue
    chmod u+w "$dylib" 2>/dev/null || true
    install_name_tool -id "@rpath/$(basename "$dylib")" "$dylib"
done < <(find "$LIBS" "$JRES/R/lib" -type f -name '*.dylib' 2>/dev/null)

# Fail the build if an absolute Homebrew or system R dependency remains.
absolute_refs="$BUILD_DIR/macho-absolute-deps.txt"
: > "$absolute_refs"
while IFS= read -r macho; do
    otool -L "$macho" 2>/dev/null | tail -n +2 | awk '{print $1}' \
        | grep -E '^(/opt/homebrew/|/Library/Frameworks/R\.framework/)' \
        >> "$absolute_refs" || true
done < "$MACHO_LIST"
[ ! -s "$absolute_refs" ] || {
    sed -n '1,20p' "$absolute_refs" >&2
    die "Pozostaly absolutne zaleznosci Homebrew/R.framework"
}
log "Przepisano $rewrite_count zaleznosci; brak odwolan do Homebrew i systemowego R.framework."

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
EOF

# ---------------------------------------------------------------------------
# 5. podpis ad-hoc (lokalny build bez certyfikatu i notaryzacji)
# ---------------------------------------------------------------------------
# install_name_tool invalidates existing signatures. Re-sign every Mach-O first,
# then the enclosing app bundle. Any signing or verification error is fatal.
log "Ponowne podpisywanie zmodyfikowanych plikow Mach-O ..."
while IFS= read -r macho; do
    codesign --force --sign - "$macho" >/dev/null
    codesign --verify --strict "$macho" >/dev/null
done < "$MACHO_LIST"

log "Podpis ad-hoc calej aplikacji (--force --deep) ..."
codesign --force --deep --sign - "$APP" >/dev/null
codesign --verify --deep --strict "$APP" >/dev/null

log "OK — $APP jest teraz relokowalna ($(du -sh "$APP" | cut -f1))"
log "Weryfikacja: żadna zależność nie wskazuje /opt/homebrew ani systemowego R.framework."
