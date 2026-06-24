#!/bin/bash
# Faza D — serwer Python: interpreter 3.12 + requirements + jamovi.core/server/readstat.
# UWAGA: w tej wersji budujemy venv (dev). Pełna relokowalność (python-build-standalone)
# jest opisana w 10-build-macos.md jako krok do dystrybucji.
. "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

export PATH="$BREW_PREFIX/bin:$PATH"
PYVENV="$PAYLOAD/python"
STUBLIB="$BUILD_DIR/stublib"   # libboost_system.a stub (tworzony w 40-engine.sh)
if [ ! -f "$STUBLIB/libboost_system.a" ]; then
    mkdir -p "$STUBLIB"; echo 'static int _s;' > "$STUBLIB/stub.c"
    clang -c "$STUBLIB/stub.c" -o "$STUBLIB/stub.o"; ar rcs "$STUBLIB/libboost_system.a" "$STUBLIB/stub.o"
fi

# flagi kompilacji rozszerzeń C/Cython (boost/nanomsg/protobuf z Homebrew + stub boost_system)
export CFLAGS="-I$BREW_PREFIX/include ${CFLAGS:-}"
export CPPFLAGS="-I$BREW_PREFIX/include ${CPPFLAGS:-}"
export LDFLAGS="-L$BREW_PREFIX/lib -L$STUBLIB ${LDFLAGS:-}"

log "Tworzenie venv 3.12 w $PYVENV ..."
rm -rf "$PYVENV"
"$PY312" -m venv --copies "$PYVENV"
PY="$PYVENV/bin/python"
"$PY" -m pip install --upgrade pip wheel setuptools >/dev/null

log "Instalacja requirements serwera ..."
"$PY" -m pip install -r "$REPO_ROOT/server/requirements.txt"

# protobuf (python) MUSI być >= gencode generowanego przez brew protoc (inaczej VersionError
# przy starcie: "gencode 7.35.1 runtime 7.34.0"). UWAGA: PyPI używa innego schematu niż protoc
# (protoc 35.1 == python protobuf 7.35.1), więc bierzemy po prostu najnowszy — runtime >= gencode
# jest wymagane, a runtime NOWSZY niż gencode jest dozwolony.
log "Aktualizacja protobuf (python) do najnowszego (zgodność z protoc $(protoc --version|awk '{print $2}')) ..."
"$PY" -m pip install --upgrade protobuf

mkdir -p "$PAYLOAD/server"

log "Build readstat ..."
cd "$REPO_ROOT/readstat"
"$PY" setup.py install --install-lib="$PAYLOAD/server" --single-version-externally-managed --record=/dev/null

log "Build jamovi.core (Cython) ..."
cd "$REPO_ROOT/server"
SETUP_CORE_ONLY=1 "$PY" setup.py install --install-lib="$PAYLOAD/server" --single-version-externally-managed --record=/dev/null

log "Build jamovi.server ..."
SETUP_SERVER_ONLY=1 "$PY" setup.py install --install-lib="$PAYLOAD/server" --single-version-externally-managed --record=/dev/null

log "Weryfikacja importu serwera ..."
PYTHONPATH="$PAYLOAD/server" "$PY" -c "import jamovi.server; import jamovi.core; print('jamovi.server + core OK')" \
    || die "Import serwera nieudany."
log "OK — serwer zbudowany."
