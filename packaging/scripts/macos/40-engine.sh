#!/bin/bash
# Faza E — kompilacja silnika C++ (jamovi-engine) przeciw systemowemu R.framework.
# Boost/nanomsg/protobuf z Homebrew arm64 (/opt/homebrew). RInside/Rcpp z biblioteki R.
. "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

cd "$REPO_ROOT/engine"

# proto z brew (protoc) musi być w PATH; pkg-config musi widzieć protobuf+abseil
export PATH="$BREW_PREFIX/bin:$PATH"
export PKG_CONFIG_PATH="$BREW_PREFIX/lib/pkgconfig:$BREW_PREFIX/opt/protobuf/lib/pkgconfig:${PKG_CONFIG_PATH:-}"

log "configure (rhome=$R_HOME_SYS) ..."
# --base-module-path wskazuje bibliotekę systemowego R (tam są RInside, Rcpp) — jak w Dockerfile.
# Konkretne CXXFLAGS nadpisujemy przy make (poniżej) — configure ustawia tu R_HOME/INCLUDES/LDFLAGS.
bash configure \
    --rhome="$R_HOME_SYS" \
    --base-module-path="$R_HOME_SYS/library" \
    --rpath="$R_HOME_SYS/lib"

# protobuf 35 ciągnie abseil i wymaga C++17 (Makefile.in domyślnie -std=c++11).
# Nadpisujemy CXXFLAGS w wierszu poleceń make (GNU make: command-line var wygrywa,
# więc 'CXXFLAGS += -std=c++11' z Makefile jest ignorowane). NIE modyfikujemy plików engine/.
# LDFLAGS z configure (R, boost, nanomsg, RInside, frameworks) zostają nietknięte.
PROTO_CFLAGS="$(pkg-config --cflags protobuf)"
PROTO_LIBS="$(pkg-config --libs protobuf)"

# Boost >= 1.69 ma boost_system jako header-only — brew Boost 1.90 nie dostarcza
# libboost_system, a Makefile.in twardo linkuje -lboost_system. Podkładamy pusty
# stub libboost_system.a (symbole są inline w nagłówkach) na ścieżce linkera.
STUBLIB="$BUILD_DIR/stublib"
mkdir -p "$STUBLIB"
echo 'static int _jupwr_boost_system_stub;' > "$STUBLIB/stub.c"
clang -c "$STUBLIB/stub.c" -o "$STUBLIB/stub.o"
ar rcs "$STUBLIB/libboost_system.a" "$STUBLIB/stub.o"

# -lboost_nowide: kod używa boost::nowide::setenv w EngineR::initR (Makefile linkuje je tylko
# w bloku Windows; na macOS musimy dodać sami).
ENGINE_CXXFLAGS="-std=c++17 -DJAMOVI_ENGINE_SUPPORT_LOCAL_SOCKETS -mmacosx-version-min=11.0 \
-I$BREW_PREFIX/include $PROTO_CFLAGS -L$STUBLIB -L$BREW_PREFIX/lib -lboost_nowide $PROTO_LIBS"

log "make (C++17 + protobuf/abseil) ..."
make clean >/dev/null 2>&1 || true
make CXXFLAGS="$ENGINE_CXXFLAGS"

[ -f jamovi-engine ] || die "Kompilacja silnika nieudana — brak jamovi-engine."

mkdir -p "$PAYLOAD/bin"
cp jamovi-engine "$PAYLOAD/bin/jamovi-engine"
log "Skopiowano do $PAYLOAD/bin/jamovi-engine"
log "Zależności dynamiczne (otool -L):"
otool -L "$PAYLOAD/bin/jamovi-engine" | sed 's/^/    /'
log "OK — silnik zbudowany."
