#!/bin/bash
# Faza C — moduły R: jmvcore + kompilacja modułów przez jmc.
# Zależności pakietowe (ggplot2, car, multcomp, emmeans, vcd, ...) są w bibliotece
# systemowego R.framework — patrz 00-prereqs.sh / instalacja brakujących z CRAN.
. "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

JMC="$REPO_ROOT/jamovi-compiler/index.js"
BASE_R="$PAYLOAD/modules/base/R"
[ -d "$REPO_ROOT/jamovi-compiler/node_modules" ] || ( cd "$REPO_ROOT/jamovi-compiler" && npm install )
mkdir -p "$BASE_R"

# === RProtoBuf — KRYTYCZNE ===
# Silnik serializuje wyniki przez RProtoBuf (jmvcore$RProtoBuf_serialize <- RProtoBuf::serialize).
# Dwa wymogi:
#  1) RProtoBuf MUSI dzielić ten sam protobuf co silnik (inaczej SIGSEGV) → budujemy ZE ŹRÓDŁA
#     przeciw protobuf z Homebrew (dynamiczny libprotobuf), nie binarkę CRAN (statyczny protobuf).
#  2) RProtoBuf MUSI być zainstalowany PRZED jmvcore — jmvcore pieczętuje RProtoBuf_serialize przy
#     instalacji (lazy-load); bez RProtoBuf wychodzi NULL → "could not find function RProtoBuf_serialize".
# RProtoBuf ląduje w base/R (pewna ścieżka .libPaths silnika).
log "Build RProtoBuf ze źródła (przeciw protobuf $(pkg-config --modversion protobuf 2>/dev/null)) -> $BASE_R ..."
export PATH="$BREW_PREFIX/bin:$PATH"
export PKG_CONFIG_PATH="$BREW_PREFIX/lib/pkgconfig:$BREW_PREFIX/opt/protobuf/lib/pkgconfig:${PKG_CONFIG_PATH:-}"
RP_SRC="$BUILD_DIR/RProtoBuf-src.tar.gz"
[ -f "$RP_SRC" ] || Rscript -e "download.file(available.packages(repos='https://cloud.r-project.org')['RProtoBuf','Repository'] |> paste0('/RProtoBuf_', available.packages(repos='https://cloud.r-project.org')['RProtoBuf','Version'], '.tar.gz'), '$RP_SRC')" 2>/dev/null \
  || curl -L -f -s -o "$RP_SRC" "https://cran.r-project.org/src/contrib/RProtoBuf_0.4.27.tar.gz"
(
  export CPPFLAGS="-std=c++17 -I$BREW_PREFIX/include $(pkg-config --cflags protobuf)"
  export PKG_LIBS="$(pkg-config --libs protobuf)"
  # pomiń wadliwe testy nagłówków protobuf 35 (abseil/c++17) w configure RProtoBuf
  export ac_cv_header_google_protobuf_stubs_common_h=yes
  export ac_cv_header_google_protobuf_compiler_code_generator_h=yes
  R_LIBS="$BASE_R" R CMD INSTALL "$RP_SRC" --library="$BASE_R" --no-test-load
)
[ -d "$BASE_R/RProtoBuf" ] || die "RProtoBuf nie zainstalowany — silnik nie zserializuje wyników!"

log "Instalacja jmvcore do $BASE_R (RProtoBuf obecny → RProtoBuf_serialize zostanie zapieczony) ..."
R_LIBS="$BASE_R" R CMD INSTALL "$REPO_ROOT/jmvcore" --library="$BASE_R"

# === Dodatkowe zależności CRAN dla modułu plots (scatr): ridgeline -> ggridges, hexbin -> hexbin ===
# jmc uruchamia R CMD INSTALL z R_LIBS_SITE/R_LIBS_USER ograniczonym do base/R (patrz
# jamovi-compiler/compilerr.js), więc systemowa biblioteka R.framework NIE jest widoczna mimo
# --skip-deps. Bez tego kroku instalacja modułu 'plots' pada z "dependency 'hexbin' is not
# available for package 'scatr'" i moduł scatr w ogóle się nie buduje (brak R/ w payloadzie).
log "Instalacja zależności ridge/hexbin (ggridges, hexbin) do $BASE_R ..."
"$R_HOME_SYS/bin/R" -e "install.packages(c('ggridges', 'hexbin'), repos='https://cloud.r-project.org', lib='$BASE_R')" \
  || die "Nie udało się zainstalować ggridges/hexbin — moduł plots (scatr) nie zbuduje się poprawnie!"

# Moduły w kolejności jak w docker/jamovi-Dockerfile.
# R znajduje zależności w $R_HOME_SYS/library (systemowy R), jmvcore w base/R.
MODULES=(jmv plots jperm jCI jboot jdistrACTION)
for m in "${MODULES[@]}"; do
    [ -d "$REPO_ROOT/$m" ] || { log "pomijam $m (brak katalogu)"; continue; }
    log "jmc --install $m ..."
    node "$JMC" --install "$REPO_ROOT/$m" \
        --to "$PAYLOAD/modules" \
        --rhome "$R_HOME_SYS" \
        --rlibs "$BASE_R" \
        --patch-version --skip-deps
done

log "Zainstalowane moduły:"
ls "$PAYLOAD/modules"
for m in jmv jCI scatr; do
    [ -d "$PAYLOAD/modules/$m" ] || die "Moduł $m nie zainstalowany!"
    [ -d "$PAYLOAD/modules/$m/R" ] || die "Moduł $m nie ma zainstalowanego pakietu R (brak modules/$m/R) — silnik nie znajdzie analiz!"
done
log "OK — moduły zbudowane."
