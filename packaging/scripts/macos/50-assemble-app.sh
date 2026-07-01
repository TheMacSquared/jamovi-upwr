#!/bin/bash
# Faza F — montaż jUPWR.app (wariant DEV: R i biblioteki Homebrew po ścieżkach absolutnych
# tej maszyny — NIE relokowalny). Relokowalność do .dmg: patrz 60-package-dmg.sh / docs.
. "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

ELECTRON_VERSION="${ELECTRON_VERSION:-32.3.3}"
APP="$DIST/$APP_NAME.app"
RES="$APP/Contents/Resources"
MACOS="$APP/Contents/MacOS"

# --- 1. pobranie Electrona (ZIP z GitHub releases — niezawodniej niż postinstall npm) ---
EDIR="$BUILD_DIR/electron"
ELECTRON_APP="$EDIR/extracted/Electron.app"
if [ ! -d "$ELECTRON_APP" ]; then
    log "Pobieranie Electron $ELECTRON_VERSION (darwin-arm64) ..."
    mkdir -p "$EDIR/extracted"
    URL="https://github.com/electron/electron/releases/download/v${ELECTRON_VERSION}/electron-v${ELECTRON_VERSION}-darwin-arm64.zip"
    curl -L -f -s -o "$EDIR/electron.zip" "$URL" || die "Pobranie Electrona nieudane: $URL"
    ( cd "$EDIR/extracted" && unzip -q -o ../electron.zip )
fi
[ -d "$ELECTRON_APP" ] || die "Nie znaleziono Electron.app"

# --- 2. szkielet .app z Electrona ---
log "Tworzenie $APP ..."
rm -rf "$APP"; mkdir -p "$DIST"
cp -R "$ELECTRON_APP" "$APP"

# przemianowanie binarki na jUPWR + aktualizacja Info.plist
mv "$MACOS/Electron" "$MACOS/$APP_NAME"
PLIST="$APP/Contents/Info.plist"
/usr/libexec/PlistBuddy -c "Set :CFBundleExecutable $APP_NAME" "$PLIST"
/usr/libexec/PlistBuddy -c "Set :CFBundleName $APP_NAME" "$PLIST" 2>/dev/null || /usr/libexec/PlistBuddy -c "Add :CFBundleName string $APP_NAME" "$PLIST"
/usr/libexec/PlistBuddy -c "Set :CFBundleDisplayName $APP_NAME" "$PLIST" 2>/dev/null || /usr/libexec/PlistBuddy -c "Add :CFBundleDisplayName string $APP_NAME" "$PLIST"
/usr/libexec/PlistBuddy -c "Set :CFBundleIdentifier pl.upwr.jupwr" "$PLIST" 2>/dev/null || true
# wersja jUPWR (0.5.1), nie bazowego jamovi
/usr/libexec/PlistBuddy -c "Set :CFBundleShortVersionString $JUPWR_VERSION" "$PLIST" 2>/dev/null || /usr/libexec/PlistBuddy -c "Add :CFBundleShortVersionString string $JUPWR_VERSION" "$PLIST"
/usr/libexec/PlistBuddy -c "Set :CFBundleVersion $JUPWR_VERSION" "$PLIST" 2>/dev/null || /usr/libexec/PlistBuddy -c "Add :CFBundleVersion string $JUPWR_VERSION" "$PLIST"

# usuń domyślną aplikację Electrona, podstaw naszą jako app.asar
rm -f "$RES/default_app.asar"
log "Pakowanie kodu aplikacji (app.asar) ..."
( cd "$REPO_ROOT/electron" && [ -d node_modules ] || npm install ) >/dev/null 2>&1 || true
npx --yes @electron/asar pack "$REPO_ROOT/electron/app" "$RES/app.asar"

# --- 3. payload do Resources/jamovi ---
log "Kopiowanie payloadu (client, server, python, modules, i18n) ..."
rm -rf "$RES/jamovi"; mkdir -p "$RES/jamovi"
cp -R "$PAYLOAD/client"  "$RES/jamovi/client"
cp -R "$PAYLOAD/server"  "$RES/jamovi/server"
cp -R "$PAYLOAD/python"  "$RES/jamovi/python"
cp -R "$PAYLOAD/modules" "$RES/jamovi/modules"
[ -d "$PAYLOAD/i18n" ] && cp -R "$PAYLOAD/i18n" "$RES/jamovi/i18n"
cp "$REPO_ROOT/version" "$RES/jamovi/version"

# silnik do Contents/MacOS (engine.py szuka home/MacOS/jamovi-engine na Darwin)
cp "$PAYLOAD/bin/jamovi-engine" "$MACOS/jamovi-engine"

# --- 4. env.conf (DEV: R systemowy po ścieżce absolutnej) ---
log "Zapis env.conf (Resources/env.conf) ..."
RLIB="$R_HOME_SYS/library"
# JAMOVI_R_VERSION MUSI równać się rVersion modułów — inaczej serwer oznacza je jako
# 'incompatible' (R-version mismatch) i wstążka analiz/wykresów jest PUSTA. Czytamy z modułu.
JAMOVI_R_VERSION="$(grep -E '^rVersion:' "$PAYLOAD/modules/jmv/jamovi-full.yaml" | awk '{print $2}')"
[ -n "$JAMOVI_R_VERSION" ] || JAMOVI_R_VERSION="${R_VERSION}-arm64"
cat > "$RES/env.conf" <<EOF
[ENV]
JAMOVI_HOME=..
JAMOVI_CLIENT_PATH=../Resources/jamovi/client
JAMOVI_MODULES_PATH=../Resources/jamovi/modules
JAMOVI_I18N_PATH=../Resources/jamovi/i18n/json
JAMOVI_VERSION_PATH=../Resources/jamovi/version
JAMOVI_R_VERSION=$JAMOVI_R_VERSION
JAMOVI_SERVER_CMD=../Resources/jamovi/python/bin/python -m jamovi.server 0 --slave
PYTHONPATH=../Resources/jamovi/server
R_HOME=$R_HOME_SYS
R_LIBS=../Resources/jamovi/modules/base/R
DYLD_FALLBACK_LIBRARY_PATH=$R_HOME_SYS/lib:$RLIB/RInside/lib
EOF

# --- 5. podpis ad-hoc (bez certyfikatu) by Gatekeeper nie zabijał od razu ---
log "Podpis ad-hoc (codesign --deep -s -) ..."
codesign --force --deep --sign - "$APP" >/dev/null 2>&1 || log "(codesign ad-hoc pominięty/niekrytyczny)"

log "OK — zmontowano $APP"
log "Uruchom: open \"$APP\"   (lub: \"$MACOS/$APP_NAME\" aby widzieć logi)"
