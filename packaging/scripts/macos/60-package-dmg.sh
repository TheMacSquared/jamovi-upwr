#!/bin/bash
# Faza G — zapakowanie jUPWR.app do jUPWR.dmg (create-dmg).
# UWAGA: .dmg dziedziczy relokowalność .app. Wariant DEV (50-assemble-app.sh) zależy od
# /opt/homebrew i systemowego R — działa tylko na maszynach z tym samym środowiskiem.
# Do dystrybucji studentom najpierw uczyń .app relokowalną (patrz 10-build-macos.md → Relokowalność).
. "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

APP="$DIST/$APP_NAME.app"
DMG="$DIST/$APP_NAME-$VERSION-arm64.dmg"
[ -d "$APP" ] || die "Brak $APP — najpierw uruchom 50-assemble-app.sh"

command -v create-dmg >/dev/null || die "create-dmg nie zainstalowany (brew install create-dmg)"

log "Tworzenie $DMG ..."
rm -f "$DMG"
create-dmg \
    --volname "$APP_NAME $VERSION" \
    --window-size 540 380 \
    --icon "$APP_NAME.app" 140 180 \
    --app-drop-link 400 180 \
    --no-internet-enable \
    "$DMG" "$APP" \
  || { # create-dmg zwraca !=0 gdy AppleScript do układu okna nie zadziała (headless) —
       # .dmg i tak zwykle powstaje. Sprawdzamy poniżej.
       log "(create-dmg zgłosił ostrzeżenie — sprawdzam czy .dmg powstał)"; }

[ -f "$DMG" ] || die "Nie udało się utworzyć .dmg"
log "OK — $DMG ($(du -h "$DMG" | cut -f1))"
log "Test montażu:"; hdiutil attach "$DMG" -nobrowse -readonly | tail -1
