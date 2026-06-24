#!/bin/bash
# Faza C2 — kompilacja tłumaczeń (i18n) do JSON.
. "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

cd "$REPO_ROOT/i18n"
[ -d node_modules ] || { log "npm install (i18n)..."; npm install; }

mkdir -p "$PAYLOAD/i18n/json"
log "Build i18n -> $PAYLOAD/i18n/json"
node "$REPO_ROOT/i18n/index.js" --build src --dest "$PAYLOAD/i18n/json"

[ -f "$PAYLOAD/i18n/json/manifest.json" ] && log "OK — i18n zbudowane." || die "Brak manifest.json — i18n nieudane."
