#!/bin/bash
# Faza B — build frontendu (klient) przez vite.
# Wynik trafia do $PAYLOAD/client.
. "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

cd "$REPO_ROOT/client"

if [ ! -d node_modules ]; then
    log "npm install (klient)..."
    npm install
fi

log "Kopiowanie proto (build:coms) + vite build..."
npm run build:coms
mkdir -p "$PAYLOAD/client"
node ./node_modules/vite/bin/vite.js build --outDir "$PAYLOAD/client" --emptyOutDir

# version obok klienta (jak w Dockerfile)
cp "$REPO_ROOT/version" "$PAYLOAD/client/version"

log "Gotowe. Zawartość $PAYLOAD/client:"
ls "$PAYLOAD/client" | head
[ -f "$PAYLOAD/client/index.html" ] && log "OK — index.html obecny." || die "Brak index.html — build klienta nieudany."
