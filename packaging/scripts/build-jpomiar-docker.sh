#!/bin/bash
# Export an optional Linux .jmo using the reference Docker toolchain.
set -euo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$REPO_ROOT"
POMIAR_VERSION="$(sed -n 's/^version: *//p' jPomiar/jamovi/0000.yaml)"
POMIAR_DESC="$(sed -n 's/^Version: *//p' jPomiar/DESCRIPTION)"
if [ -z "$POMIAR_VERSION" ] || [ "$POMIAR_VERSION" != "$POMIAR_DESC" ]; then
    echo 'jPomiar: wersje 0000.yaml i DESCRIPTION różnią się' >&2
    exit 1
fi
mkdir -p packaging/build/dist
docker build --file docker/jamovi-Dockerfile --target jpomiar-artifact \
    --output type=local,dest=packaging/build/dist .
echo "Moduł Linux: packaging/build/dist/jPomiar_${POMIAR_VERSION}-linux.jmo"
