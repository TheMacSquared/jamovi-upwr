#!/bin/bash
# Export an optional Linux .jmo using the reference Docker toolchain.
set -euo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$REPO_ROOT"
WODY_VERSION="$(sed -n 's/^version: *//p' jWody/jamovi/0000.yaml)"
WODY_DESC="$(sed -n 's/^Version: *//p' jWody/DESCRIPTION)"
if [ -z "$WODY_VERSION" ] || [ "$WODY_VERSION" != "$WODY_DESC" ]; then
    echo 'jWody: wersje 0000.yaml i DESCRIPTION różnią się' >&2
    exit 1
fi
mkdir -p packaging/build/dist
docker build --file docker/jamovi-Dockerfile --target jwody-artifact \
    --output type=local,dest=packaging/build/dist .
echo "Moduł Linux: packaging/build/dist/jWody_${WODY_VERSION}-linux.jmo"
