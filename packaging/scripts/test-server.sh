#!/usr/bin/env bash
# Build the Python server from a source copy and run its tests, without the
# Docker image. Mirrors the "server" stage of docker/jamovi-Dockerfile.
# Needs: python3-dev, build-essential, protobuf-compiler,
# libboost-filesystem-dev, libboost-system-dev, libnanomsg-dev.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/jupwr-server.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT

PYTHON="${PYTHON:-python3}"
LIB="$WORK/lib"
mkdir -p "$LIB" "$WORK/source" "$WORK/recovery"

"$PYTHON" -m venv "$WORK/venv"
# shellcheck disable=SC1091
. "$WORK/venv/bin/activate"
python -m pip install --quiet --upgrade pip setuptools wheel
python -m pip install --quiet -r "$ROOT/server/requirements.txt"

# Build copies so setup.py never writes into the checkout. setup.py resolves
# shared headers relative to the server directory, so keep the layout.
cp -R "$ROOT/readstat" "$WORK/source/readstat"
cp -R "$ROOT/server" "$WORK/source/server"
rm -rf "$WORK/source/readstat/build" "$WORK/source/server/build" \
       "$WORK/source/server/jamovi.egg-info"

install_lib() {
    python setup.py --quiet install --install-lib="$LIB" \
        --single-version-externally-managed --record=/dev/null
}

(cd "$WORK/source/readstat" && install_lib)
(cd "$WORK/source/server" && SETUP_CORE_ONLY=1 install_lib)
(cd "$WORK/source/server" && SETUP_SERVER_ONLY=1 install_lib)

# The image supplies these through bin/env.conf; here they point at the
# temporary tree so the tests never read or write the user's profile.
mkdir -p "$WORK/app/modules" "$WORK/app/client" "$WORK/user"
export HOME="$WORK/user"
export JAMOVI_HOME="$WORK/app"
export JAMOVI_MODULES_PATH="$WORK/app/modules"
export JAMOVI_CLIENT_PATH="$WORK/app/client"
export JAMOVI_VERSION_PATH="$ROOT/version"
export JAMOVI_RECOVERY_PATH="$WORK/recovery"

# Tests that need the real C++/R engine skip themselves outside the image.
cd "$LIB"
PYTHONPATH="$LIB" python -m pytest jamovi/tests -q -p no:cacheprovider "$@"
