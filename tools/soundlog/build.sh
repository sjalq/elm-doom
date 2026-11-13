#!/bin/sh
# Build the sound log tool natively with eco.
#   ECO=/path/to/eco ./build.sh
set -e
cd "$(dirname "$0")"
ECO="${ECO:-eco}"
"$ECO" make src/SoundLog.elm --output=soundlog
