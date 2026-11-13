#!/bin/sh
# Build the headless Doom runner natively with eco.
#   ECO=/path/to/eco ./build.sh
set -e
cd "$(dirname "$0")"
ECO="${ECO:-eco}"
"$ECO" make src/Main.elm --output=doom-headless
