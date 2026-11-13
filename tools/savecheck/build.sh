#!/bin/sh
# Build the saved-game checker natively with eco.
#   ECO=/path/to/eco ./build.sh
# Compare with the oracle: doom-oracle -iwad W -playdemo DEMOn -savecheck T c.dsg
# and ./savecheck W DEMOn T e.dsg give the same save bytes and state lines.
set -e
cd "$(dirname "$0")"
ECO="${ECO:-eco}"
"$ECO" make src/SaveCheck.elm --output=savecheck
