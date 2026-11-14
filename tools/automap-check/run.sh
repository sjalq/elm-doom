#!/bin/sh
# Compare Doom.Automap with the C automap pixel for pixel.
#
#   ECO=/path/to/eco WAD=/path/to/freedoom1.wad ./run.sh
#
# Builds a C harness from verify/oracle (patched by patch_harness.py: script
# events into AM_Responder after P_Ticker, AM_Drawer's 320x168 window dumped
# after AM_Ticker) and the Elm tool, plays DEMO1..4 with each script*.txt, and
# compares every dumped frame (cmp.py; PNGs of each frame land in out/png*).
set -e
cd "$(dirname "$0")"
ECO="${ECO:-eco}"
WAD="${WAD:-../../wads/freedoom1.wad}"
rm -rf out/harness
mkdir -p out/harness
cp -R ../../verify/oracle/src ../../verify/oracle/Makefile out/harness/
python3 -I patch_harness.py out/harness
make -C out/harness -j8 >/dev/null
"$ECO" make src/AutomapCheck.elm --output=automap-check >/dev/null
for d in ${DEMOS:-1 2 3 4}; do
  for s in script*.txt; do
    n="$d-${s%.txt}"
    rm -rf "out/c$n" "out/e$n"
    mkdir -p "out/c$n" "out/e$n"
    out/harness/doom-oracle -iwad "$WAD" -playdemo "DEMO$d" -amscript "$s" -amout "out/c$n" >/dev/null 2>&1
    ./automap-check "$WAD" "DEMO$d" "$s" "out/e$n" >/dev/null
    echo "DEMO$d $s: $(python3 -I cmp.py "out/c$n" "out/e$n" "out/png$n" "$WAD" | tail -1)"
  done
done
