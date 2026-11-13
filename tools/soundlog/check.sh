#!/bin/sh
# Compare the Elm sound log with the C oracle's -soundlog for demos (DEMO1..4 by
# default): builds nothing, runs ../../oracle/doom-oracle and ./soundlog.
#   ./check.sh [WAD] [DEMO...]
set -e
cd "$(dirname "$0")"
WAD="${1:-../../headless/freedoom1.wad}"
[ $# -gt 0 ] && shift
DEMOS="${*:-DEMO1 DEMO2 DEMO3 DEMO4}"
mkdir -p out
status=0
for d in $DEMOS; do
  ../../oracle/doom-oracle -full -iwad "$WAD" -playdemo "$d" -soundlog "out/$d.oracle.txt" > /dev/null 2>&1
  ./soundlog "$WAD" "$d" > "out/$d.elm.txt"
  printf '%s: ' "$d"
  python3 ../../oracle/compare_sounds.py --files "out/$d.oracle.txt" "out/$d.elm.txt" || status=1
done
exit $status
