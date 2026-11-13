#!/bin/sh
# Render songs with Chocolate Doom's C music code (music-ref) and with the
# browser's JS port (render.mjs), and compare them sample by sample.
#   ./compare.sh [seconds]       (default 60)
# WADs: ../../headless/freedoom1.wad, and FREEDOOM2 (default: next to it).
set -e
cd "$(dirname "$0")"
S="${1:-60}"
W1="${FREEDOOM1:-../../headless/freedoom1.wad}"
W2="${FREEDOOM2:-$(dirname "$(readlink "$W1" 2>/dev/null || echo "$W1")")/freedoom2.wad}"
mkdir -p out
python3 midi2mus.py "$W1" D_E1M1 out/E1M1.mus
python3 midi2mus.py "$W1" D_INTER out/INTER.mus
status=0
run() { # name wad lump [volume] [looping]
  name=$1; wad=$2; lump=$3; shift 3
  ./music-ref "$wad" "$lump" "$S" "out/$name.ref.raw" "$@" 2> /dev/null
  node render.mjs "$wad" "$lump" "$S" "out/$name.js.raw" "$@"
  printf '%-16s ' "$name"
  node compare.mjs "out/$name.ref.raw" "out/$name.js.raw" || status=1
}
run D_E1M1 "$W1" D_E1M1
run D_E1M2 "$W1" D_E1M2
run D_INTRO "$W1" D_INTRO
run D_INTER "$W1" D_INTER
run D_VICTOR "$W1" D_VICTOR
run D_INTRO-once "$W1" D_INTRO 64 0
run D_E1M1-vol120 "$W1" D_E1M1 120
run D_E1M1-vol8 "$W1" D_E1M1 8
run E1M1.mus "$W1" out/E1M1.mus
run INTER.mus "$W1" out/INTER.mus
# the game's later calls: pause and resume, volume, song changes, stop
run script "$W1" D_E1M1 64 1 "300:pause,420:resume,700:volume:24,900:volume:100,1200:play:D_E1M2:1,1500:play:D_INTRO:0,1800:stop,1900:play:D_E1M1:1"
if [ -e "$W2" ]; then
  run D_RUNNIN "$W2" D_RUNNIN
  run D_STALKS "$W2" D_STALKS
  run D_DM2TTL "$W2" D_DM2TTL
  run D_SHAWN "$W2" D_SHAWN
fi
exit $status
