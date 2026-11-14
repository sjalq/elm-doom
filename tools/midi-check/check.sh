#!/bin/sh
# Compare Doom.Audio.Midi with Chocolate Doom's mus2mid (doom/browser/oplmusic.js's
# port of it) for every music lump of the WADs given, byte for byte.
#   ELM=/path/to/elm-0.19.1 ./check.sh WAD...
# (default WAD: ../../headless/freedoom1.wad)
set -e
cd "$(dirname "$0")"
ELM="${ELM:-elm}"
"$ELM" make src/MidiCheck.elm --optimize --output=midi-check.js > /dev/null
[ $# -gt 0 ] || set -- ../../headless/freedoom1.wad
node check.mjs "$@"
