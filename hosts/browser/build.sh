#!/bin/sh
# Build the browser host: elm.js (the game, src/Main.elm) and soundtest.js (the
# sound test page, src/SoundTest.elm), optimised, with Elm 0.19.1.
#   ELM=/path/to/elm-0.19.1 ./build.sh
set -e
cd "$(dirname "$0")"
ELM="${ELM:-elm}"
"$ELM" make src/SoundTest.elm --optimize --output=soundtest.js
if [ "$1" != "--soundtest-only" ]; then
  "$ELM" make src/Main.elm --optimize --output=elm.js
fi
