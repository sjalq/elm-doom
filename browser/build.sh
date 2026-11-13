#!/bin/sh
# Build the browser host: elm.js (the game, src/Main.elm) and soundtest.js (the
# sound test page, src/SoundTest.elm), optimised, with Elm 0.19.1.
#   ELM=/path/to/elm-0.19.1 ./build.sh
set -e
cd "$(dirname "$0")"
ELM="${ELM:-/private/tmp/claude-501/-Users-sjalq-git-Elm-OS/e4fd43a1-3ec5-4ef0-8a7c-bd78ee51e29e/scratchpad/elm0191/node_modules/.bin/elm}"
if [ ! -x "$ELM" ]; then ELM=elm; fi
"$ELM" make src/SoundTest.elm --optimize --output=soundtest.js
if [ "$1" != "--soundtest-only" ]; then
  "$ELM" make src/Main.elm --optimize --output=elm.js
fi
