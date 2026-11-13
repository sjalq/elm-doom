#!/bin/sh
# Serve the browser host on http://localhost:${PORT:-8000}/ (index.html is the game,
# soundtest.html the sound test). Links freedoom1.wad from ../headless if it isn't
# here yet.
set -e
cd "$(dirname "$0")"
if [ ! -e freedoom1.wad ] && [ -e ../headless/freedoom1.wad ]; then
  ln -s "$(cd ../headless && pwd -P)/freedoom1.wad" freedoom1.wad
fi
exec python3 -m http.server "${PORT:-8000}"
