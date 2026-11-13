#!/bin/sh
# Serve the browser host on http://localhost:${PORT:-8000}/ (index.html is the game,
# soundtest.html the sound test), telling the browser not to cache, so a rebuild
# shows on reload. Links freedoom1.wad from ../headless if it isn't here yet.
set -e
cd "$(dirname "$0")"
if [ ! -e freedoom1.wad ] && [ -e ../headless/freedoom1.wad ]; then
  ln -s "$(cd ../headless && pwd -P)/freedoom1.wad" freedoom1.wad
fi
exec python3 -c '
import http.server, sys
class Handler(http.server.SimpleHTTPRequestHandler):
    extensions_map = {**http.server.SimpleHTTPRequestHandler.extensions_map, ".js": "text/javascript", ".wad": "application/octet-stream"}
    def end_headers(self):
        self.send_header("Cache-Control", "no-store")
        super().end_headers()
http.server.ThreadingHTTPServer(("", int(sys.argv[1])), Handler).serve_forever()
' "${PORT:-8000}"
