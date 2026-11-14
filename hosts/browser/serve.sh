#!/bin/sh
# Serve the browser host on http://localhost:${PORT:-8000}/ (index.html is the game,
# soundtest.html the sound test), telling the browser not to cache, so a rebuild
# shows on reload. The page loads freedoom1.wad (or ?wad=NAME) from here: this links
# every WAD in ../../wads, fetching Freedoom there first (tools/fetch-freedoom.sh,
# pinned and checked) if it isn't there yet. You can also drop an IWAD on the page.
set -e
cd "$(dirname "$0")"
../../tools/fetch-freedoom.sh
for w in ../../wads/*.wad ../../wads/*.WAD; do
  [ -e "$w" ] || continue
  name="$(basename "$w")"
  [ -e "$name" ] || ln -s "$(cd ../../wads && pwd -P)/$name" "$name"
done
exec python3 -c '
import http.server, sys
class Handler(http.server.SimpleHTTPRequestHandler):
    extensions_map = {**http.server.SimpleHTTPRequestHandler.extensions_map, ".js": "text/javascript", ".wad": "application/octet-stream"}
    def end_headers(self):
        self.send_header("Cache-Control", "no-store")
        super().end_headers()
http.server.ThreadingHTTPServer(("", int(sys.argv[1])), Handler).serve_forever()
' "${PORT:-8000}"
