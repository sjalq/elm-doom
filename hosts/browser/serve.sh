#!/bin/sh
# Serve the browser host on http://localhost:${PORT:-8000}/ (index.html is the game,
# soundtest.html the sound test), telling the browser not to cache, so a rebuild
# shows on reload. The page's choices load from wads/ here, a link to ../../wads,
# which tools/fetch-wads.sh fills first (pinned and checked) if it isn't yet.
set -e
cd "$(dirname "$0")"
../../tools/fetch-wads.sh
[ -e wads ] || ln -s ../../wads wads
exec python3 -c '
import http.server, sys
class Handler(http.server.SimpleHTTPRequestHandler):
    extensions_map = {**http.server.SimpleHTTPRequestHandler.extensions_map, ".js": "text/javascript", ".wad": "application/octet-stream"}
    def end_headers(self):
        self.send_header("Cache-Control", "no-store")
        super().end_headers()
http.server.ThreadingHTTPServer(("", int(sys.argv[1])), Handler).serve_forever()
' "${PORT:-8000}"
