#!/bin/sh
# Build the browser host as a site to put on a web server, in DIR (default _site):
# the pages, the scripts, and the WADs the page offers in DIR/wads (fetched by
# tools/fetch-wads.sh, pinned and checked). The GitHub Pages workflow
# (.github/workflows/pages.yml) publishes it.
#   ELM=/path/to/elm-0.19.1 hosts/browser/site.sh [DIR]
set -e
HERE="$(cd "$(dirname "$0")" && pwd)"
OUT="${1:-$HERE/_site}"
"$HERE/build.sh"
rm -rf "$OUT"
mkdir -p "$OUT/wads"
cp "$HERE"/index.html "$HERE"/soundtest.html "$HERE"/page.css "$HERE"/*.js "$OUT/"
"$HERE/../../tools/fetch-wads.sh" "$OUT/wads"
rm -f "$OUT/wads/freedoom2.wad"
