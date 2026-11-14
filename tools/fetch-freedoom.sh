#!/bin/sh
# Fetch Freedoom (a free IWAD, BSD-licensed, by the Freedoom project) into wads/:
# wads/freedoom1.wad (Phase 1, the one the hosts and tools default to),
# wads/freedoom2.wad and wads/COPYING-freedoom.txt. The release and its SHA-256 are
# pinned here (the same release Elm OS pins); the zip is checked before anything is
# unpacked. Does nothing if wads/freedoom1.wad is already there.
#   tools/fetch-freedoom.sh
set -e
VER=0.13.0
SHA256=3f9b264f3e3ce503b4fb7f6bdcb1f419d93c7b546f4df3e874dd878db9688f59
URL="https://github.com/freedoom/freedoom/releases/download/v$VER/freedoom-$VER.zip"

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
WADS="$ROOT/wads"
[ -e "$WADS/freedoom1.wad" ] && exit 0
mkdir -p "$WADS"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
echo "Fetching Freedoom $VER from $URL" >&2
curl -fsSL -o "$TMP/freedoom.zip" "$URL"
if command -v sha256sum >/dev/null 2>&1; then
  GOT="$(sha256sum "$TMP/freedoom.zip" | cut -d' ' -f1)"
else
  GOT="$(shasum -a 256 "$TMP/freedoom.zip" | cut -d' ' -f1)"
fi
if [ "$GOT" != "$SHA256" ]; then
  echo "freedoom-$VER.zip has SHA-256 $GOT, expected $SHA256; not using it" >&2
  exit 1
fi
mkdir "$TMP/x"
unzip -q "$TMP/freedoom.zip" -d "$TMP/x"
cp "$TMP/x/freedoom-$VER/freedoom1.wad" "$TMP/x/freedoom-$VER/freedoom2.wad" "$WADS/"
cp "$TMP/x/freedoom-$VER/COPYING.txt" "$WADS/COPYING-freedoom.txt"
echo "Freedoom $VER: $WADS/freedoom1.wad, $WADS/freedoom2.wad" >&2
