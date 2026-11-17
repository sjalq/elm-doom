#!/bin/sh
# Fetch the WADs the hosts and tools here use into wads/ (or the directory given),
# each release pinned by its SHA-256 and checked before anything is unpacked. A WAD
# already there is left alone.
#
#   freedoom1.wad, freedoom2.wad   Freedoom 0.13.0 (BSD licensed), the default WAD
#                                  of the hosts and tools; its licence goes in
#                                  COPYING-freedoom.txt
#   doom1.wad                      Doom's shareware episode, v1.9, from id's 1995 zip
#                                  (which id let anyone copy as it is, not for sale)
#   evan.wad                       a patch for doom1.wad (this repository's mods-1
#                                  release), which the browser host offers
#
#   tools/fetch-wads.sh [DIR]
set -e
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
WADS="${1:-$ROOT/wads}"
mkdir -p "$WADS"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

# fetch URL SHA256 FILE: the file at URL into FILE, if its SHA-256 is SHA256
fetch() {
  echo "Fetching $1" >&2
  curl -fsSL --retry 3 -o "$3" "$1"
  if command -v sha256sum >/dev/null 2>&1; then
    got="$(sha256sum "$3" | cut -d' ' -f1)"
  else
    got="$(shasum -a 256 "$3" | cut -d' ' -f1)"
  fi
  if [ "$got" != "$2" ]; then
    echo "$1 has SHA-256 $got, expected $2; not using it" >&2
    exit 1
  fi
}

if [ ! -e "$WADS/freedoom1.wad" ]; then
  fetch https://github.com/freedoom/freedoom/releases/download/v0.13.0/freedoom-0.13.0.zip \
    3f9b264f3e3ce503b4fb7f6bdcb1f419d93c7b546f4df3e874dd878db9688f59 "$TMP/freedoom.zip"
  unzip -q "$TMP/freedoom.zip" -d "$TMP/freedoom"
  cp "$TMP/freedoom/freedoom-0.13.0/freedoom1.wad" "$TMP/freedoom/freedoom-0.13.0/freedoom2.wad" "$WADS/"
  cp "$TMP/freedoom/freedoom-0.13.0/COPYING.txt" "$WADS/COPYING-freedoom.txt"
fi

if [ ! -e "$WADS/doom1.wad" ]; then
  fetch https://archive.org/download/DoomsharewareEpisode/doom.ZIP \
    5560b3cf160b5729e2651a0c576e7898cb34ccbf46fc08a5cafc70e7e0736c60 "$TMP/doom.zip"
  unzip -q "$TMP/doom.zip" DOOM1.WAD -d "$TMP/doom"
  cp "$TMP/doom/DOOM1.WAD" "$WADS/doom1.wad"
fi

if [ ! -e "$WADS/evan.wad" ]; then
  fetch https://github.com/sjalq/elm-doom/releases/download/mods-1/evan.wad \
    248321b58da866ef333c6aa53f6296d53cc72b13cf199fa1c2012af694a32dce "$TMP/evan.wad"
  cp "$TMP/evan.wad" "$WADS/evan.wad"
fi
