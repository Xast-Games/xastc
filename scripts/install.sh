#!/usr/bin/env bash
# Builds xastc and installs it with Std:
#   <prefix>/bin/xastc
#   <prefix>/share/xastc/std
#
#   scripts/install.sh                 install to /usr/local
#   scripts/install.sh --dev           link Std to this repo, so edits to std/ apply at once
#   scripts/install.sh --prefix DIR    install elsewhere, e.g. ~/.local (needs no sudo)
#   scripts/install.sh --uninstall     remove what was installed
set -euo pipefail

prefix=/usr/local
dev=false
uninstall=false

while [[ $# -gt 0 ]]; do
   case "$1" in
      --prefix)    prefix="${2:?--prefix needs a directory}"; shift 2 ;;
      --dev)       dev=true; shift ;;
      --uninstall) uninstall=true; shift ;;
      -h|--help)   sed -n '2,10p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
      *)           echo "unknown option: $1 (see --help)" >&2; exit 1 ;;
   esac
done

repo="$(cd "$(dirname "$0")/.." && pwd)"
binary="$prefix/bin/xastc"
std="$prefix/share/xastc/std"

# Runs a command with sudo only if the prefix is not writable.
privileged() {
   if [[ -w "$prefix" || ( ! -e "$prefix" && -w "$(dirname "$prefix")" ) ]]; then
      "$@"
   else
      sudo "$@"
   fi
}

if $uninstall; then
   privileged rm -f "$binary"
   privileged rm -rf "${std:?}" "$prefix/share/xastc"
   echo "Removed $binary and $prefix/share/xastc"
   exit 0
fi

cd "$repo"
echo "Building xastc..."
cabal build -v0 -O2 exe:xastc
built="$(cabal list-bin -O2 exe:xastc)"

privileged install -d "$prefix/bin" "$prefix/share/xastc"
privileged install -m 755 "$built" "$binary"

privileged rm -rf "$std"
if $dev; then
   privileged ln -s "$repo/std" "$std"
else
   privileged cp -r "$repo/std" "$std"
fi

echo "Installed:"
echo "  $binary"
echo "  $std$($dev && echo " -> $repo/std")"
case ":$PATH:" in
   *":$prefix/bin:"*) ;;
   *) echo "Note: $prefix/bin is not in your PATH." ;;
esac
