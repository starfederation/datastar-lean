#!/usr/bin/env bash
# Vendors the parts of Brotli that datastar-brotli builds: the encoder, the
# decoder (tests only), their shared sources and headers, and the license.
# Replaces brotli-<version>/ next to this script. Needs curl and tar.

set -euo pipefail

version=1.2.0
sha256=816c96e8e8f193b40151dad7e8ff37b1221d019dbcb9c35cd3fadbfe6477dfec
keep=(c/common c/dec c/enc c/include LICENSE)

cd "$(dirname "${BASH_SOURCE[0]}")"

dest=brotli-$version
url=https://github.com/google/brotli/archive/refs/tags/v$version.tar.gz

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
tarball=$tmp/$dest.tar.gz

curl -fsSL "$url" -o "$tarball"

if command -v sha256sum >/dev/null 2>&1; then
    actual=$(sha256sum "$tarball")
else
    actual=$(shasum -a 256 "$tarball")
fi
actual=${actual%% *}
if [ "$actual" != "$sha256" ]; then
    echo "$url: sha256 $actual, expected $sha256" >&2
    exit 1
fi

rm -rf "$dest"
tar -xzf "$tarball" "${keep[@]/#/$dest/}"
