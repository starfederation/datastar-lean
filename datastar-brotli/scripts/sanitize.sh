#!/usr/bin/env bash
#
# Run the Brotli tests under AddressSanitizer and UndefinedBehaviorSanitizer.
#
# Lake links with Lean's own toolchain, which has no sanitizer runtimes. So this
# builds the tests with Lake to get the C that Lean generates for them, then
# compiles that C, the bindings and Brotli with the system compiler and links
# the result against Lean's shared runtime.
#
# Lean's objects come from its own allocator, which the sanitizers do not track:
# an overrun of a ByteArray can go unnoticed. Everything the bindings and
# Brotli allocate with malloc is checked.
#
# Usage: scripts/sanitize.sh [arguments for brotli_test]

set -euo pipefail

cd "$(dirname "$0")/.."

lake build brotli_test

prefix="$(lean --print-prefix)"
brotli=vendor/brotli/brotli-1.2.0/c
out=.lake/build/sanitize
mkdir -p "$out"

# The modules that brotli_test is made of, apart from Lean's own.
generated=(
  .lake/build/ir/BrotliTest/Properties.c
  .lake/build/ir/DatastarBrotli.c
  .lake/build/ir/DatastarBrotli/DatastarBrotli.c
  ../.lake/build/ir/Datastar/Compression.c
)

# -fPIC: the generated C reads globals of Lean's shared libraries, such as
# ByteArray.empty. Without it, an x86-64 executable gets its own copy of each,
# which Lean's initialisers never fill in, so they read as null.
flags=(-g -O1 -fPIC -fno-omit-frame-pointer -DBROTLI_ENCODER_CLEANUP_ON_OOM
       -DDATASTAR_BROTLI_TESTING
       -I "$prefix/include" -I "$brotli/include")
sanitize=(-fsanitize=address,undefined -fno-sanitize-recover=undefined)

# When an allocation fails, Brotli offsets the null pointer before it checks
# for the failure. The result is never used, so that one check is off for the
# vendored code.
objects=()
for source in "$brotli"/common/*.c "$brotli"/enc/*.c "$brotli"/dec/*.c; do
  object="$out/$(echo "$source" | tr / _).o"
  "${CC:-cc}" -c "${flags[@]}" "${sanitize[@]}" -fno-sanitize=pointer-overflow "$source" -o "$object" &
  objects+=("$object")
done
wait

"${CC:-cc}" "${flags[@]}" "${sanitize[@]}" \
  "${generated[@]}" c/datastar_brotli.c c/datastar_brotli_test.c "${objects[@]}" \
  -L "$prefix/lib/lean" -lleanshared -lInit_shared -Wl,-rpath,"$prefix/lib/lean" -lm \
  -o "$out/brotli_test"

# The soak measures memory use, which the sanitizer changes. Lean does not free
# everything at exit, which is not what this is looking for, and it installs its
# own signal stack, which AddressSanitizer must not try to manage.
ASAN_OPTIONS=detect_leaks=0:use_sigaltstack=0 "$out/brotli_test" --no-soak "$@"
echo "sanitize: no errors reported"
