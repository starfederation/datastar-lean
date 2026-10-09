# datastar-brotli

Brotli compression of [Datastar](https://data-star.dev/) SSE streams, for the
[Datastar Lean SDK](../README.md).

It provides one thing: `brotli`, a `Compressor` to pass to `sseResponseWith`. Each event is
compressed and flushed as it is sent, so the browser can act on it straight away, and the encoder
keeps its history for the life of the connection, so later events that repeat earlier markup
compress well.

The encoder is Google's [Brotli](https://github.com/google/brotli) 1.2.0, vendored under
[vendor/brotli](vendor/brotli) by [fetch.sh](vendor/brotli/fetch.sh) and built by Lake. Nothing
needs to be installed apart from a C compiler.

## Usage

Add both packages to your `lakefile.toml`:

```toml
[[require]]
name = "datastar"
git = "https://github.com/starfederation/datastar-lean"

[[require]]
name = "datastar-brotli"
git = "https://github.com/starfederation/datastar-lean"
subDir = "datastar-brotli"
```

Then pass `brotli` to `sseResponseWith`:

```lean
import Datastar
import DatastarBrotli

open Datastar

def handler (req : Request Body.Stream) : ContextAsync (Response Body.Any) :=
  sseResponseWith [brotli] req fun sse =>
    sse.send <| patchElements "<div id=\"message\">Hello!</div>"
```

The response is compressed when the request's `Accept-Encoding` includes `br`, and sent as it is
otherwise. Browsers only offer `br` over HTTPS and to `localhost`.

## Options

```lean
brotli (quality := 5) (windowLog := 24) (mode := .text)
```

| Option | Range | Default | Meaning |
|---|---|---|---|
| `quality` | 0 to 11 | 5 | 0 is fastest, 11 gives the smallest output. 10 and 11 are far slower and meant for static files. |
| `windowLog` | 10 to 24 | 24 | Base-2 logarithm of the window size. A larger window finds more repetition across events, and uses more memory for each open stream. |
| `mode` | `.generic`, `.text`, `.font` | `.text` | A hint about the kind of input. |

Starting a stream fails with an `IO` error if `quality` or `windowLog` is out of range.

## Calls must be serialised

A stream is not thread-safe. `compress` and `finish` on one stream must never run at the same
time as each other, or as a second `compress`. `finish` frees the encoder, so a `compress` that
is still running on another thread would use freed memory.

`sseResponseWith` takes a lock around every call, so a `ServerSentEventGenerator` is safe to
share between tasks, and nothing more is needed in normal use. This only matters to code that
calls `brotli.start` itself and uses the resulting `Encoder` from more than one task: that code
must serialise its calls.

After `finish`, `compress` fails with an error and a second `finish` returns nothing.

## Building

```
lake build
```

Lake compiles the vendored Brotli sources and [c/datastar_brotli.c](c/datastar_brotli.c) with
`cc`, or with `$CC` if that is set. Only the encoder is built into the library; the decoder is
built for the tests alone.

The library is built with `BROTLI_ENCODER_CLEANUP_ON_OOM`, so a failed allocation inside the
encoder is an `IO` error and does not exit the process.

It has been built on macOS and Linux. Windows is untested.

## Tests

```
lake test
```

runs [BrotliTest/Properties.lean](BrotliTest/Properties.lean), which takes a few seconds:

- **Flush completeness.** For generated streams over every quality and window size, the output
  after each `compress` decodes to exactly the input so far.
- **A model of the stream.** Generated sequences of `compress` and `finish` behave as described
  above, and a stream can be dropped at any point.
- **No leaks.** The C side counts its live allocations, which must be zero after every case.
- **Allocation failures.** Each allocation of a stream is made to fail in turn. Every failure
  must be reported as an error, with nothing leaked.
- **Soak.** Memory use must stay level over many calls, which catches leaked Lean objects that
  the allocation count cannot see.

A failing generated case prints its seed. `lake exe brotli_test 1 <seed>` runs that case alone,
and `lake exe brotli_test <cases>` runs more cases than the default 300.

Two more checks run separately:

- `scripts/sanitize.sh` rebuilds the tests with AddressSanitizer and UndefinedBehaviorSanitizer
  and runs them, without the soak.
- `e2e/tests/brotli.spec.ts`, in the Playwright suite at the top of the repository, checks that
  a browser receives each event of a compressed stream while the stream is still open. It is
  served by [BrotliTest/E2EServer.lean](BrotliTest/E2EServer.lean).

The tests link the bindings compiled with `DATASTAR_BROTLI_TESTING`, which adds the allocation
counter and fault injection behind `datastar_brotli_live_allocations` and
`datastar_brotli_fail_allocation`. The library has neither.
