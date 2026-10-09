import Datastar.Compression

/-!
Brotli compression of SSE streams: the `brotli` compressor, for `sseResponseWith`.

The encoder is the vendored Brotli C library, reached through the bindings in
`c/datastar_brotli.c`. A stream takes a lock around each call, so an `Encoder` may be shared
between tasks: `finish` cannot free the encoder under a running `compress`.
-/

namespace Datastar

private opaque BrotliStreamPointed : NonemptyType

/--
A Brotli encoder for one stream, owned by the C side and freed when the last reference is
dropped. Its calls are serialised by a lock on the C side.
-/
private def BrotliStream : Type := BrotliStreamPointed.type

private instance : Nonempty BrotliStream := BrotliStreamPointed.property

/-- A new encoder. Out-of-range `quality` and `windowLog` are clamped by Brotli, not rejected. -/
@[extern "datastar_brotli_new"]
private opaque BrotliStream.new (quality : UInt8) (windowLog :UInt8) (mode : UInt8) : IO BrotliStream

/--
Compress `chunk` and flush, so that the output so far decodes to all of the input so far. Fails
once the stream is finished.
-/
@[extern "datastar_brotli_compress"]
private opaque BrotliStream.compress (stream : @& BrotliStream) (chunk : @& ByteArray) : IO ByteArray

/--
End the stream and free the encoder, returning the trailing bytes. Later calls return nothing.
-/
@[extern "datastar_brotli_finish"]
private opaque BrotliStream.finish (stream : @& BrotliStream) : IO ByteArray

/-- A hint to the encoder about the kind of input. -/
inductive BrotliMode where
  /-- No assumption. -/
  | generic
  /-- UTF-8 text, the default. -/
  | text
  /-- WOFF 2.0 font data. -/
  | font
deriving DecidableEq, Repr

private def BrotliMode.toUInt8 : BrotliMode → UInt8
  | .generic => 0
  | .text    => 1
  | .font    => 2

/--
A Brotli compressor for `sseResponseWith`.

* `quality`: 0 (fastest) to 11 (smallest). 10 and 11 are far slower, and meant for static files.
* `windowLog`: the base-2 logarithm of the window size, 10 to 24. A larger window finds more
  repetition across events and uses more memory for each open stream.
* `mode`: a hint about the kind of input.

A started stream serialises its own calls, so its `Encoder` may be shared between tasks. The
output of each call must still be sent before the next call is made, as `sseResponseWith`
arranges under its own lock.
-/
def brotli (quality : Nat := 6) (windowLog : Nat := 22) (mode : BrotliMode := .text)
    (hq : quality ≤ 11 := by decide) (hw : 10 ≤ windowLog ∧ windowLog ≤ 24 := by decide) :
    Compressor where
  encoding := "br"
  start := do
    -- The bounds make the narrowing exact.
    let quality := UInt8.ofNatLT quality (Nat.lt_of_le_of_lt hq (by decide))
    let windowLog := UInt8.ofNatLT windowLog (Nat.lt_of_le_of_lt hw.2 (by decide))
    let stream ← BrotliStream.new quality windowLog mode.toUInt8
    pure { compress := stream.compress, finish := stream.finish }

end Datastar
