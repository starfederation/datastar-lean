module

public import Datastar.SSE
public import Datastar.Compression

public section

/-!
SSE streaming over any transport.

A server library supplies the monad its handlers run in and a way to write a chunk of the
response body. `Datastar.StdHttp` does this for `Std.Http`.
-/

namespace Datastar

/--
An opaque handle for sending SSE events to the browser from a handler in `m`.
-/
structure Generator (m : Type → Type) where
  private mk ::
  private write : ByteArray → m Unit
  private serialise : m Unit → m Unit
  private encoder : Option Encoder
  private finished : IO.Ref Bool

namespace Generator

/--
A generator that writes each event to the response body with `write`.

`send` and `finish` encode and write under `serialise`, which must run its action exclusively.
Pass `id` if only one task uses the generator.
-/
def new (write : ByteArray → m Unit) (serialise : m Unit → m Unit)
    (encoder : Option Encoder := none) : IO (Generator m) :=
  return {write, serialise, encoder, finished := ← IO.mkRef false}

private def encode (sse : Generator m) (bytes : ByteArray) : IO ByteArray := do
  let some encoder := sse.encoder | return bytes
  if ← sse.finished.get then
    throw (IO.userError "SSE stream has ended")
  encoder.compress bytes

/--
Send a `PatchElements`, `PatchSignals` or `ExecuteScript` event.
-/
def send [Monad m] [MonadLiftT IO m] [ToEvent α] (sse : Generator m) (x : α) : m Unit :=
  let text := renderEvent (toEvent x)
  sse.serialise do
    sse.write (← (sse.encode text.toUTF8 : IO ByteArray))

/--
End a compressed stream, writing the encoder's trailing bytes. Does nothing to an uncompressed
stream. Call this once, after the last `send`.
-/
def finish [Monad m] [MonadLiftT IO m] (sse : Generator m) : m Unit :=
  match sse.encoder with
  | none => pure ()
  | some encoder =>
    sse.serialise do
      liftM (m := IO) (sse.finished.set true)
      sse.write (← (encoder.finish : IO ByteArray))

end Generator

/--
The response headers of an SSE stream. `encoding` is the `Content-Encoding` of a compressed
stream; `negotiated` says that the choice depended on the request's `Accept-Encoding`, so that a
cache must not serve the response to a client that sent a different one (RFC 9110 §12.5.5).
-/
def sseHeaders (encoding : Option String := none) (negotiated := false) : List (String × String) :=
  [("cache-control", "no-cache"), ("content-type", "text/event-stream")]
    ++ (if negotiated then [("vary", "Accept-Encoding")] else [])
    ++ (encoding.map (fun e => [("content-encoding", e)])).getD []

end Datastar
