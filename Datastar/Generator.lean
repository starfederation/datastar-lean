module

public import Datastar.SSE
public import Datastar.Compression

public section

/-!
SSE streaming over any transport.

A server library supplies the monad its handlers run in and a way to write a chunk of the
response body, and calls `sseStream`. `Datastar.StdHttp` does this for `Std.Http`.
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

private def new (write : ByteArray → m Unit) (serialise : m Unit → m Unit)
    (encoder : Option Encoder) : IO (Generator m) :=
  return {write, serialise, encoder, finished := ← IO.mkRef false}

/--
Send a `PatchElements`, `PatchSignals` or `ExecuteScript` event.

Throws once the stream has ended.
-/
def send [Monad m] [MonadLiftT IO m] [ToEvent α] (sse : Generator m) (x : α) : m Unit :=
  let bytes := (renderEvent (toEvent x)).toUTF8
  sse.serialise do
    let encoded ← liftM (m := IO) do
      if ← sse.finished.get then
        throw (IO.userError "SSE stream has ended")
      match sse.encoder with
      | none => pure bytes
      | some encoder => encoder.compress bytes
    sse.write encoded

/-- End the stream, writing a compressed stream's trailing bytes. A second call does nothing. -/
private def finish [Monad m] [MonadLiftT IO m] (sse : Generator m) : m Unit :=
  sse.serialise do
    let trailer ← liftM (m := IO) do
      if ← sse.finished.get then return none
      sse.finished.set true
      sse.encoder.mapM (·.finish)
    if let some bytes := trailer then sse.write bytes

end Generator

private def sseHeaders (encoding : Option String) (negotiated : Bool) : List (String × String) :=
  [("cache-control", "no-cache"), ("content-type", "text/event-stream")]
    ++ (if negotiated then [("vary", "Accept-Encoding")] else [])
    ++ (encoding.map (fun e => [("content-encoding", e)])).getD []

/--
An SSE response for a server library to send: its headers, and its body.
-/
structure SseStream (m : Type → Type) where
  /-- The response headers, names in lower case. -/
  headers : List (String × String)
  /--
  Write the body with the given `write`, returning when the stream has ended. `write` must not
  buffer, or events reach the browser late.
  -/
  writeBody : (ByteArray → m Unit) → m Unit

/--
An SSE stream that runs `callback`, compressed with the first of `compressors` that
`acceptEncoding` (the request's header) allows, uncompressed if there is none.

`send` encodes and writes under `serialise`, which must run its action exclusively. The default,
`id`, is right when only the callback's task sends.

The stream ends when `callback` returns or throws. The headers carry `Vary: Accept-Encoding`
whenever the choice depended on the request (RFC 9110 §12.5.5), so with every strategy but
`.forced`.
-/
def sseStream [Monad m] [MonadLiftT IO m] [MonadExcept IO.Error m]
    (callback : Generator m → m Unit)
    (serialise : m Unit → m Unit := id)
    (compressors : List Compressor := [])
    (acceptEncoding : Option String := none)
    (strategy : CompressionStrategy := .serverPriority) : IO (SseStream m) := do
  let chosen := negotiate compressors ((acceptEncoding.map parseEncodings).getD []) strategy
  let negotiated := !compressors.isEmpty && strategy != .forced
  let encoder ← chosen.mapM (·.start)
  return {
    headers := sseHeaders (chosen.map (·.encoding)) negotiated
    writeBody := fun write => do
      let sse ← (Generator.new write serialise encoder : IO _)
      try
        callback sse
      catch e =>
        -- The callback threw, or the client went away. Finishing releases the encoder; its own
        -- error is dropped in favour of `e`.
        try sse.finish catch _ => pure ()
        throw e

      -- Not under `try`, so that a failed `sse.finish` fails the response: the server then closes
      -- it as incomplete, rather than ending a compressed stream without its trailing bytes.
      sse.finish
  }

end Datastar
