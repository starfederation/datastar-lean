module

public import Std.Http
public import Datastar.Types
public import Datastar.SSE
public import Datastar.Compression
public import Datastar.Generator

public section

/-!
SSE streaming on `Std.Http`, and the signals a request carries.

The browser sends its signals as JSON: in the `datastar` query parameter for GET and DELETE, in
the request body otherwise. `signalsText` returns that JSON as text, for any JSON library to
read; `Datastar.LeanJson` decodes it with `Lean.Data.Json`.
-/

open Std Async Http

namespace Datastar

/--
An opaque handle for sending SSE events to the browser.

Obtain one from the callback passed to `sseResponse`.

The handle is safe to share between tasks.
-/
abbrev ServerSentEventGenerator := Generator Async

/--
Send a `PatchElements`, `PatchSignals` or `ExecuteScript` event.

Throws once the client has gone away.
-/
def ServerSentEventGenerator.send [ToEvent α] (sse : ServerSentEventGenerator) (x : α) : Async Unit :=
  Generator.send sse x

private def withLock (lock : Std.Semaphore) (action : Async Unit) : Async Unit := do
  let p ← lock.acquire
  let res : Option Unit ← await p.result?

  match res with
    | none => throw (IO.userError "SSE generator lock was dropped")
    | some _ => try action finally lock.release

private def acceptEncoding : Header.Name := .mk "accept-encoding"

private def clientEncodings (req : Request β) : List String :=
  match req.line.headers.get? acceptEncoding with
  | some header => parseEncodings header.value
  | none => []

private def sseResponseCore
    (chosen : Option Compressor)
    (negotiated : Bool)
    (callback : ServerSentEventGenerator → ContextAsync Unit) :
    ContextAsync (Response Body.Any) := do
  let ctx ← ContextAsync.getContext
  let lock ← Semaphore.new 1
  let mut builder := Response.ok
  for (name, value) in sseHeaders (chosen.map (·.encoding)) negotiated do
    let some name' := Header.Name.ofString? name
      | throw (IO.userError s!"invalid header name: {name.quote}")
    let some value' := Header.Value.ofString? value
      | throw (IO.userError s!"invalid {name}: {value.quote}")
    builder := builder.header name' value'

  let encoder ← chosen.mapM (·.start)

  builder.stream fun stream => do
    let sse ← Generator.new (fun data => stream.send { data }) (withLock lock) encoder
    try
      ContextAsync.runIn ctx (callback sse)
    catch e =>
      -- The callback threw, or the client disconnected. We should try to finish the
      -- sse stream, as this will clean up resources like the encoder.
      -- This call to `sse.finish` might also fail, but we drop the error here as we
      -- are about to throw `e`.
      try sse.finish catch _ => pure ()
      throw e

    -- The callback completed, so finish the SSE stream, which calls `encoder.finish` for
    -- a compressed stream.
    --
    -- We deliberately do not wrap this `sse.finish` in a try/catch. If we did so, an
    -- error from `encoder.finish` would be squashed, and this (body) producer would
    -- return normally. The client would receive a complete response whose compressed
    -- stream is missing its final bytes. Instead, the error is propagated so it
    -- reaches `Handler.onFailure` and the connection is closed as incomplete.
    sse.finish

/--
A response that streams SSE events. The connection stays open until `callback` returns.
-/
def sseResponse (callback : ServerSentEventGenerator → ContextAsync Unit) : ContextAsync (Response Body.Any) :=
  sseResponseCore none false callback

/--
`sseResponse`, compressed with the first of `compressors` that the client accepts; uncompressed
if there is none. The response carries `Vary: Accept-Encoding` whenever the choice depended on
the request, so with every strategy but `.forced`.
-/
def sseResponseWith
    (compressors : List Compressor)
    (req : Request β)
    (callback : ServerSentEventGenerator → ContextAsync Unit)
    (strategy : CompressionStrategy := .serverPriority) :
    ContextAsync (Response Body.Any) :=
  sseResponseCore (negotiate compressors (clientEncodings req) strategy)
    (negotiated := !compressors.isEmpty && strategy != .forced) callback

/--
The signals in the `datastar` query parameter, as the JSON text the browser sent.
-/
def signalsTextFromQuery (req : Request β) : Except String String := do
  let some (some encoded) := req.line.uri.query.find? "datastar"
    | throw "missing 'datastar' query parameter"
  let some raw := encoded.decode
    | throw "'datastar' query parameter is not valid percent-encoded UTF-8"
  return raw

/--
The signals in the request body, as the JSON text the browser sent.
-/
def signalsTextFromBody (req : Request Body.Stream) : ContextAsync (Except String String) := do
  try
    let raw : String ← req.body.readAll
    return .ok raw
  catch e =>
    return .error s!"could not read request body: {e}"

/--
The signals the browser sent, as JSON text: from the query for GET and DELETE, from the body
otherwise.
-/
def signalsText (req : Request Body.Stream) : ContextAsync (Except String String) :=
  if req.line.method == .get || req.line.method == .delete then
    pure (signalsTextFromQuery req)
  else
    signalsTextFromBody req

end Datastar
