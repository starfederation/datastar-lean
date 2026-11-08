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

private def sseResponseCore
    (compressors : List Compressor)
    (acceptEncoding : Option String)
    (strategy : CompressionStrategy)
    (callback : ServerSentEventGenerator → ContextAsync Unit) :
    ContextAsync (Response Body.Any) := do

  let ctx ← ContextAsync.getContext

  let callback'   := fun sse => ContextAsync.runIn ctx (callback sse)
  let serialise   := withLock (← Semaphore.new 1)

  let eventStream : SseStream Async ← sseStream callback' serialise compressors acceptEncoding strategy

  let mut builder := Response.ok

  for (name, value) in eventStream.headers do
    let some name' := Header.Name.ofString? name
      | throw (IO.userError s!"invalid header name: {name.quote}")
    let some value' := Header.Value.ofString? value
      | throw (IO.userError s!"invalid {name}: {value.quote}")
    builder := builder.header name' value'

  builder.stream fun out => eventStream.writeBody fun data => out.send { data }

/--
A response that streams SSE events. The connection stays open until `callback` returns.
-/
def sseResponse (callback : ServerSentEventGenerator → ContextAsync Unit) : ContextAsync (Response Body.Any) :=
  sseResponseCore [] none .serverPriority callback

/--
`sseResponse`, compressed with the first of `compressors` that the client accepts; uncompressed
if there is none. See `sseStream`.
-/
def sseResponseWith
    (compressors : List Compressor)
    (req : Request β)
    (callback : ServerSentEventGenerator → ContextAsync Unit)
    (strategy : CompressionStrategy := .serverPriority) :
    ContextAsync (Response Body.Any) :=
  sseResponseCore compressors ((req.line.headers.get? acceptEncoding).map (·.value)) strategy callback

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
otherwise, read with `readBody`. For a server library whose requests have another body type.
-/
def signalsTextWith [Monad m] (req : Request β) (readBody : β → m (Except String String)) :
    m (Except String String) :=
  if req.line.method == .get || req.line.method == .delete then
    pure (signalsTextFromQuery req)
  else
    readBody req.body

/--
The signals the browser sent, as JSON text: from the query for GET and DELETE, from the body
otherwise.
-/
def signalsText (req : Request Body.Stream) : ContextAsync (Except String String) :=
  signalsTextWith req fun _ => signalsTextFromBody req

end Datastar
