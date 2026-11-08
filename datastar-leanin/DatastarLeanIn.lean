module

public import Datastar.Core
public import LeanIn.Http.StdHandler

public section

/-!
SSE streaming on [LeanIn](https://github.com/scidonia/leanin)'s `Std.Http` handlers, and the
signals a request carries.

```lean
def handler (req : Std.Http.Request Server.BodyReader) : VM (Std.Http.Response ResponseBody) :=
  LeanIn.sseResponse fun sse =>
    sse.send <| patchElements "<div id=\"hello\">Hello, world!</div>"
```

The generator is for the handler's own task: send from other tasks through a `Channel`.
-/

open LeanIn.Runtime (VM)
open LeanIn.Http (ResponseBody)
open LeanIn.Http.Server (BodyReader)

namespace Datastar.LeanIn

private def response (stream : SseStream VM) : IO (Std.Http.Response ResponseBody) := do
  let mut headers := Std.Http.Headers.empty
  for (name, value) in stream.headers do
    let some name' := Std.Http.Header.Name.ofString? name
      | throw (IO.userError s!"invalid header name: {name.quote}")
    let some value' := Std.Http.Header.Value.ofString? value
      | throw (IO.userError s!"invalid {name}: {value.quote}")
    headers := headers.insert name' value'
  return { line := { status := .ok, headers }, body := .stream none stream.writeBody }

/--
A response that streams SSE events. The connection stays open until `callback` returns.
-/
def sseResponse (callback : Generator VM → VM Unit) : VM (Std.Http.Response ResponseBody) :=
  (do response (← sseStream callback) : IO _)

/--
`sseResponse`, compressed with the first of `compressors` that the client accepts; uncompressed
if there is none.
-/
def sseResponseWith
    (compressors : List Compressor)
    (req : Std.Http.Request β)
    (callback : Generator VM → VM Unit)
    (strategy : CompressionStrategy := .serverPriority) :
    VM (Std.Http.Response ResponseBody) :=
  let acceptEncoding := (req.line.headers.get? (.mk "accept-encoding")).map (·.value)
  (do response (← sseStream callback (compressors := compressors) (acceptEncoding := acceptEncoding)
    (strategy := strategy)) : IO _)

/--
The signals in the request body, as the JSON text the browser sent.
-/
def signalsTextFromBody (req : Std.Http.Request BodyReader) : VM (Except String String) := do
  let raw ← req.body.readAll
  match String.fromUTF8? raw with
  | some text => return .ok text
  | none => return .error "the request body is not valid UTF-8"

/--
The signals the browser sent, as JSON text: from the query for GET and DELETE, from the body
otherwise. Decode it with `decodeSignals`, or any JSON library.
-/
def signalsText (req : Std.Http.Request BodyReader) : VM (Except String String) :=
  signalsTextWith req fun _ => signalsTextFromBody req

end Datastar.LeanIn
