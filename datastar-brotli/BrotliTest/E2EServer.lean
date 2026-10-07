import Datastar
import DatastarBrotli

import Std.Async
import Std.Http
import Std.Net.Addr

/-!
Serves the page that the Playwright test in `e2e/tests/brotli.spec.ts` drives.

`/sse/stream` is a Brotli-compressed stream that stops after each event until the test calls
`/release`. The test can then see that the browser has an event while the stream is still open,
which is only true if every event is flushed through the encoder.
-/

open Std Async Http Server
open Datastar

def testPage : String := include_str "e2e-server.html"

/-- A list of `count` items; 3000 of them are well over the encoder's 64 KiB input block. -/
def largeList (count : Nat) : String :=
  let items := (List.range count).map fun i => s!"<li id=\"item-{i}\">Item number {i}</li>"
  s!"<ul id=\"large\">{String.join items}</ul>"

/-- Wait until `/release` has been called `count` times. -/
partial def waitForRelease (released : IO.Ref Nat) (count : Nat) : Async Unit := do
  if (← released.get) < count then
    sleep (.ofNat 20)
    waitForRelease released count

def app (released : IO.Ref Nat) (req : Request Body.Stream) : ContextAsync (Response Body.Any) := do
  match req.line.method, toString req.line.uri.path with
  | .get, "/" => Response.ok |>.html testPage
  | .get, "/release" =>
    released.modify (· + 1)
    Response.ok |>.text "released"
  | .get, "/sse/stream" =>
    let start ← released.get
    sseResponseWith [brotli] req fun sse => do
      sse.send <| patchElements "<div id=\"step\">Event 1</div>"
      waitForRelease released (start + 1)
      sse.send <| patchElements (largeList 3000)
      sse.send <| patchElements "<div id=\"step\">Event 2</div>"
      waitForRelease released (start + 2)
      sse.send <| patchElements "<div id=\"step\">Event 3</div>"
  | _, _ => Response.notFound |>.text "Not found"

def main : IO Unit := Async.block do
  let addr : Std.Net.SocketAddressV4 := ⟨.ofParts 127 0 0 1, 3114⟩
  let released ← IO.mkRef 0

  let server ← Server.serve addr <| Handler.ofFn (app released)

  IO.println "brotli-e2e-server running on http://127.0.0.1:3114"
  (← IO.getStdout).flush
  server.waitShutdown
