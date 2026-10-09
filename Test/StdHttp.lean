import Datastar

/-!
The SSE response over a mock connection, run by `lake test`.

Checked:

* When the callback returns and `finish` fails, the response fails: the connection is dropped
  without the chunked terminator, and `onFailure` sees the error. Ended normally, a compressed
  stream would reach the client cut short, and nothing on the server would show it.
* When `finish` succeeds, its bytes are sent and the response ends normally.
* When the callback fails, that is the error reported, and a failing `finish` is then ignored.
* `sseResponseWith` sends `Vary: Accept-Encoding` whether or not a compressor was chosen, except
  with `.forced`; `sseResponse` never does.
-/

open Std Async Http Server
open Datastar

/-- A compressor that passes bytes through, ending the stream with `finish`. -/
private def passthrough (finish : IO ByteArray) : Compressor where
  encoding := "test"
  start := pure { compress := pure, finish }

private def finishFailure : IO ByteArray := throw (IO.userError "finish failed")

private def callbackFailure : ServerSentEventGenerator → ContextAsync Unit :=
  fun _ => throw (IO.userError "callback failed")

private def oneEvent (sse : ServerSentEventGenerator) : ContextAsync Unit :=
  sse.send <| patchSignals "{\"n\":1}"

/-- A handler that streams `callback`, compressed with `compressor` when the request allows. -/
private def app (compressor : Compressor) (callback : ServerSentEventGenerator → ContextAsync Unit)
    (strategy : CompressionStrategy := .serverPriority) :
    Request Body.Stream → ContextAsync (Response Body.Any) :=
  fun req => sseResponseWith [compressor] req callback strategy

/-- A request for the stream, with `headers` as extra header lines. -/
private def request (headers : String := "") : String :=
  s!"GET /sse HTTP/1.1\r\nHost: example.com\r\n{headers}Connection: close\r\n\r\n"

private def acceptsTest : String := request "Accept-Encoding: test\r\n"

/-- The chunked terminator, which ends a response that completed. -/
private def chunkEnd : String := "0\r\n\r\n"

/-- Serve `request` to `app`: the bytes written to the client, and the errors reported. -/
private def serve (app : Request Body.Stream → ContextAsync (Response Body.Any))
    (request : String := acceptsTest) : IO (String × List String) := do
  let failures ← IO.mkRef ([] : List String)
  let handler := Handler.ofFns app (onFailure := fun e => failures.modify (· ++ [toString e]))
  let (client, server) ← Internal.Mock.new
  let output ← Async.block do
    client.send request.toUTF8
    (serveConnection server handler { lingeringTimeout := 1000, generateDate := false }).run
    client.recv?
  return (String.fromUTF8! (output.getD .empty), ← failures.get)

/-- The status line and headers of the response to `request`. -/
private def responseHead (app : Request Body.Stream → ContextAsync (Response Body.Any))
    (request : String := acceptsTest) : IO String := do
  let (output, _) ← serve app request
  return (output.splitOn "\r\n\r\n").headD ""

private def check (condition : Bool) (message : String) : IO Unit :=
  unless condition do throw <| IO.userError message

private def has (text needle : String) : Bool := (text.splitOn needle).length > 1

private def finishFails : IO Unit := do
  let (output, failures) ← serve (app (passthrough finishFailure) oneEvent)
  check (output.startsWith "HTTP/1.1 200") s!"response: {output.quote}"
  check (has output "data: signals {\"n\":1}") s!"event not sent: {output.quote}"
  check (!output.endsWith chunkEnd) s!"response ended as complete: {output.quote}"
  check (failures == ["finish failed"]) s!"failures reported: {failures}"

private def finishSucceeds : IO Unit := do
  let (output, failures) ← serve (app (passthrough (pure "tail".toUTF8)) oneEvent)
  check (has output "data: signals {\"n\":1}") s!"event not sent: {output.quote}"
  check (output.endsWith s!"tail\r\n{chunkEnd}") s!"final bytes or terminator missing: {output.quote}"
  check (failures == []) s!"failures reported: {failures}"

private def callbackFails : IO Unit := do
  let (_, failures) ← serve (app (passthrough finishFailure) callbackFailure)
  check (failures == ["callback failed"]) s!"failures reported: {failures}"

private def variesOnAcceptEncoding : IO Unit := do
  let identity := passthrough (pure .empty)
  let accepted ← responseHead (app identity oneEvent)
  check (has accepted "Content-Encoding: test" && has accepted "Vary: Accept-Encoding")
    s!"accepted: {accepted.quote}"
  let refused ← responseHead (app identity oneEvent) (request "Accept-Encoding: gzip\r\n")
  check (!has refused "Content-Encoding" && has refused "Vary: Accept-Encoding")
    s!"refused: {refused.quote}"
  let forced ← responseHead (app identity oneEvent (strategy := .forced)) (request "Accept-Encoding: gzip\r\n")
  check (has forced "Content-Encoding: test" && !has forced "Vary")
    s!"forced: {forced.quote}"
  let plain ← responseHead fun _ => sseResponse oneEvent
  check (!has plain "Content-Encoding" && !has plain "Vary")
    s!"plain: {plain.quote}"

/-- The tests of this module, by name. -/
def stdHttpTests : List (String × IO Unit) := [
  ("finish fails after the callback returns", finishFails),
  ("finish succeeds", finishSucceeds),
  ("the callback fails", callbackFails),
  ("negotiated responses vary on Accept-Encoding", variesOnAcceptEncoding)]
