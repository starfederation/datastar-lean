import Datastar

/-!
The SSE response over a mock connection, run by `lake test`.

Checked:

* When the callback returns and `finish` fails, the response fails: the connection is dropped
  without the chunked terminator, and `onFailure` sees the error. Ended normally, a compressed
  stream would reach the client cut short, and nothing on the server would show it.
* When `finish` succeeds, its bytes are sent and the response ends normally.
* When the callback fails, that is the error reported, and a failing `finish` is then ignored.
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

private def request : String :=
  "GET /sse HTTP/1.1\r\nHost: example.com\r\nAccept-Encoding: test\r\nConnection: close\r\n\r\n"

/-- The chunked terminator, which ends a response that completed. -/
private def chunkEnd : String := "0\r\n\r\n"

/-- Serve `request` to an SSE handler: the bytes written to the client, and the errors reported. -/
private def serve (compressor : Compressor) (callback : ServerSentEventGenerator → ContextAsync Unit) :
    IO (String × List String) := do
  let failures ← IO.mkRef ([] : List String)
  let handler := Handler.ofFns (fun req => sseResponseWith [compressor] req callback)
    (onFailure := fun e => failures.modify (· ++ [toString e]))
  let (client, server) ← Internal.Mock.new
  let output ← Async.block do
    client.send request.toUTF8
    (serveConnection server handler { lingeringTimeout := 1000, generateDate := false }).run
    client.recv?
  return (String.fromUTF8! (output.getD .empty), ← failures.get)

private def check (condition : Bool) (message : String) : IO Unit :=
  unless condition do throw <| IO.userError message

private def has (text needle : String) : Bool := (text.splitOn needle).length > 1

private def finishFails : IO Unit := do
  let (output, failures) ← serve (passthrough finishFailure) oneEvent
  check (output.startsWith "HTTP/1.1 200") s!"response: {output.quote}"
  check (has output "data: signals {\"n\":1}") s!"event not sent: {output.quote}"
  check (!output.endsWith chunkEnd) s!"response ended as complete: {output.quote}"
  check (failures == ["finish failed"]) s!"failures reported: {failures}"

private def finishSucceeds : IO Unit := do
  let (output, failures) ← serve (passthrough (pure "tail".toUTF8)) oneEvent
  check (has output "data: signals {\"n\":1}") s!"event not sent: {output.quote}"
  check (output.endsWith s!"tail\r\n{chunkEnd}") s!"final bytes or terminator missing: {output.quote}"
  check (failures == []) s!"failures reported: {failures}"

private def callbackFails : IO Unit := do
  let (_, failures) ← serve (passthrough finishFailure) callbackFailure
  check (failures == ["callback failed"]) s!"failures reported: {failures}"

/-- The tests of this module, by name. -/
def stdHttpTests : List (String × IO Unit) := [
  ("finish fails after the callback returns", finishFails),
  ("finish succeeds", finishSucceeds),
  ("the callback fails", callbackFails)]
