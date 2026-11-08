import Datastar

/-!
`sseStream` over a transport that collects what is written, run by `lake test`.

Checked:

* An uncompressed stream writes each rendered event and nothing more.
* A compressed stream writes the encoder's output and ends with the trailing bytes of `finish`.
* The headers name the chosen encoding, with `Vary` when the choice was negotiated.
* The stream ends once, whether `callback` returns or throws, and `send` after it throws.
* `send` runs under `serialise`.
-/

open Datastar

private def collect : IO (IO.Ref (Array String) × (ByteArray → IO Unit)) := do
  let written ← IO.mkRef #[]
  return (written, fun bytes => written.modify (·.push (String.fromUTF8! bytes)))

private def expect [BEq α] [ToString α] (name : String) (got want : α) : IO Unit :=
  unless got == want do
    throw (IO.userError s!"{name}: expected {want}, got {got}")

/-- A compressor whose output names its calls, counting `finish`. -/
private def packer (finishes : IO.Ref Nat) : Compressor where
  encoding := "pack"
  start := pure {
    compress := fun _ => pure "packed".toUTF8
    finish := do finishes.modify (· + 1); pure "end".toUTF8 }

private def uncompressed : IO Unit := do
  let (written, write) ← collect
  let event := patchSignals "{\"n\":1}"
  let stream ← sseStream (m := IO) (·.send event)
  expect "headers" stream.headers
    [("cache-control", "no-cache"), ("content-type", "text/event-stream")]
  stream.writeBody write
  expect "written" (← written.get) #[render event]

private def compressed : IO Unit := do
  let (written, write) ← collect
  let finishes ← IO.mkRef 0
  let stream ← sseStream (m := IO) (·.send <| patchSignals "{\"n\":1}")
    (compressors := [packer finishes]) (acceptEncoding := some "gzip, pack")
  expect "content-encoding" (stream.headers.lookup "content-encoding") (some "pack")
  expect "vary" (stream.headers.lookup "vary") (some "Accept-Encoding")
  stream.writeBody write
  expect "written" (← written.get) #[("packed" : String), "end"]
  expect "finishes" (← finishes.get) 1

private def unaccepted : IO Unit := do
  let finishes ← IO.mkRef 0
  let stream ← sseStream (m := IO) (fun _ => pure ())
    (compressors := [packer finishes]) (acceptEncoding := some "gzip")
  expect "content-encoding" (stream.headers.lookup "content-encoding") none
  expect "vary" (stream.headers.lookup "vary") (some "Accept-Encoding")

private def failing : IO Unit := do
  let (written, write) ← collect
  let finishes ← IO.mkRef 0
  let stream ← sseStream (m := IO) (fun _ => throw (IO.userError "boom"))
    (compressors := [packer finishes]) (acceptEncoding := some "pack")
  match ← (stream.writeBody write).toBaseIO with
  | .ok _ => throw (IO.userError "the callback's error was dropped")
  | .error e => expect "error" (toString e) "boom"
  expect "written" (← written.get) #[("end" : String)]
  expect "finishes" (← finishes.get) 1

private def sendAfterEnd : IO Unit := do
  let (written, write) ← collect
  let kept ← IO.mkRef (none : Option (Generator IO))
  let stream ← sseStream (m := IO) fun sse => kept.set (some sse)
  stream.writeBody write
  let some sse ← kept.get | throw (IO.userError "the callback did not run")
  match ← (sse.send <| patchSignals "{\"n\":2}").toBaseIO with
  | .ok _ => throw (IO.userError "send succeeded after the stream ended")
  | .error _ => expect "written" (← written.get).size 0

private def serialised : IO Unit := do
  let (written, write) ← collect
  let entered ← IO.mkRef 0
  let serialise (action : IO Unit) : IO Unit := do
    entered.modify (· + 1)
    action
  let stream ← sseStream (·.send <| patchSignals "{\"n\":1}") serialise
  stream.writeBody write
  -- Once for `send`, once for the end of the stream.
  expect "entered" (← entered.get) 2
  expect "writes" (← written.get).size 1

/-- The tests in this file, for `Test/Main.lean`. -/
def generatorTests : List (String × IO Unit) := [
  ("an uncompressed stream", uncompressed),
  ("a compressed stream", compressed),
  ("an encoding the client does not accept", unaccepted),
  ("a callback that throws", failing),
  ("send after the stream ends", sendAfterEnd),
  ("send runs under serialise", serialised)]
