import Datastar

/-!
`Generator` over a transport that collects what is written, run by `lake test`.

Checked:

* `send` writes the rendered event, and `finish` adds nothing to an uncompressed stream.
* A compressed stream writes the encoder's output, ends with the trailing bytes of `finish`, and
  refuses further events.
* `send` runs under `serialise`.
-/

open Datastar

private def collect : IO (IO.Ref (Array String) × (ByteArray → IO Unit)) := do
  let written ← IO.mkRef #[]
  return (written, fun bytes => written.modify (·.push (String.fromUTF8! bytes)))

private def expect [BEq α] [ToString α] (name : String) (got want : α) : IO Unit :=
  unless got == want do
    throw (IO.userError s!"{name}: expected {want}, got {got}")

private def uncompressed : IO Unit := do
  let (written, write) ← collect
  let sse ← Generator.new write id
  let event := patchSignals "{\"n\":1}"
  sse.send event
  sse.finish
  expect "written" (← written.get) #[render event]

private def compressed : IO Unit := do
  let (written, write) ← collect
  let encoder : Encoder := { compress := fun _ => pure "packed".toUTF8, finish := pure "end".toUTF8 }
  let sse ← Generator.new write id encoder
  sse.send <| patchSignals "{\"n\":1}"
  sse.finish
  expect "written" (← written.get) #[("packed" : String), "end"]
  match ← (sse.send <| patchSignals "{\"n\":2}").toBaseIO with
  | .ok _ => throw (IO.userError "send succeeded after finish")
  | .error _ => expect "written" (← written.get).size 2

private def serialised : IO Unit := do
  let (written, write) ← collect
  let entered ← IO.mkRef 0
  let sse ← Generator.new write fun action => do
    entered.modify (· + 1)
    action
  sse.send <| patchSignals "{\"n\":1}"
  expect "entered" (← entered.get) 1
  expect "writes" (← written.get).size 1

/-- The tests in this file, for `Test/Main.lean`. -/
def generatorTests : List (String × IO Unit) := [
  ("an uncompressed stream", uncompressed),
  ("a compressed stream", compressed),
  ("send runs under serialise", serialised)]
