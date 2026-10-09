import DatastarBrotli

/-!
Tests for the Brotli bindings, run with `lake test`.

Most cases are generated from a seed. A failure prints the seed of the case, which reruns alone
with `lake exe brotli_test 1 <seed>`. `lake exe brotli_test <cases>` runs more cases, and
`--no-soak` skips the soak, which measures memory use and so cannot run under a sanitizer.

The bindings are compiled with `DATASTAR_BROTLI_TESTING` for this executable, which adds the
allocation counter and fault injection used below.

Checked:

* Flush completeness: after every `compress`, the output so far decodes to exactly the input so
  far. An SSE event must not sit in the encoder until the next one arrives.
* A model of the stream: `compress` after `finish` fails, a second `finish` is empty, and a
  stream may be dropped at any point.
* No leaks: once a stream is dropped, the C side has no live allocations.
* Allocation failures: failing each allocation of a stream in turn gives an error every time,
  and never a crash or a leak.
* Soak: memory use is stable over many calls, which catches leaked Lean objects.
* Parameters: a `quality` or `windowLog` out of range is a compile error.
-/

open Datastar

/-- Decode as much of `bytes` as possible; the flag says whether the stream has ended. -/
@[extern "datastar_brotli_test_decode"]
private opaque decodePrefix (bytes : @& ByteArray) : IO (ByteArray × Bool)

/-- Allocations currently held by the C side of the bindings. -/
@[extern "datastar_brotli_live_allocations"]
private opaque liveAllocations (_ : Unit) : IO Nat

/--
Make the `n`th allocation on the C side from now fail, or none if `n` is zero. Returns the
previous setting, which is zero once that failure has happened.
-/
@[extern "datastar_brotli_fail_allocation"]
private opaque failAllocation (n : USize) : IO Nat

private def check (condition : Bool) (message : String) : IO Unit :=
  unless condition do throw <| IO.userError message

private def same (a b : ByteArray) : Bool := a.data == b.data

/-- Run `action`, which must fail with a message containing `fragment`. -/
private def expectError (what fragment : String) (action : IO α) : IO Unit := do
  let message ← try
      discard action
      pure none
    catch e => pure (some (toString e))
  match message with
  | none => throw <| IO.userError s!"{what}: expected an error"
  | some message =>
    check ((message.splitOn fragment).length > 1) s!"{what}: unexpected error: {message}"

/-! ## Generators -/

/-- A generator is a random number generator state over `IO`. -/
private abbrev Gen := StateT UInt64 IO

/-- SplitMix64: well distributed even for consecutive seeds, which is how cases are numbered. -/
private def next : Gen UInt64 :=
  modifyGet fun (s : UInt64) =>
    let s := s + 0x9E3779B97F4A7C15
    let z := (s ^^^ (s >>> 30)) * 0xBF58476D1CE4E5B9
    let z := (z ^^^ (z >>> 27)) * 0x94D049BB133111EB
    (z ^^^ (z >>> 31), s)

/-- A number in `[0, n)`; `n` must be positive. -/
private def below (n : Nat) : Gen Nat := return (← next).toNat % n

private def oneOf [Inhabited α] (xs : Array α) : Gen α := return xs[← below xs.size]!

private structure Params where
  quality : Nat
  windowLog : Nat
  mode : BrotliMode

private instance : ToString Params where
  toString p := s!"quality {p.quality}, windowLog {p.windowLog}, mode {repr p.mode}"

/--
Any valid parameters. Quality 2 and window 10 are favoured: they make the encoder hand back its
output in several pieces, which is the hard case for the bindings.
-/
private def genParams : Gen Params := do
  let quality ← if (← below 4) == 0 then pure 2 else below 12
  let windowLog ← if (← below 4) == 0 then pure 10 else (10 + ·) <$> below 15
  let mode := match ← below 3 with
    | 0 => BrotliMode.generic
    | 1 => .text
    | _ => .font
  return { quality, windowLog, mode }

/-- A chunk size, favouring the edges of the encoder's 64 KiB input block. -/
private def genSize (large : Bool) : Gen Nat := do
  match ← below 10 with
  | 0 => pure 0
  | 1 => pure 1
  | 2 => if large then oneOf #[65535, 65536, 65537] else below 300
  | 3 => if large then oneOf #[131071, 131072, 131073, 200000] else below 300
  | 4 => if large then below 100000 else below 5000
  | _ => below 3000

/-- Bytes that look like SSE events. -/
private def genText (size : Nat) : Gen ByteArray := do
  let mut bytes := ByteArray.emptyWithCapacity size
  while bytes.size < size do
    let line := s!"data: elements <div id=\"item-{← below 50}\" class=\"row\">{← below 1000000}</div>\n"
    bytes := bytes ++ line.toUTF8
  return bytes.extract 0 size

/-- Bytes that compress very well, not at all, or like SSE events. -/
private def genBytes (size : Nat) : Gen ByteArray := do
  let mut bytes := ByteArray.emptyWithCapacity size
  match ← below 4 with
  | 0 =>
    for _ in [0:size] do
      bytes := bytes.push 0
  | 1 =>
    for _ in [0:size] do
      bytes := bytes.push (← next).toUInt8
  | _ => bytes ← genText size
  return bytes

/-- `Params` come from a generator, so the range proofs are made here, as code with values from a
configuration file would. -/
private def start (p : Params) : IO Encoder :=
  if hq : p.quality ≤ 11 then
    if hw : 10 ≤ p.windowLog ∧ p.windowLog ≤ 24 then
      (brotli p.quality p.windowLog p.mode hq hw).start
    else throw <| IO.userError s!"{p}: windowLog is out of range"
  else throw <| IO.userError s!"{p}: quality is out of range"

/-- The slowest qualities get small inputs, to keep the run short. -/
private def genChunk (p : Params) : Gen ByteArray := do
  genBytes (← genSize (large := p.quality < 10))

/-! ## Properties -/

/-- After each `compress` the client can decode everything sent so far, and `finish` ends the stream. -/
private def flushCase : Gen Unit := do
  let p ← genParams
  let encoder ← start p
  let mut sent := ByteArray.empty
  let mut wire := ByteArray.empty
  for step in [0:1 + (← below 5)] do
    let chunk ← genChunk p
    wire := wire ++ (← encoder.compress chunk)
    sent := sent ++ chunk
    let (decoded, finished) ← decodePrefix wire
    check (same decoded sent)
      s!"{p}: after chunk {step} of {chunk.size} bytes, decoded {decoded.size} of {sent.size} bytes"
    check (!finished) s!"{p}: stream ended after chunk {step}"
  wire := wire ++ (← encoder.finish)
  let (decoded, finished) ← decodePrefix wire
  check (same decoded sent) s!"{p}: after finish, decoded {decoded.size} of {sent.size} bytes"
  check finished s!"{p}: stream did not end after finish"

/-- A random sequence of calls behaves like the model; the stream is then dropped as it is. -/
private def modelCase : Gen Unit := do
  let p ← genParams
  let encoder ← start p
  let mut sent := ByteArray.empty
  let mut wire := ByteArray.empty
  let mut ended := false
  for step in [0:← below 12] do
    if (← below 4) == 0 then
      let output ← encoder.finish
      if ended then
        check (output.size == 0) s!"{p}: step {step}: second finish returned {output.size} bytes"
      else
        check (output.size > 0) s!"{p}: step {step}: finish returned nothing"
      wire := wire ++ output
      ended := true
    else
      let chunk ← genBytes (← genSize (large := false))
      if ended then
        expectError s!"{p}: step {step}: compress after finish" "ended" (encoder.compress chunk)
      else
        wire := wire ++ (← encoder.compress chunk)
        sent := sent ++ chunk
    let (decoded, finished) ← decodePrefix wire
    check (same decoded sent) s!"{p}: step {step}: decoded {decoded.size} of {sent.size} bytes"
    check (finished == ended) s!"{p}: step {step}: stream ended is {finished}, expected {ended}"

/-! ## Parameters out of range are compile errors -/

/--
error: could not synthesize default value for parameter 'hq' using tactics
---
error: Tactic `decide` proved that the proposition
  12 ≤ 11
is false
-/
#guard_msgs in
example : Compressor := brotli (quality := 12)

/--
error: could not synthesize default value for parameter 'hw' using tactics
---
error: Tactic `decide` proved that the proposition
  10 ≤ 9 ∧ 9 ≤ 24
is false
-/
#guard_msgs in
example : Compressor := brotli (windowLog := 9)

/--
error: could not synthesize default value for parameter 'hw' using tactics
---
error: Tactic `decide` proved that the proposition
  10 ≤ 25 ∧ 25 ≤ 24
is false
-/
#guard_msgs in
example : Compressor := brotli (windowLog := 25)

/-! ## Fixed cases -/

private def fixedCases : IO Unit := do
  -- An open stream holds allocations: the leak check can see them.
  let encoder ← (brotli).start
  check ((← liveAllocations ()) > 0) "an open stream has no live allocations"

  -- A stream with no events is still a valid, empty stream.
  let (decoded, finished) ← decodePrefix (← encoder.finish)
  check (decoded.size == 0 && finished) "an empty stream did not decode to nothing"

  -- Finishing releases the encoder, leaving only the stream's own state. The second
  -- `finish` keeps the stream alive across the check.
  let live ← liveAllocations ()
  check (live == 1) s!"{live} allocations live after finish, expected 1"
  check ((← encoder.finish).size == 0) "second finish returned bytes"

/-! ## Allocation failures -/

/--
One stream's worth of calls. Every call is attempted even after one has failed, as a caller that
ignores errors would. Returns the bytes produced and whether any call failed.
-/
private def allCalls (p : Params) (chunks : Array ByteArray) : IO (ByteArray × Bool) := do
  let mut wire := ByteArray.empty
  let mut failed := false
  let encoder ← try start p catch _ => return (wire, true)
  for chunk in chunks do
    try wire := wire ++ (← encoder.compress chunk) catch _ => failed := true
  try wire := wire ++ (← encoder.finish) catch _ => failed := true
  return (wire, failed)

/--
Fail the first allocation of a stream, then the second, and so on, until the stream completes
without reaching the failing one. Each failure must surface as an error, and never as a crash, a
leak or a stream that claims to have succeeded. Returns how many failures were injected.
-/
private def allocationFailures (p : Params) (chunks : Array ByteArray) : IO Nat := do
  let sent := chunks.foldl (· ++ ·) ByteArray.empty
  for n in [1:100000] do
    discard <| failAllocation n.toUSize
    let (wire, failed) ← allCalls p chunks
    let untouched := (← failAllocation 0) > 0
    let live ← liveAllocations ()
    check (live == 0) s!"{p}: failing allocation {n} left {live} allocations live"
    if untouched then
      check (!failed) s!"{p}: a call failed with no allocation failure"
      let (decoded, finished) ← decodePrefix wire
      check (same decoded sent && finished) s!"{p}: stream is wrong after earlier failures"
      return n - 1
    check failed s!"{p}: allocation {n} failed but no call reported an error"
  throw <| IO.userError s!"{p}: still allocating after 100000 failures"

private def allocationFailureCases : IO Nat := do
  let mut injected := 0
  for (quality, windowLog) in [(0, 10), (1, 16), (2, 12), (5, 24), (9, 18), (11, 10)] do
    let p : Params := { quality, windowLog, mode := .text }
    -- The middle chunk spans more than one of the encoder's input blocks.
    let large := if quality < 10 then 70000 else 5000
    let (chunks, _) ← (#[3000, large, 100].mapM genText).run quality.toUInt64
    injected := injected + (← allocationFailures p chunks)
  return injected

/-! ## Soak -/

/-- Resident memory of this process, in kilobytes. -/
private def residentKb : IO Nat := do
  let pid ← IO.Process.getPID
  let out ← IO.Process.output { cmd := "ps", args := #["-o", "rss=", "-p", toString pid] }
  match out.stdout.trimAscii.toString.toNat? with
  | some kb => return kb
  | none => throw <| IO.userError s!"soak: cannot read memory use from ps: {out.stdout}{out.stderr}"

/--
Run `action` many times, and fail if resident memory keeps growing after a warm-up. The C-side
counter cannot see Lean objects, such as a result or an error that is never released; this can.
-/
private def soak (name : String) (iterations limitKb : Nat) (action : Nat → IO Unit) : IO Unit := do
  for i in [0:iterations / 5] do action i
  let before ← residentKb
  for i in [0:iterations] do action i
  let after ← residentKb
  check (after ≤ before + limitKb)
    s!"soak {name}: memory grew from {before} kB to {after} kB over {iterations} calls"

private def soakCases : IO Unit := do
  let (text, _) ← (genText 16384).run 1
  let p : Params := { quality := 1, windowLog := 18, mode := .text }

  -- A leaked chunk or result costs kilobytes per call. Each call builds its own chunk, which
  -- depends on `i` so that it cannot be computed once and shared: a leaked reference to a
  -- shared chunk would not use any more memory.
  let encoder ← start p
  soak "compress" 10000 20000 fun i => do
    discard <| encoder.compress (text.push i.toUInt8)

  -- A leaked error is about a hundred bytes, so this needs many more calls.
  discard encoder.finish
  soak "compress after finish" 1000000 20000 fun _ => do
    try discard <| encoder.compress text catch _ => pure ()

  soak "whole streams" 5000 20000 fun _ => do
    let encoder ← start p
    discard <| encoder.compress text
    discard encoder.finish

/-! ## Runner -/

/-- Run one generated case from `seed`, then check that it leaked nothing. -/
private def runCase (name : String) (case : Gen Unit) (seed : UInt64) : IO Bool := do
  try
    discard <| case.run seed
    let live ← liveAllocations ()
    check (live == 0) s!"{live} allocations still live after the stream was dropped"
    return true
  catch e =>
    IO.eprintln s!"FAILED {name}, seed {seed}: {e}"
    return false

/-- Run a named group of checks, reporting a failure instead of stopping. -/
private def runGroup (name : String) (group : IO String) : IO Bool := do
  try
    let summary ← group
    check ((← liveAllocations ()) == 0) "allocations left behind"
    IO.println s!"brotli: {name}: {summary}"
    return true
  catch e =>
    -- An allocation failure must not stay armed for the next group.
    discard <| failAllocation 0
    IO.eprintln s!"FAILED {name}: {e}"
    return false

def main (args : List String) : IO UInt32 := do
  let withSoak := !args.contains "--no-soak"
  let args := args.filter (· != "--no-soak")
  let cases := (args[0]? >>= String.toNat?).getD 300
  let seed := (args[1]? >>= String.toNat?).getD 1
  let mut failures := 0

  unless ← runGroup "fixed cases" (do fixedCases; pure "passed") do failures := failures + 1

  for i in [0:cases] do
    -- Later failures are usually the same bug again.
    if failures ≥ 10 then
      IO.eprintln "brotli: stopping after 10 failures"
      break
    let seed := (seed + i).toUInt64
    unless ← runCase "flush" flushCase seed do failures := failures + 1
    unless ← runCase "model" modelCase seed do failures := failures + 1
  if failures == 0 then
    IO.println s!"brotli: generated cases: {cases} of each property passed"

  unless ← runGroup "allocation failures"
      (do pure s!"{← allocationFailureCases} injected, all reported as errors") do
    failures := failures + 1

  if withSoak then
    unless ← runGroup "soak" (do soakCases; pure "memory use is stable") do failures := failures + 1

  if failures == 0 then
    return 0
  else
    IO.eprintln s!"brotli: {failures} failures"
    return 1
