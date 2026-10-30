import Datastar

import LeanIn
import LeanIn.Http.StdHandler
import LeanIn.Runtime.Net

open LeanIn LeanIn.Http LeanIn.Runtime
open LeanIn.Task.Sync

open Datastar

structure Signals where
  jobs : Nat
  deadline : Nat
deriving Lean.FromJson

/-- How one job ended. -/
structure Report where
  index : Nat
  latency : Nat
  finished : Bool

def nextReport (ctx : Task.Ctx) (reports : Channel Report) : VM Report :=
  let shared := { ctx with resume := ctx.resumeShared }
  VM.ioE fun resume _ => (Task.Sync.Channel.recv reports).step (fun report _ => resume report) shared

abbrev Push := Task.Item → BaseIO Unit

/-- Sleep for `ms`, then return `finished`. -/
def after (clock : Clock) (ms : Nat) (finished : Bool) : VM Bool := do
  VM.sleep clock (.ofNat ms)
  return finished

/--
Race the work against the deadline and report the winner. The loser is not cancelled: its timer
fires and nobody reads its cell.

`RawCell` is an unlocked `IO.Ref`, safe only because `main` runs exactly one carrier.
-/
def job (reports : Channel Report) (push : Push) (clock : Clock)
    (index latency deadline : Nat) : VM Unit := do
  let finished ← VM.ioE fun resume _ => do
    let work ← RawCell.new
    let timer ← RawCell.new
    VM.runToCell (after clock latency true) work
    VM.runToCell (after clock deadline false) timer
    selectRaw work timer fun (_, finished) => resume finished
  let sent ← (Channel.trySend reports { index, latency, finished } push : IO Bool)
  unless sent do throw (.userError "the report channel is full")

/--
The `Std.Http` response of an SSE stream on LeanIn. Only `callback` writes to the stream, so the
generator has nothing to serialise.
-/
def sseResponse (callback : Generator VM → VM Unit) : VM (Std.Http.Response ResponseBody) := do
  let headers := (sseHeaders).foldl (init := Std.Http.Headers.empty) fun acc (name, value) =>
    match Std.Http.Header.Name.ofString? name, Std.Http.Header.Value.ofString? value with
    | some name, some value => acc.insert name value
    | _, _ => acc
  return {
    line := { status := .ok, headers }
    body := .stream none fun write => do
      let sse ← (monadLift (Generator.new write id : IO (Generator VM)) : VM (Generator VM))
      callback sse
      sse.finish
  }

def row (id : Nat) (state : String) : String :=
  s!"<li id='job-{id}'>job {id}: {state}</li>"

def outcome (report : Report) (deadline : Nat) : String :=
  if report.finished then
    s!"<span class='text-green-500'>finished in {report.latency} ms</span>"
  else
    s!"<span class='text-red-500'>timed out at {deadline} ms, wanted {report.latency} ms</span>"

def nowMs : VM Nat := IO.monoMsNow

def logStage (jobs started : Nat) (stage : String) : VM Unit := do
  let now ← nowMs
  monadLift (IO.eprintln s!"jobs={jobs} +{now - started}ms {stage}" : IO Unit)

/-- Spawn every job, then stream each report as it arrives. -/
def run (push : Push) (clock : Clock) (ctx : Task.Ctx) (signals : Signals) (sse : Generator VM) : VM Unit := do
  let started ← nowMs
  let log := logStage signals.jobs started
  let rows := (List.range signals.jobs).map (row · "<span class='text-gray-400'>running</span>")
  let page := s!"<ol id='results' class='list-decimal list-inside'>{String.join rows}</ol>"
  log s!"rendered rows, {page.utf8ByteSize} bytes"
  sse.send <| patchElements page
  sse.send <| patchElements "<p id='summary'></p>"
  log "sent rows"

  let reports ← (monadLift (Task.Sync.Channel.new Report signals.jobs) : VM (Channel Report))
  let mut work := 0
  for index in [0:signals.jobs] do
    let latency ← (monadLift (IO.rand 100 1500 : IO Nat) : VM Nat)
    work := work + latency
    monadLift (VM.run (job reports push clock index latency signals.deadline))
  log "spawned jobs"

  let mut finished := 0
  let mut writing := 0
  for _ in [0:signals.jobs] do
    let report ← nextReport ctx reports
    if finished == 0 then log "first report"
    if report.finished then finished := finished + 1
    let before ← nowMs
    sse.send <| patchElements (row report.index (outcome report signals.deadline))
    writing := writing + (← nowMs) - before

  log s!"last report, {writing} ms writing them"
  let elapsed := (← nowMs) - started
  sse.send <| patchElements
    s!"<p id='summary'>{finished} of {signals.jobs} finished: {work} ms of work in {elapsed} ms.</p>"

/-- The most jobs one run may start. `fan-out.html` and `smoke.py` repeat it. -/
def maxJobs : Nat := 50000

def indexHtml : String := include_str "fan-out.html"

def text (status : Std.Http.Status) (contentType body : String) : Std.Http.Response ResponseBody :=
  let name := Std.Http.Header.Name.contentType
  let headers := (Std.Http.Header.Value.ofString? contentType).map (Std.Http.Headers.empty.insert name)
  { line := { status, headers := headers.getD .empty }, body := .bytes body.toUTF8 }

/-- Show `message` on the page. The browser would ignore the body of an error status. -/
def rejected (message : String) : VM (Std.Http.Response ResponseBody) :=
  sseResponse fun sse => do
    sse.send <| patchElements "<ol id='results' class='list-decimal list-inside'></ol>"
    sse.send <| patchElements s!"<p id='summary' class='text-red-500'>{message}</p>"

def app (push : Push) (clock : Clock) (ctx : Task.Ctx) (req : Std.Http.Request Server.BodyReader) :
    VM (Std.Http.Response ResponseBody) :=
  match req.line.method, toString req.line.uri.path with
  | .get, "/" => pure <| text .ok "text/html; charset=utf-8" indexHtml
  | .get, "/run" =>
    match signalsFromQuery (α := Signals) req with
    | .error _ => rejected "Jobs and deadline must be whole numbers."
    | .ok signals =>
      if signals.jobs == 0 || signals.jobs > maxJobs then
        rejected s!"Jobs must be between 1 and {maxJobs}."
      else
        sseResponse (run push clock ctx signals)
  | _, _ => pure <| text .notFound "text/plain" "Not found"

def main (args : List String) : IO UInt32 := do
  let port := (args.head? >>= String.toNat?).getD 3000
  let hooks ← Hooks.new
  let listener ← Listener.bind (loopback port.toUInt16)
  -- Exactly one carrier: `Runtime.run` serves the one queue on this thread and drives the libuv loop
  -- itself, so every step, timer and socket callback runs here. That is what makes `RawCell` safe.
  let executor ← Sched.Executor.new Task.Item 256 1
  let clock := Clock.live hooks
  let config : Server.Config := { clock, continuePolicy := .application }
  let handler (ctx : Task.Ctx) : StdHandler := { onRequest := app (sharedPush executor) clock ctx }

  IO.println s!"Listening on http://127.0.0.1:{port}"
  (← IO.getStdout).flush

  let serve : Task.EAsync IO.Error Unit :=
    serveForever hooks listener fun conn => do
      let (ctx : Task.Ctx) ← Task.EAsync.ofAsync ⟨fun (k : Task.Ctx → Task.Ctx → IO Unit) (current : Task.Ctx) => k current current⟩
      Server.serveConnStd config conn (handler ctx)

  match ← (Runtime.run executor serve : IO (Except IO.Error Unit)) with
  | .ok _ => return 0
  | .error err =>
    IO.eprintln s!"The accept loop ended: {err}"
    return 1

