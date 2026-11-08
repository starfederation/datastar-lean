import Datastar
import DatastarLeanIn

import LeanIn
import LeanIn.Http.StdHandler
import LeanIn.Runtime.Net

open LeanIn LeanIn.Http LeanIn.Runtime
open LeanIn.Task.Sync

open Datastar Datastar.LeanIn

structure Signals where
  jobs : Nat
  deadline : Nat
deriving Lean.FromJson

/-- How one job ended. -/
structure Report where
  index : Nat
  latency : Nat
  finished : Bool

/-- Run a `Task.Async` from `VM`, resuming on the shared route. -/
def awaitAsync (ctx : Task.Ctx) (a : Task.Async α) : VM α :=
  let shared := { ctx with resume := ctx.resumeShared }
  VM.ioE fun resume _ => a.step (fun v _ => resume v) shared

def nextReport (ctx : Task.Ctx) (reports : Channel Report) : VM Report :=
  awaitAsync ctx (Task.Sync.Channel.recv reports)

abbrev Push := Task.Item → BaseIO Unit

/-- Race the work against the deadline and report the winner. -/
def job (ctx : Task.Ctx) (reports : Channel Report) (push : Push) (clock : Clock)
    (index latency deadline : Nat) : VM Unit := do
  let work := Runtime.sleep clock (.ofNat latency)
  let finished := (← awaitAsync ctx (Runtime.withTimeout clock (.ofNat deadline) work)).isSome
  let sent ← (Channel.trySend reports { index, latency, finished } push : IO Bool)
  unless sent do throw (.userError "the report channel is full")

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
    monadLift (VM.run (job ctx reports push clock index latency signals.deadline))
  log "spawned jobs"

  let mut finished := 0
  let mut writing := 0
  for received in [0:signals.jobs] do
    let report ← nextReport ctx reports
    if received == 0 then log "first report"
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
    VM (Std.Http.Response ResponseBody) := do
  match req.line.method, toString req.line.uri.path with
  | .get, "/" => pure <| text .ok "text/html; charset=utf-8" indexHtml
  | .post, "/run" =>
    match (← signalsText req) >>= decodeSignals (α := Signals) with
    | .error _ => rejected "Jobs and deadline must be whole numbers."
    | .ok signals =>
      if signals.jobs == 0 || signals.jobs > maxJobs then
        rejected s!"Jobs must be between 1 and {maxJobs}."
      else
        sseResponse (run push clock ctx signals)
  | _, _ => pure <| text .notFound "text/plain" "Not found"

def main (args : List String) : IO UInt32 := do
  let port := (args.head? >>= String.toNat?).getD 3000
  let workers := (args.findSome? fun a => (a.dropPrefix? "workers=").bind (·.toNat?)).getD 1
  let hooks ← Hooks.new
  -- Binding starts LeanIn's pump thread, which owns the libuv loop. The `workers` carriers run the
  -- steps; a completion on the loop thread only enqueues its continuation for a carrier to take.
  let listener ← Listener.bind (loopback port.toUInt16)
  let executor ← Sched.Executor.new Task.Item 256 workers
  let clock := Clock.live hooks
  let config : Server.Config := { clock, continuePolicy := .application }
  let handler (ctx : Task.Ctx) : StdHandler := { onRequest := app (sharedPush executor) clock ctx }

  IO.println s!"Listening on http://127.0.0.1:{port} with {workers} workers"
  (← IO.getStdout).flush

  let serve : Task.EAsync IO.Error Unit :=
    serveForeverShared hooks executor listener fun conn => do
      let (ctx : Task.Ctx) ← Task.EAsync.ofAsync ⟨fun (k : Task.Ctx → Task.Ctx → IO Unit) (current : Task.Ctx) => k current current⟩
      Server.serveConnStd config conn (handler ctx)

  let (_, outcome) ← Runtime.runCarriersWait executor workers serve
  Runtime.Io.stop
  match outcome with
  | .ok _ => return 0
  | .error err =>
    IO.eprintln s!"The accept loop ended: {err}"
    return 1

