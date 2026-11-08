# datastar-leanin

[Datastar](https://data-star.dev/) SSE streams on [LeanIn](https://github.com/scidonia/leanin)'s
`Std.Http` handlers, for the [Datastar Lean SDK](../README.md).

```lean
-- In namespace Datastar.LeanIn
sseResponse     : (Generator VM → VM Unit) → VM (Response ResponseBody)
sseResponseWith : List Compressor → Request β → (Generator VM → VM Unit) → VM (Response ResponseBody)
signalsText     : Request Server.BodyReader → VM (Except String String)
```

Send events with `Generator.send`. A generator is for its handler's task: send from other tasks
through a `Channel`. Decode signals with `decodeSignals`, from `Datastar.LeanJson`.

## Usage

LeanIn needs Lean 4.35 and libuv. Add both packages to your `lakefile.toml`, at the same `rev`,
and link libuv:

```toml
[[require]]
name = "datastar"
git = "https://github.com/starfederation/datastar-lean"
rev = "v1.0.0"

[[require]]
name = "datastar-leanin"
git = "https://github.com/starfederation/datastar-lean"
rev = "v1.0.0"
subDir = "datastar-leanin"

[[lean_exe]]
name = "server"
moreLinkArgs = ["-luv"]
```

```lean
import Datastar
import DatastarLeanIn

open Datastar LeanIn.Runtime LeanIn.Http

structure Signals where
  count : Nat
deriving Lean.FromJson

def handler (req : Std.Http.Request Server.BodyReader) : VM (Std.Http.Response ResponseBody) := do
  match (← LeanIn.signalsText req) >>= decodeSignals (α := Signals) with
  | .error err => LeanIn.sseResponse fun sse => sse.send <| patchElements s!"<p id=\"error\">{err}</p>"
  | .ok signals => LeanIn.sseResponse fun sse =>
      sse.send <| patchElements s!"<div id=\"count\">{signals.count}</div>"
```

With Homebrew, set `C_INCLUDE_PATH=$(brew --prefix)/include` and `LIBRARY_PATH=$(brew --prefix)/lib`.

## Example

```
lake exe fan-out 3000 workers=4
```

Then open <http://127.0.0.1:3000>. Jobs run concurrently on `workers` carriers (default 1), each
racing a deadline; results stream as they finish.

`./smoke.py` builds the example and checks its responses on 1 and 4 carriers.
