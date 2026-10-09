import Test.Unit
import Test.StdHttp

/-- Run by `lake test`. The `#guard`s of `Test.Unit` are checked as it compiles. -/
def main : IO UInt32 := do
  let mut failures := 0
  for (name, test) in stdHttpTests do
    try
      test
      IO.println s!"datastar: {name}: ok"
    catch e =>
      IO.eprintln s!"FAILED {name}: {e}"
      failures := failures + 1
  return if failures == 0 then 0 else 1
