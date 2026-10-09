import Lake
open System Lake DSL

package "datastar-brotli" where
  description := "Brotli compression of Datastar SSE streams."
  version := v!"0.1.0"
  homepage := "https://github.com/starfederation/datastar-lean"
  keywords := #["datastar", "compression", "brotli"]
  license := "MIT"
  builtinLint := true

require datastar from ".."

def brotliC : FilePath := "vendor" / "brotli" / "brotli-1.2.0" / "c"

def cSources (pkg : Package) (dir : FilePath) : IO (Array FilePath) := do
  let names := (← (pkg.dir / dir).readDir).map (·.fileName)
                  |>.filter (·.endsWith ".c")

  return names.qsort (fun x y => x < y)
    |>.map fun (name : String) => dir / name

/--
Compile `sources` (paths relative to the package) with the
system C compiler, or `$CC` if that is defined. Builds a static library.

With `testing`, the sources are compiled with `DATASTAR_BROTLI_TESTING`, which gives the
bindings their allocation counter and fault injection, and the objects go under `testing/` so
that they do not clash with the library's.
-/
def buildCLib (pkg : Package) (name : String) (sources : Array FilePath) (testing := false) : FetchM (Job FilePath) := do
  let headers := ← inputDir (pkg.dir / brotliC) (text := true) (·.extension == some "h")
  let compiler := (← IO.getEnv "CC").getD "cc"
  let weakArgs := #[ "-I", (← getLeanIncludeDir).toString
                   , "-I", (pkg.dir / brotliC / "include").toString
                   ]
  -- CLEANUP_ON_OOM: a failed allocation inside the encoder is reported as an error
  -- instead of calling exit().
  let defines := #["-DNDEBUG", "-DBROTLI_ENCODER_CLEANUP_ON_OOM"] ++ if testing then #["-DDATASTAR_BROTLI_TESTING"] else #[]
  let traceArgs := #["-O2"] ++ defines ++ if Platform.isWindows then #[] else #["-fPIC"]
  let objDir := if testing then pkg.buildDir / "testing" else pkg.buildDir

  let objects ← sources.mapM fun source => do
    -- Make each source job (for a C file) depend on every header. This is overly
    -- cautious but the vendored Brotli sources shouldn't change often and we
    -- don't want to build with incorrect headers.
    let sourceJob := (← inputTextFile (pkg.dir / source)).zipWith (fun source _ => source) headers
    buildO (objDir / source.withExtension "o") sourceJob weakArgs traceArgs compiler

  buildStaticLib (pkg.staticLibDir / nameToStaticLib name) objects

target libdatastar_brotli pkg : FilePath := do
  buildCLib pkg "datastar_brotli" <|
    #[("c" : FilePath) / "datastar_brotli.c" ]
      ++ (← cSources pkg (brotliC / "common"))
      ++ (← cSources pkg (brotliC / "enc"))

@[default_target]
lean_lib DatastarBrotli where
  moreLinkObjs := #[libdatastar_brotli]

/--
Test-only: the bindings with their instrumentation, the Brotli decoder to check what the
encoder produces, and `c/datastar_brotli_test.c`.

Lake links an executable's own `moreLinkObjs` before those of the libraries it imports, so
`brotli_test` resolves the bindings from this archive and never pulls the library's copy in.
Were a Lake release to reverse that order, the link would fail with duplicate symbols.
-/
target libdatastar_brotli_test pkg : FilePath := do
  buildCLib pkg "datastar_brotli_test" (testing := true) <|
    #[("c" : FilePath) / "datastar_brotli.c", ("c" : FilePath) / "datastar_brotli_test.c"]
      ++ (← cSources pkg (brotliC / "dec"))

@[test_driver]
lean_exe brotli_test where
  root := `BrotliTest.Properties
  moreLinkObjs := #[libdatastar_brotli_test]

/-- Serves the page that the Playwright test in `e2e/tests/brotli.spec.ts` drives. -/
lean_exe «brotli-e2e-server» where
  root := `BrotliTest.E2EServer
