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
-/
def buildCLib (pkg : Package) (name : String) (sources : Array FilePath) : FetchM (Job FilePath) := do
  let headers := ← inputDir (pkg.dir / brotliC) (text := true) (·.extension == some "h")
  let compiler := (← IO.getEnv "CC").getD "cc"
  let weakArgs := #[ "-I", (← getLeanIncludeDir).toString
                   , "-I", (pkg.dir / brotliC / "include").toString
                   ]
  -- CLEANUP_ON_OOM: a failed allocation inside the encoder is reported as an error
  -- instead of calling exit().
  let traceArgs := #["-O2", "-DNDEBUG", "-DBROTLI_ENCODER_CLEANUP_ON_OOM"] ++ if Platform.isWindows then #[] else #["-fPIC"]

  let objects ← sources.mapM fun source => do
    -- Make each source job (for a C file) depend on every header. This is overly
    -- cautious but the vendored Brotli sources shouldn't change often and we
    -- don't want to build with incorrect headers.
    let sourceJob := (← inputTextFile (pkg.dir / source)).zipWith (fun source _ => source) headers
    buildO (pkg.buildDir / source.withExtension "o") sourceJob weakArgs traceArgs compiler

  buildStaticLib (pkg.staticLibDir / nameToStaticLib name) objects

target libdatastar_brotli pkg : FilePath := do
  buildCLib pkg "datastar_brotli" <|
    #[("c" : FilePath) / "datastar_brotli.c" ]
      ++ (← cSources pkg (brotliC / "common"))
      ++ (← cSources pkg (brotliC / "enc"))

@[default_target]
lean_lib DatastarBrotli where
  moreLinkObjs := #[libdatastar_brotli]

/-- Test-only: the Brotli decoder, used to check what the encoder produces. -/
target libdatastar_brotli_test pkg : FilePath := do
  buildCLib pkg "datastar_brotli_test" <|
    #[("c" : FilePath) / "datastar_brotli_test.c"]
      ++ (← cSources pkg (brotliC / "dec"))

@[test_driver]
lean_exe brotli_test where
  root := `BrotliTest.Properties
  moreLinkObjs := #[libdatastar_brotli_test]

/-- Serves the page that the Playwright test in `e2e/tests/brotli.spec.ts` drives. -/
lean_exe «brotli-e2e-server» where
  root := `BrotliTest.E2EServer
