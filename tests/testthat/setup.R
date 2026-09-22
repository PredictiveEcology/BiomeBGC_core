if (!testthat::is_testing()) {
  suppressPackageStartupMessages(library(testthat))
  testthat::source_test_helpers(env = globalenv())
}

suppressPackageStartupMessages({
  library(SpaDES.core)
  library(data.table)
})

## Ensure the module's own dependencies (incl. BiomeBGCR, which ships the
## example .ini/.epc/.met/.co2 fixtures used by these tests) are available.
##
## Also pulls in BiomeBGC_dataPrep's dependencies: test-5-integration-*.R in
## this suite chains BiomeBGC_dataPrep + BiomeBGC_core together in a single
## simInit()/spades() call, so both modules' reqdPkgs must be loadable.
## BiomeBGC_dataPrep is expected to be checked out as a sibling folder of
## BiomeBGC_core (as it is in the BiomeBGC workspace), reachable at
## "../../../BiomeBGC_dataPrep" from tests/testthat.
withr::with_options(c(timeout = 600), Require::Require(
  unique(c(
    SpaDES.core::packages(modules = "BiomeBGC_core", paths = "../..")[[1]],
    if (dir.exists("../../../BiomeBGC_dataPrep")) {
      SpaDES.core::packages(modules = "BiomeBGC_dataPrep", paths = "../../..")[[1]]
    }
  )),
  repos = unique(c("predictiveecology.r-universe.dev", getOption("repos")))
))

## Paths used by all tests in this suite.
## Mirrors the naming convention used in CBM_core / LandRCBM_split3pools
## tests, but is built directly (no external test-harness download) since
## BiomeBGC_core has no tests/testthat/testdata folder of its own - fixtures
## instead come from the installed BiomeBGCR package (inst/inputs).
spadesTestPaths <- local({
  root <- tempfile("BiomeBGC_core_test_")
  dir.create(root, recursive = TRUE)
  modulePath <- normalizePath(file.path(getwd(), "..", "..", ".."), mustWork = TRUE)

  list(
    projectPath   = root,
    modulePath    = modulePath,
    # modulePath above already resolves to the BiomeBGC workspace root,
    # i.e. the shared parent of both BiomeBGC_core and BiomeBGC_dataPrep
    # (sibling module folders); kept as its own name for clarity in
    # integration tests that run both modules in one simInit() call.
    sharedModulePath = modulePath,
    packagePath   = file.path(root, "packages"),
    cachePath     = file.path(root, "cache"),
    outputPath    = file.path(root, "outputs"),
    bbgcPath      = file.path(root, "bbgc"),
    # BiomeBGCR ships the example Biome-BGC inputs used as test fixtures
    bbgcInputPath = system.file("inputs", package = "BiomeBGCR")
  )
})

## Sanity check: fixtures must be present (i.e., BiomeBGCR is installed with
## its example inst/inputs data).
if (!nzchar(spadesTestPaths$bbgcInputPath) || !dir.exists(spadesTestPaths$bbgcInputPath)) {
  stop(
    "Could not find BiomeBGCR's example inputs ",
    "(system.file(\"inputs\", package = \"BiomeBGCR\")). ",
    "Make sure BiomeBGCR is installed, e.g. via ",
    "remotes::install_github(\"PredictiveEcology/BiomeBGCR\")."
  )
}
