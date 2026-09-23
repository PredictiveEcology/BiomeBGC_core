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
withr::with_options(c(timeout = 600), Require::Install(
  unique(c(
    SpaDES.core::packages(modules = "BiomeBGC_core", paths = "../..")[[1]],
    "SpaDES.project"
  )),
  repos = unique(c("predictiveecology.r-universe.dev", getOption("repos")))
))

## Paths used by all tests in this suite.
spadesTestPaths <- local({
  root <- tempfile("BiomeBGC_core_test_")
  dir.create(root, recursive = TRUE)
  modulePath <- normalizePath(file.path(getwd(), "..", "..", ".."), mustWork = TRUE)

  list(
    projectPath   = root,
    modulePath    = modulePath,
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
