if (!testthat::is_testing()) {
  source(testthat::test_path("setup.R"))
}

## Integration tests chaining BiomeBGC_dataPrep + BiomeBGC_core in a single
## simInit()/spades() call: BiomeBGC_dataPrep prepares .ini files (and
## pixelGroupParameters/pixelGroupMap) from small, fully offline toy inputs,
## and BiomeBGC_core consumes those .ini files directly to run a simulation,
## in the same simList. Mirrors the module-integration test patterns used in
## CBM_core and LandRCBM_split3pools (tests/testthat), and the toy-input
## builders already used by BiomeBGC_dataPrep's own integration tests
## (test-5/6-integration-*.R), duplicated locally in
## helper-mockDataPrepInputs.R so this suite stays self-contained.
##
## See BiomeBGC_dataPrep's test-5-integration-point.R for why these tests
## require network access even though every other input is mocked:
## prepareSpinupIni() unconditionally calls getOutputDescription(), which
## fetches from GitHub with no suppliedElsewhere() guard.
testthat::skip_if_offline()
testthat::skip_if_not_installed("SpaDES.core")
if (
  !dir.exists(file.path(spadesTestPaths$sharedModulePath, "BiomeBGC_dataPrep"))
) {
  testthat::skip(
    "BiomeBGC_dataPrep module not found as a sibling of BiomeBGC_core"
  )
}

test_that("BiomeBGC_dataPrep + BiomeBGC_core run end-to-end for a point studyArea (single pixelGroup)", {
  # See BiomeBGC_dataPrep's test-5-integration-point.R for why we avoid
  # library(SpaDES.core): its .onAttach() calls setPaths() and can error in
  # some interactive sessions; requireNamespace()/:: calls sidestep that.
  requireNamespace("SpaDES.core", quietly = TRUE)

  mocks <- makeMockPointInputs()

  # BiomeBGC_core's createBGCdirs() copies met/epc/co2 input files from
  # inputPath (see helper-mockDataPrepInputs.R's writeMockDataPrepFixtureFiles()
  # for why these must be written to disk here rather than relying on
  # BiomeBGC_dataPrep to write them).
  bbgcInputPath <- tempfile("bbgc-integration-point-input-")
  writeMockDataPrepFixtureFiles(bbgcInputPath, mocks)

  parameters <- list(
    BiomeBGC_dataPrep = list(
      maxSpinupYears = 10L,
      metSpinupYears = mocks$metSpinupYears,
      # Disable the N-deposition ramp: it interpolates against CO2
      # concentration years beyond our toy CO2 fixture's short 8-year range,
      # which would otherwise crash the BGC C engine ("CO2 array index ...
      # in a ... sized array").
      NDepositionLevel = c(0, NA, NA),
      .plots = "none",
      .useCache = FALSE
    ),
    BiomeBGC_core = list(
      bbgcPath = file.path(spadesTestPaths$bbgcPath, "integration-point"),
      .useCache = FALSE
    )
  )

  sim <- SpaDES.core::simInit(
    times = mocks$times,
    params = parameters,
    modules = list("BiomeBGC_dataPrep", "BiomeBGC_core"),
    objects = mocks$objects,
    paths = list(
      modulePath = spadesTestPaths$sharedModulePath,
      inputPath = bbgcInputPath,
      outputPath = tempfile("bbgc-integration-point-output-"),
      cachePath = tempfile("bbgc-integration-point-cache-")
    )
  )

  out <- NULL
  # See BiomeBGC_dataPrep's test-5-integration-point.R for why this cosmetic
  # BioSimClient_R warning (from simInit()'s package-loading check) is
  # tolerated here, while any other, unexpected warning still fails the test.
  withCallingHandlers(
    {
      out <- SpaDES.core::spades(sim, debug = FALSE)
    },
    warning = function(w) {
      if (grepl("BioSimClient_R", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )

  expect_s4_class(out, "simList")

  ## BiomeBGC_dataPrep outputs, produced within the same run ----

  # exactly one point -> exactly one pixel group
  expect_equal(nrow(out$pixelGroupParameters), 1)
  expect_type(out$bbgcSpinup.ini, "list")
  expect_length(out$bbgcSpinup.ini, 1)
  expect_type(out$bbgc.ini, "list")
  expect_length(out$bbgc.ini, 1)

  ## BiomeBGC_core outputs, consuming the .ini files produced above ----

  expect_true(data.table::is.data.table(out$annualAverages))
  expect_true(data.table::is.data.table(out$monthlyAverages))
  expect_true(data.table::is.data.table(out$dailyOutput))
  expect_gt(nrow(out$annualAverages), 0)

  pixelGroupId <- out$pixelGroupParameters$pixelGroup[1]
  expect_equal(unique(out$annualAverages$pixelGroup), pixelGroupId)

  # Column names come from the default outputVariables parameter (see
  # BiomeBGC_core's own test-1/2-module-*-site.R tests, which use the same
  # default and the same column names).
  fluxCols <- c("daily_npp", "daily_nep")
  expect_true(all(fluxCols %in% names(out$annualAverages)))
  expect_false(anyNA(out$annualAverages[, .SD, .SDcols = fluxCols]))
})

test_that("BiomeBGC_dataPrep + BiomeBGC_core run end-to-end for a polygon studyArea (multiple pixelGroups)", {
  requireNamespace("SpaDES.core", quietly = TRUE)

  # makeMockPolygonInputs()'s default 4x4 raster varies soilDepth over the
  # first 2 cells vs. the rest, so BiomeBGC_dataPrep's pixel-grouping step
  # should produce more than one pixelGroup.
  mocks <- makeMockPolygonInputs()

  bbgcInputPath <- tempfile("bbgc-integration-polygon-input-")
  writeMockDataPrepFixtureFiles(bbgcInputPath, mocks)

  parameters <- list(
    BiomeBGC_dataPrep = list(
      maxSpinupYears = 10L,
      metSpinupYears = mocks$metSpinupYears,
      NDepositionLevel = c(0, NA, NA),
      .plots = "none",
      .useCache = FALSE
    ),
    BiomeBGC_core = list(
      bbgcPath = file.path(spadesTestPaths$bbgcPath, "integration-polygon"),
      .useCache = FALSE
    )
  )

  sim <- SpaDES.core::simInit(
    times = mocks$times,
    params = parameters,
    modules = list("BiomeBGC_dataPrep", "BiomeBGC_core"),
    objects = mocks$objects,
    paths = list(
      modulePath = spadesTestPaths$sharedModulePath,
      inputPath = bbgcInputPath,
      outputPath = tempfile("bbgc-integration-polygon-output-"),
      cachePath = tempfile("bbgc-integration-polygon-cache-")
    )
  )

  out <- NULL
  withCallingHandlers(
    {
      out <- SpaDES.core::spades(sim, debug = FALSE)
    },
    warning = function(w) {
      if (grepl("BioSimClient_R", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )

  expect_s4_class(out, "simList")

  ## BiomeBGC_dataPrep outputs, produced within the same run ----

  nPixelGroups <- nrow(out$pixelGroupParameters)
  expect_gt(nPixelGroups, 1)
  expect_type(out$bbgcSpinup.ini, "list")
  expect_length(out$bbgcSpinup.ini, nPixelGroups)
  expect_type(out$bbgc.ini, "list")
  expect_length(out$bbgc.ini, nPixelGroups)

  ## BiomeBGC_core outputs, consuming the .ini files produced above ----

  expect_true(data.table::is.data.table(out$annualAverages))
  expect_true(data.table::is.data.table(out$monthlyAverages))
  expect_true(data.table::is.data.table(out$dailyOutput))

  # every pixelGroup dataPrep produced should have a matching set of results
  # from BiomeBGC_core
  expect_setequal(
    unique(out$annualAverages$pixelGroup),
    out$pixelGroupParameters$pixelGroup
  )

  fluxCols <- c("daily_npp", "daily_nep")
  expect_true(all(fluxCols %in% names(out$annualAverages)))
  expect_false(anyNA(out$annualAverages[, .SD, .SDcols = fluxCols]))
})
