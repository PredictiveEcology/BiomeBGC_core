if (!testthat::is_testing()) {
  source(testthat::test_path("setup.R"))
}

## Integration tests chaining BiomeBGC_dataPrep + BiomeBGC_core in a single
## simInit2()/spades() call: BiomeBGC_dataPrep prepares .ini files (and
## pixelGroupParameters/pixelGroupMap) from small, fully offline toy inputs,
## and BiomeBGC_core consumes those .ini files directly to run a simulation,
## in the same simList. 

testthat::skip_if_offline()
testthat::skip_if_not_installed("SpaDES.core")
testthat::skip_if_not_installed("SpaDES.project")

test_that("BiomeBGC_dataPrep + BiomeBGC_core run end-to-end for a point studyArea (single pixelGroup)", {
  requireNamespace("SpaDES.core", quietly = TRUE)

  testthat::skip_on_ci()

  # Set up project
  projectName <- "integration_dataPrep-core_point"
  mocks <- makeMockPointInputs()
  times <- mocks$times

  bbgcInputPath <- tempfile("bbgc-integration-point-input-")
  writeMockDataPrepFixtureFiles(bbgcInputPath, mocks)

  simInitInput <- SpaDES.project::setupProject(
    modules = c("PredictiveEcology/BiomeBGC_dataPrep@main", "BiomeBGC_core"),
    times   = times,
    paths   = list(
      projectPath = tempfile("bbgc-integration-point-project-"),
      modulePath  = spadesTestPaths$modulePath,
      inputPath   = bbgcInputPath,
      outputPath  = tempfile("bbgc-integration-point-output-"),
      cachePath   = tempfile("bbgc-integration-point-cache-")
    ),

    # Prepare input objects
    studyArea                 = mocks$objects$studyArea,
    rasterToMatch              = mocks$objects$rasterToMatch,
    dominantSpecies            = mocks$objects$dominantSpecies,
    climatePolygons            = mocks$objects$climatePolygons,
    sppEquiv                   = mocks$objects$sppEquiv,
    ecophysiologicalConstants  = mocks$objects$ecophysiologicalConstants,
    soilTexture                = mocks$objects$soilTexture,
    soilDepth                  = mocks$objects$soilDepth,
    elevation                  = mocks$objects$elevation,
    Ndeposition                = mocks$objects$Ndeposition,
    NfixationRates             = mocks$objects$NfixationRates,
    snowpackWaterContent       = mocks$objects$snowpackWaterContent,
    shortwaveAlbedo            = mocks$objects$shortwaveAlbedo,
    meteorologicalData         = mocks$objects$meteorologicalData,
    CO2concentration           = mocks$objects$CO2concentration,

    # Parameters
    params = list(
      BiomeBGC_dataPrep = list(
        maxSpinupYears = 10L,
        metSpinupYears = mocks$metSpinupYears,
        # Disable the N-deposition ramp: it interpolates against CO2
        # concentration years beyond our toy CO2 fixture's short 8-year
        # range, which would otherwise crash the BGC C engine ("CO2 array
        # index ... in a ... sized array").
        NDepositionLevel = c(0, NA, NA),
        .plots = "none",
        .useCache = FALSE
      ),
      BiomeBGC_core = list(
        bbgcPath = file.path(spadesTestPaths$bbgcPath, "integration-point"),
        .useCache = FALSE
      )
    ),
    updateRprofile = FALSE
  )

  # Run simInit
  simTestInit <- suppressWarnings(SpaDES.core::simInit2(simInitInput))
  expect_s4_class(simTestInit, "simList")

  # Run spades
  simTest <- suppressWarnings(SpaDES.core::spades(simTestInit, debug = FALSE))
  expect_s4_class(simTest, "simList")

  # Check outputs

  # exactly one point -> exactly one pixel group
  expect_equal(nrow(simTest$pixelGroupParameters), 1)

  expect_true(data.table::is.data.table(simTest$annualAverages))
  expect_gt(nrow(simTest$annualAverages), 0)

  pixelGroupId <- simTest$pixelGroupParameters$pixelGroup[1]
  expect_equal(unique(simTest$annualAverages$pixelGroup), pixelGroupId)
})

test_that("BiomeBGC_dataPrep + BiomeBGC_core run end-to-end for a polygon studyArea (multiple pixelGroups)", {
  requireNamespace("SpaDES.core", quietly = TRUE)

  testthat::skip_on_ci()

  # Set up project
  projectName <- "integration_dataPrep-core_polygon"
  # makeMockPolygonInputs()'s default 4x4 raster varies soilDepth over the
  # first 2 cells vs. the rest, so BiomeBGC_dataPrep's pixel-grouping step
  # should produce more than one pixelGroup.
  mocks <- makeMockPolygonInputs()
  times <- mocks$times

  bbgcInputPath <- tempfile("bbgc-integration-polygon-input-")
  writeMockDataPrepFixtureFiles(bbgcInputPath, mocks)

  simInitInput <- SpaDES.project::setupProject(
    modules = c("PredictiveEcology/BiomeBGC_dataPrep@main", "BiomeBGC_core"),
    times   = times,
    paths   = list(
      projectPath = tempfile("bbgc-integration-polygon-project-"),
      modulePath  = spadesTestPaths$modulePath,
      inputPath   = bbgcInputPath,
      outputPath  = tempfile("bbgc-integration-polygon-output-"),
      cachePath   = tempfile("bbgc-integration-polygon-cache-")
    ),

    # Prepare input objects
    studyArea                 = mocks$objects$studyArea,
    rasterToMatch              = mocks$objects$rasterToMatch,
    dominantSpecies            = mocks$objects$dominantSpecies,
    climatePolygons            = mocks$objects$climatePolygons,
    sppEquiv                   = mocks$objects$sppEquiv,
    ecophysiologicalConstants  = mocks$objects$ecophysiologicalConstants,
    soilTexture                = mocks$objects$soilTexture,
    soilDepth                  = mocks$objects$soilDepth,
    elevation                  = mocks$objects$elevation,
    Ndeposition                = mocks$objects$Ndeposition,
    NfixationRates             = mocks$objects$NfixationRates,
    snowpackWaterContent       = mocks$objects$snowpackWaterContent,
    shortwaveAlbedo            = mocks$objects$shortwaveAlbedo,
    meteorologicalData         = mocks$objects$meteorologicalData,
    CO2concentration           = mocks$objects$CO2concentration,

    # Parameters
    params = list(
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
    ),
    updateRprofile = FALSE
  )

  # Run simInit
  simTestInit <- suppressWarnings(SpaDES.core::simInit2(simInitInput))
  expect_s4_class(simTestInit, "simList")

  # Run spades
  simTest <-  suppressWarnings(SpaDES.core::spades(simTestInit, debug = FALSE))
  expect_s4_class(simTest, "simList")

  # Check outputs

  nPixelGroups <- nrow(simTest$pixelGroupParameters)
  expect_gt(nPixelGroups, 1)

  expect_true(data.table::is.data.table(simTest$annualAverages))

  # every pixelGroup dataPrep produced should have a matching set of results
  # from BiomeBGC_core
  expect_setequal(
    unique(simTest$annualAverages$pixelGroup),
    simTest$pixelGroupParameters$pixelGroup
  )
})
