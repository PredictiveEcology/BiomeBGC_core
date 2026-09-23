if (!testthat::is_testing()) {
  source(testthat::test_path("setup.R"))
}

test_that("BiomeBGC_core: single-site example run (Missoula, enf_test1)", {
  # Missoula single-site fixture: same ini used for spinup and main run
  # (its RESTART flags mark it as a self-contained example, not a spinup file).
  ini <- buildIniInputs("enf_test1.ini", pixelGroupIds = 1)

  simInitInput <- list(
    modules = "BiomeBGC_core",
    paths = list(
      modulePath = spadesTestPaths$modulePath,
      outputPath = file.path(spadesTestPaths$outputPath, "single-site"),
      cachePath = spadesTestPaths$cachePath,
      inputPath = spadesTestPaths$bbgcInputPath
    ),
    params = list(
      BiomeBGC_core = list(
        bbgcPath = file.path(spadesTestPaths$bbgcPath, "single-site"),
        .useCache = FALSE
      )
    ),
    objects = list(
      bbgcSpinup.ini = ini,
      bbgc.ini = ini
    )
  )

  simTestInit <- do.call(SpaDES.core::simInit, simInitInput)
  expect_s4_class(simTestInit, "simList")

  simTest <- SpaDES.core::spades(simTestInit)
  expect_s4_class(simTest, "simList")

  ## Check outputs ----

  expect_true(data.table::is.data.table(simTest$annualAverages))
  expect_true(data.table::is.data.table(simTest$monthlyAverages))
  expect_true(data.table::is.data.table(simTest$dailyOutput))

  # 44 simulation years in enf_test1.ini (TIME_DEFINE: 1950-1993)
  expect_equal(nrow(simTest$annualAverages), 44)
  expect_equal(sort(unique(simTest$annualAverages$year)), 1950:1993)
  expect_equal(unique(simTest$annualAverages$pixelGroup), 1)

  expect_equal(nrow(simTest$monthlyAverages), 44 * 12)
  expect_equal(nrow(simTest$dailyOutput), 44 * 365)

  # Key carbon/water flux columns should be present and non-NA
  fluxCols <- c("summary.daily_npp", "summary.daily_nep", "summary.daily_gpp")
  expect_true(all(fluxCols %in% names(simTest$annualAverages)))
  expect_false(anyNA(simTest$annualAverages[, .SD, .SDcols = fluxCols]))
})
