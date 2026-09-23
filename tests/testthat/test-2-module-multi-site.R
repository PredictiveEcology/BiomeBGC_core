if (!testthat::is_testing()) {
  source(testthat::test_path("setup.R"))
}

test_that("BiomeBGC_core: multi-site example run (Boisvenue 2010, 6 sites)", {
  ini <- buildMultiSiteIni()
  nSites <- length(boisvenueSiteSuffixes)

  simInitInput <- list(
    modules = "BiomeBGC_core",
    paths = list(
      modulePath = spadesTestPaths$modulePath,
      outputPath = file.path(spadesTestPaths$outputPath, "multi-site"),
      cachePath = spadesTestPaths$cachePath,
      inputPath = spadesTestPaths$bbgcInputPath
    ),
    params = list(
      BiomeBGC_core = list(
        bbgcPath = file.path(spadesTestPaths$bbgcPath, "multi-site"),
        .useCache = FALSE,
        returnDailyEstimates = FALSE,
        returnMonthlyEstimates = FALSE,
        parallel.cores = 1L
      )
    ),
    objects = list(
      bbgcSpinup.ini = ini$bbgcSpinup.ini,
      bbgc.ini = ini$bbgc.ini
    )
  )

  simTestInit <- do.call(SpaDES.core::simInit, simInitInput)
  expect_s4_class(simTestInit, "simList")

  simTest <- SpaDES.core::spades(simTestInit)
  expect_s4_class(simTest, "simList")

  ## Check outputs ----

  expect_true(data.table::is.data.table(simTest$annualAverages))
  expect_setequal(unique(simTest$annualAverages$pixelGroup), seq_len(nSites))

  # 140 simulation years (1950-2089) per site, per cccmat63_*.ini TIME_DEFINE
  expect_true(all(
    simTest$annualAverages[, .N, by = pixelGroup]$N == 140
  ))
  expect_equal(
    range(simTest$annualAverages$year),
    c(1950, 2089)
  )

  fluxCols <- c("summary.daily_npp", "summary.daily_nep", "summary.daily_gpp")
  expect_true(all(fluxCols %in% names(simTest$annualAverages)))
  expect_false(anyNA(simTest$annualAverages[, .SD, .SDcols = fluxCols]))

  # Save for comparison against the parallel run in test-3
  qs2::qs_save(
    simTest$annualAverages,
    file.path(
      spadesTestPaths$outputPath,
      "multi-site-annualAverages-sequential.qs2"
    )
  )
})
