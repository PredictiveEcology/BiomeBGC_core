if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("BiomeBGC_core: parallel and sequential runs agree (Boisvenue 2010, 6 sites)", {

  skip_if_not_installed("future.apply")
  skip_if_not_installed("future")

  ini <- buildMultiSiteIni()
  nSites <- length(boisvenueSiteSuffixes)

  refFile <- file.path(spadesTestPaths$outputPath, "multi-site-annualAverages-sequential.qs2")
  if (!file.exists(refFile)) {
    skip("Sequential reference run (test-2) has not produced its output file.")
  }
  sequentialAnnual <- qs2::qs_read(refFile)

  simInitInput <- list(
    modules = "BiomeBGC_core",
    paths = list(
      modulePath  = spadesTestPaths$modulePath,
      outputPath  = file.path(spadesTestPaths$outputPath, "multi-site-parallel"),
      cachePath   = spadesTestPaths$cachePath,
      inputPath   = spadesTestPaths$bbgcInputPath
    ),
    params = list(BiomeBGC_core = list(
      bbgcPath = file.path(spadesTestPaths$bbgcPath, "multi-site-parallel"),
      .useCache = FALSE,
      returnDailyEstimates = FALSE,
      returnMonthlyEstimates = FALSE,
      parallel.cores = 2L
    )),
    objects = list(
      bbgcSpinup.ini       = ini$bbgcSpinup.ini,
      bbgc.ini             = ini$bbgc.ini,
      pixelGroupParameters = data.table::data.table(pixelGroup = seq_len(nSites))
    )
  )

  simTestInit <- do.call(SpaDES.core::simInit, simInitInput)
  expect_s4_class(simTestInit, "simList")

  simTest <- SpaDES.core::spades(simTestInit)
  expect_s4_class(simTest, "simList")

  ## Check outputs ----

  parallelAnnual <- simTest$annualAverages
  data.table::setorder(parallelAnnual, pixelGroup, year)
  data.table::setorder(sequentialAnnual, pixelGroup, year)

  expect_equal(
    parallelAnnual[, .SD, .SDcols = names(sequentialAnnual)],
    sequentialAnnual
  )
})
