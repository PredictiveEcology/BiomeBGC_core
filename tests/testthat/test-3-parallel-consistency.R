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
      bbgc.ini             = ini$bbgc.ini
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

  ## Subset/reorder columns via base indexing ("[["), not data.table's `[`
  ## NSE (.SD/.SDcols): in some evaluation contexts (e.g. the CI test runner)
  ## [.data.table dispatch does not occur as expected and falls through to
  ## [.data.frame, which does not understand .SD/.SDcols and silently returns
  ## a mismatched result rather than erroring (see the analogous comment in
  ## test-4-validation-reference.R).
  parallelAnnualSub <- data.table::as.data.table(
    lapply(names(sequentialAnnual), function(col) parallelAnnual[[col]])
  )
  data.table::setnames(parallelAnnualSub, names(sequentialAnnual))

  expect_equal(parallelAnnualSub, sequentialAnnual)
})
