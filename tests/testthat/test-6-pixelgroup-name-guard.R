if (!testthat::is_testing()) {
  source(testthat::test_path("setup.R"))
}

## Init() derives pixelGroup identity from names(sim$bbgc.ini)/names(sim$bbgcSpinup.ini)
## rather than sim$pixelGroupParameters (see issue #5). These tests exercise the
## guard that catches missing or mismatched names before they can silently
## mislabel simulation outputs.

test_that("Init() errors when bbgc.ini/bbgcSpinup.ini are unnamed", {
  ini <- buildIniInputs("enf_test1.ini", pixelGroupIds = 1)
  unnamedIni <- ini
  names(unnamedIni) <- NULL

  simInitInput <- list(
    modules = "BiomeBGC_core",
    paths = list(
      modulePath = spadesTestPaths$modulePath,
      outputPath = file.path(spadesTestPaths$outputPath, "unnamed-ini"),
      cachePath = spadesTestPaths$cachePath,
      inputPath = spadesTestPaths$bbgcInputPath
    ),
    params = list(
      BiomeBGC_core = list(
        bbgcPath = file.path(spadesTestPaths$bbgcPath, "unnamed-ini"),
        .useCache = FALSE
      )
    ),
    objects = list(
      bbgcSpinup.ini = unnamedIni,
      bbgc.ini = unnamedIni
    )
  )

  simTestInit <- do.call(SpaDES.core::simInit, simInitInput)
  expect_error(
    SpaDES.core::spades(simTestInit),
    "must be named lists"
  )
})

test_that("Init() errors when bbgc.ini/bbgcSpinup.ini names disagree", {
  ini <- buildIniInputs("enf_test1.ini", pixelGroupIds = 1)
  mismatchedIni <- ini
  names(mismatchedIni) <- "2"

  simInitInput <- list(
    modules = "BiomeBGC_core",
    paths = list(
      modulePath = spadesTestPaths$modulePath,
      outputPath = file.path(spadesTestPaths$outputPath, "mismatched-ini"),
      cachePath = spadesTestPaths$cachePath,
      inputPath = spadesTestPaths$bbgcInputPath
    ),
    params = list(
      BiomeBGC_core = list(
        bbgcPath = file.path(spadesTestPaths$bbgcPath, "mismatched-ini"),
        .useCache = FALSE
      )
    ),
    objects = list(
      bbgcSpinup.ini = ini,
      bbgc.ini = mismatchedIni
    )
  )

  simTestInit <- do.call(SpaDES.core::simInit, simInitInput)
  expect_error(
    SpaDES.core::spades(simTestInit),
    "must match exactly"
  )
})
