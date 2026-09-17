if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

## Numeric regression test: compares BiomeBGC_core's annual outputs against
## BiomeBGCR's own reference annual summaries (inst/outputs/reference/*_ann.txt),
## which were validated against the original Biome-BGC C engine
## (see BiomeBGCR's test-validation-boisvenue2010*.R / test-validation-missoula*.R tests).
##
## The reference files store 8 columns: year, prcp, tavg, LAI, ET, OF, NPP, NBP.
## BiomeBGC_core's annualAverages data.table stores the underlying daily-output
## variables instead (see DAILY_OUTPUT section of the .ini files), so the
## comparable quantities are re-derived from those columns where available:
##   - NPP (gC/m2/yr) = summary.daily_npp (kgC/m2/day) * 1000 * 365
##   - NBP (gC/m2/yr) = summary.daily_nee (kgC/m2/day) * 1000 * 365
##   - ET  (mm/yr)    = summary.evapotranspiration (kg/m2/day, i.e. mm/day) * 365
##   - LAI (m2/m2)    = epv.ytd_maxplai (annual maximum projected LAI)
## prcp, tavg and OF are not among BiomeBGC_core's returned daily-output
## variables and are therefore never compared. ET and LAI additionally
## require that the .ini's DAILY_OUTPUT section selected
## summary.evapotranspiration / epv.ytd_maxplai; the Missoula (enf_test1.ini)
## fixture does not select these (it uses epv.proj_lai, an instantaneous
## rather than year-to-date-max LAI, which is not a valid substitute), so
## only NPP/NBP are checked for that fixture.
##
## Reference values are rounded to 1 decimal place, so comparisons use a
## small numeric tolerance rather than exact equality.

readReferenceAnnual <- function(referenceFileName) {
  refFile <- system.file(
    file.path("outputs", "reference", referenceFileName),
    package = "BiomeBGCR"
  )
  data.table::as.data.table(
    read.table(
      refFile, header = FALSE, skip = 10,
      col.names = c("year", "prcp", "tavg", "LAI", "ET", "OF", "NPP", "NBP")
    )
  )
}

## Compares a single pixelGroup's simulated annual outputs against a
## BiomeBGCR reference annual-summary file, using expect_equal() with a
## small numeric tolerance (reference values are rounded to 1 decimal place).
## `vars` controls which reference variables are checked (a subset of
## c("NPP", "NBP", "ET", "LAI")), since not every example .ini selects the
## daily-output variables needed to derive all four.
expectAnnualMatchesReference <- function(annualAverages, pixelGroupId, referenceFileName, label,
                                          vars = c("NPP", "NBP", "ET", "LAI")) {
  reference <- readReferenceAnnual(referenceFileName)

  simSite <- annualAverages[pixelGroup == pixelGroupId, .(
    year,
    NPP = summary.daily_npp * 1000 * 365,
    NBP = summary.daily_nee * 1000 * 365,
    ET  = if ("summary.evapotranspiration" %in% names(annualAverages)) summary.evapotranspiration * 365 else NA_real_,
    LAI = if ("epv.ytd_maxplai" %in% names(annualAverages)) epv.ytd_maxplai else NA_real_
  )]

  cmp <- merge(simSite, reference[, .(year, NPP, NBP, ET, LAI)],
               by = "year", suffixes = c(".sim", ".ref"))

  expect_equal(nrow(cmp), nrow(reference),
               info = paste(label, "- year mismatch with reference"))

  tolerances <- c(NPP = 0.1, NBP = 0.1, ET = 0.1, LAI = 0.2)

  for (v in vars) {
    expect_equal(cmp[[paste0(v, ".sim")]], cmp[[paste0(v, ".ref")]], tolerance = tolerances[[v]],
                 info = paste(label, "-", v, "mismatch"))
  }
}

test_that("BiomeBGC_core: annual outputs match BiomeBGCR reference (Boisvenue 2010, 6 sites)", {

  ini <- buildMultiSiteIni()
  nSites <- length(boisvenueSiteSuffixes)

  simInitInput <- list(
    modules = "BiomeBGC_core",
    paths = list(
      modulePath  = spadesTestPaths$modulePath,
      outputPath  = file.path(spadesTestPaths$outputPath, "validation-reference"),
      cachePath   = spadesTestPaths$cachePath,
      inputPath   = spadesTestPaths$bbgcInputPath
    ),
    params = list(BiomeBGC_core = list(
      bbgcPath = file.path(spadesTestPaths$bbgcPath, "validation-reference"),
      .useCache = FALSE,
      returnDailyEstimates = FALSE,
      returnMonthlyEstimates = FALSE,
      parallel.cores = 1L
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

  ## Check outputs against BiomeBGCR reference ----

  for (i in seq_along(boisvenueSiteSuffixes)) {
    siteSuffix <- boisvenueSiteSuffixes[i]

    expectAnnualMatchesReference(
      annualAverages    = simTest$annualAverages,
      pixelGroupId      = i,
      referenceFileName = paste0("cccmat63_", siteSuffix, "_ann.txt"),
      label             = paste("site", siteSuffix)
    )
  }
})

test_that("BiomeBGC_core: annual outputs match BiomeBGCR reference (Missoula, enf_test1)", {

  # Missoula single-site fixture: same ini used for spinup and main run
  # (its RESTART flags mark it as a self-contained example, not a spinup file).
  ini <- buildIniInputs("enf_test1.ini", pixelGroupIds = 1)

  simInitInput <- list(
    modules = "BiomeBGC_core",
    paths = list(
      modulePath  = spadesTestPaths$modulePath,
      outputPath  = file.path(spadesTestPaths$outputPath, "validation-reference-missoula"),
      cachePath   = spadesTestPaths$cachePath,
      inputPath   = spadesTestPaths$bbgcInputPath
    ),
    params = list(BiomeBGC_core = list(
      bbgcPath = file.path(spadesTestPaths$bbgcPath, "validation-reference-missoula"),
      .useCache = FALSE,
      returnDailyEstimates = FALSE,
      returnMonthlyEstimates = FALSE
    )),
    objects = list(
      bbgcSpinup.ini       = ini,
      bbgc.ini             = ini,
      pixelGroupParameters = data.table::data.table(pixelGroup = 1)
    )
  )

  simTestInit <- do.call(SpaDES.core::simInit, simInitInput)
  expect_s4_class(simTestInit, "simList")

  simTest <- SpaDES.core::spades(simTestInit)
  expect_s4_class(simTest, "simList")

  ## Check outputs against BiomeBGCR reference ----

  # enf_test1.ini's DAILY_OUTPUT selection does not include
  # summary.evapotranspiration or epv.ytd_maxplai, so only NPP/NBP are
  # checked here (see comment at top of file).
  expectAnnualMatchesReference(
    annualAverages    = simTest$annualAverages,
    pixelGroupId      = 1,
    referenceFileName = "oth_ann.txt",
    label             = "Missoula",
    vars              = c("NPP", "NBP")
  )
})
