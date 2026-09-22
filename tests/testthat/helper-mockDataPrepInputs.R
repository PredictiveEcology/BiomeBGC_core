# Shared builders for small, fully offline (no-network) mock objects
# satisfying every expectsInput() of BiomeBGC_dataPrep, for use by this
# suite's dataPrep+core integration tests (test-5-integration-dataPrep-core.R).
# Auto-sourced by testthat via the helper-*.R naming convention.
#
# Adapted/duplicated from BiomeBGC_dataPrep's own
# tests/testthat/helper-mockInputs.R, so that BiomeBGC_core's test suite
# remains self-contained (does not reach into BiomeBGC_dataPrep's test tree,
# and does not require both module repos to be checked out as siblings).
#
# One difference from the original: the EPC fixture is read directly from
# BiomeBGCR's shipped inst/inputs/epc/enf.epc (mirroring how the rest of
# BiomeBGC_core's tests source fixtures from the installed BiomeBGCR package),
# rather than from a local testdata/epc fixture file (BiomeBGC_core has no
# tests/testthat/testdata folder of its own).

# A minimal, physically-plausible ecophysiological constants row for a
# single mock species, built from BiomeBGCR's shipped enf.epc fixture so
# values are realistic rather than arbitrary.
.mockEcophysiologicalConstants <- function(speciesId = "Pice_gla") {
  data.frame(
    speciesId = speciesId,
    species = "Picea glauca",
    genus = "Picea",
    PFT = "enf",
    stringsAsFactors = FALSE
  )
}

# Build a short, structurally valid meteorological time series (daily rows,
# 365 days/year, no Feb 29) for one climate polygon, spanning nSpinupYears
# before firstSimYear through lastSimYear.
.mockMetDataForPolygon <- function(firstSimYear, lastSimYear, nSpinupYears) {
  years <- (firstSimYear - nSpinupYears):lastSimYear
  do.call(
    rbind,
    lapply(years, function(yr) {
      data.frame(
        year = yr,
        yday = 1:365,
        tmax = 10 + 5 * sin(2 * pi * (1:365) / 365),
        tmin = 2 + 5 * sin(2 * pi * (1:365) / 365),
        tday = 6 + 5 * sin(2 * pi * (1:365) / 365),
        prcp = 0.2,
        vpd = 300,
        srad = 150,
        daylen = 30000,
        spinup = yr < firstSimYear
      )
    })
  )
}

# Build a small CO2 concentration series covering firstYear:lastYear.
.mockCO2Concentration <- function(firstYear, lastYear) {
  data.frame(
    year = firstYear:lastYear,
    co2_ppm = 400
  )
}

# Write a minimal, valid .mtc43 meteorological data file (mirrors
# BiomeBGC_dataPrep's internal metWrite(), reimplemented here since that
# function isn't exported/available before BiomeBGC_dataPrep's own R/ files
# are sourced by simInit()).
.writeMockMtc43 <- function(metData, filePath, siteName = "mock site") {
  con <- file(filePath, open = "w")
  on.exit(close(con))
  writeLines(
    paste0(siteName, ",", paste(range(metData$year), collapse = "-")),
    con
  )
  writeLines("mock data OUTPUT FILE", con)
  writeLines(
    "  year  yday    Tmax    Tmin    Tday    prcp      VPD     srad  daylen",
    con
  )
  writeLines(
    "             (deg C) (deg C) (deg C)    (cm)     (Pa)  (W m-2)     (s)",
    con
  )
  for (i in seq_len(nrow(metData))) {
    writeLines(
      paste0(
        formatC(metData$year[i], width = 6),
        formatC(metData$yday[i], width = 6),
        formatC(metData$tmax[i], digits = 2, format = "f", width = 8),
        formatC(metData$tmin[i], digits = 2, format = "f", width = 8),
        formatC(metData$tday[i], digits = 2, format = "f", width = 8),
        formatC(metData$prcp[i], digits = 2, format = "f", width = 8),
        formatC(metData$vpd[i], digits = 2, format = "f", width = 9),
        formatC(metData$srad[i], digits = 2, format = "f", width = 9),
        formatC(metData$daylen[i], format = "d", width = 8)
      ),
      con
    )
  }
}

# Write a minimal, valid CO2 concentration file (mirrors BiomeBGC_dataPrep's
# internal CO2write(): two whitespace-separated columns, no header/row names).
.writeMockCO2File <- function(co2Data, filePath) {
  write.table(
    data.frame(year = co2Data$year, co2_ppm = co2Data$co2_ppm),
    file = filePath,
    sep = "\t",
    row.names = FALSE,
    col.names = FALSE
  )
}

#' Write the physical met/epc/co2 fixture files that BiomeBGC_core's
#' createBGCdirs() expects to find (and copy) under `inputPath`.
#'
#' BiomeBGC_dataPrep only *writes* these files to disk as a side effect of
#' its own network-based prep functions (prepClimate(), prepEPC(),
#' prepCo2Concentration()), which are skipped entirely here because the
#' corresponding sim objects (meteorologicalData, ecophysiologicalConstants,
#' CO2concentration) are supplied directly as offline mocks. This helper
#' recreates just enough of that on-disk state - matching the exact
#' filename conventions BiomeBGC_dataPrep's prepareSpinupIni()/prepareIni()
#' embed in the .ini files it produces - for BiomeBGC_core to find and copy.
writeMockDataPrepFixtureFiles <- function(
  inputPath,
  mocks,
  climModel = "RCM4",
  co2scenario = "RCP45"
) {
  dir.create(
    file.path(inputPath, "metdata"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create(
    file.path(inputPath, "epc"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create(
    file.path(inputPath, "co2"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  simStart <- mocks$times$start
  simEnd <- mocks$times$end
  metSpinupYears <- mocks$metSpinupYears
  firstYear <- simStart - metSpinupYears

  # One .mtc43 pair (spinup + main) per climate polygon in meteorologicalData.
  for (climatePolygonId in names(mocks$objects$meteorologicalData)) {
    metData <- mocks$objects$meteorologicalData[[climatePolygonId]]

    spinupFileName <- tolower(paste0(climatePolygonId, "_spinup.mtc43"))
    .writeMockMtc43(
      metData[metData$spinup, ],
      file.path(inputPath, "metdata", spinupFileName)
    )

    # Main-run met file covers the *full* spinup+simulation period (mirrors
    # BiomeBGC_dataPrep's prepClimateSinglePolygon(), which writes the whole
    # `climate` data.frame here, not just the non-spinup rows).
    mainFileName <- tolower(paste0(
      climatePolygonId,
      "_",
      climModel,
      co2scenario,
      "_",
      firstYear,
      simEnd,
      ".mtc43"
    ))
    .writeMockMtc43(
      metData,
      file.path(inputPath, "metdata", mainFileName)
    )
  }

  # One .epc file per species in sppEquiv, using BiomeBGCR's shipped enf.epc
  # as a stand-in template regardless of PFT (sufficient for a structural
  # integration test; not intended to be biologically meaningful per-species).
  # Filename must match prepSpinupIni_worker()'s convention: it looks up the
  # *species name* (not speciesId) via species_lookup, then strips spaces -
  # e.g. "Picea glauca" -> "piceaglauca.epc".
  epcTemplate <- system.file("inputs", "epc", "enf.epc", package = "BiomeBGCR")
  for (speciesName in mocks$objects$sppEquiv$species) {
    epcFileName <- tolower(paste0(gsub(" ", "", speciesName), ".epc"))
    file.copy(
      epcTemplate,
      file.path(inputPath, "epc", epcFileName),
      overwrite = TRUE
    )
  }

  # Single CO2 file covering the full spinup+simulation period.
  co2FileName <- paste0(
    "co2_",
    firstYear,
    "_",
    simEnd,
    "_",
    co2scenario,
    ".txt"
  )
  .writeMockCO2File(
    mocks$objects$CO2concentration,
    file.path(inputPath, "co2", co2FileName)
  )

  invisible(NULL)
}

#' Build mock inputs for a polygon studyArea.
#'
#' Creates a small (4x4 cell) rasterToMatch, with the first two cells set to
#' a different soilDepth than the rest, so LandR::generatePixelGroups()
#' produces more than one pixelGroup.
makeMockPolygonInputs <- function(
  nrow = 4,
  ncol = 4,
  simStart = 2000,
  simEnd = 2002,
  metSpinupYears = 5
) {
  rasterToMatch <- terra::rast(
    nrows = nrow,
    ncols = ncol,
    xmin = -1000,
    xmax = -1000 + ncol * 250,
    ymin = 5900000,
    ymax = 5900000 + nrow * 250,
    crs = "EPSG:3978"
  )
  terra::values(rasterToMatch) <- 1

  studyArea <- terra::as.polygons(rasterToMatch, extent = TRUE)
  studyArea$studyAreaId <- 1

  dominantSpecies <- terra::rast(rasterToMatch)
  terra::values(dominantSpecies) <- 1L
  levels(dominantSpecies) <- data.frame(id = 1L, category = "Pice_gla")

  climatePolygons <- studyArea
  climatePolygons$climatePolygonId <- 1

  # vary soilDepth over the first 2 cells vs the rest, to force >1 pixelGroup
  soilDepth <- terra::rast(rasterToMatch)
  ncells <- terra::ncell(rasterToMatch)
  terra::values(soilDepth) <- c(rep(0.6, 2), rep(1.2, ncells - 2))

  # NB: the module's own code reads this input as sim$soilTexture (singular),
  # even though its expectsInput() declares it as "soilTextures" (plural).
  soilTexture <- c(
    sand = {
      r <- terra::rast(rasterToMatch)
      terra::values(r) <- 30
      r
    },
    silt = {
      r <- terra::rast(rasterToMatch)
      terra::values(r) <- 50
      r
    },
    clay = {
      r <- terra::rast(rasterToMatch)
      terra::values(r) <- 20
      r
    }
  )
  names(soilTexture) <- c("sand", "silt", "clay")

  shortwaveAlbedo <- terra::rast(rasterToMatch)
  terra::values(shortwaveAlbedo) <- 0.15
  elevation <- terra::rast(rasterToMatch)
  terra::values(elevation) <- 500
  NfixationRates <- terra::rast(rasterToMatch)
  terra::values(NfixationRates) <- 0.0001
  snowpackWaterContent <- terra::rast(rasterToMatch)
  terra::values(snowpackWaterContent) <- 10

  NdepositionT1 <- terra::rast(rasterToMatch)
  terra::values(NdepositionT1) <- 0.002
  NdepositionT2 <- terra::rast(rasterToMatch)
  terra::values(NdepositionT2) <- 0.0025
  Ndeposition <- c(NdepositionT1, NdepositionT2)
  names(Ndeposition) <- c("2015", "2020")

  sppEquiv <- .mockEcophysiologicalConstants()
  ecophysiologicalConstants <- data.frame(speciesId = "Pice_gla")

  meteorologicalData <- list(
    `1` = .mockMetDataForPolygon(simStart, simEnd, metSpinupYears)
  )
  CO2concentration <- .mockCO2Concentration(simStart - metSpinupYears, simEnd)

  list(
    objects = list(
      studyArea = studyArea,
      rasterToMatch = rasterToMatch,
      dominantSpecies = dominantSpecies,
      climatePolygons = climatePolygons,
      sppEquiv = sppEquiv,
      ecophysiologicalConstants = ecophysiologicalConstants,
      soilTexture = soilTexture,
      soilDepth = soilDepth,
      elevation = elevation,
      Ndeposition = Ndeposition,
      NfixationRates = NfixationRates,
      snowpackWaterContent = snowpackWaterContent,
      shortwaveAlbedo = shortwaveAlbedo,
      meteorologicalData = meteorologicalData,
      CO2concentration = CO2concentration
    ),
    times = list(start = simStart, end = simEnd),
    metSpinupYears = metSpinupYears
  )
}

#' Build mock inputs for a point studyArea.
#'
#' Mirrors makeMockPolygonInputs(), but studyArea is a single point and
#' rasterToMatch covers a small area around it (as required by the fixed
#' `res(sim$rasterToMatch)` line in the point branch of .inputObjects()).
makeMockPointInputs <- function(
  simStart = 2000,
  simEnd = 2002,
  metSpinupYears = 5
) {
  polygonMocks <- makeMockPolygonInputs(
    nrow = 2,
    ncol = 2,
    simStart = simStart,
    simEnd = simEnd,
    metSpinupYears = metSpinupYears
  )

  rasterToMatch <- polygonMocks$objects$rasterToMatch
  pointCoords <- terra::xyFromCell(
    rasterToMatch,
    terra::ncell(rasterToMatch) %/% 2 + 1
  )
  studyArea <- terra::vect(
    pointCoords,
    type = "points",
    crs = terra::crs(rasterToMatch)
  )
  studyArea$studyAreaId <- 1

  objects <- polygonMocks$objects
  objects$studyArea <- studyArea

  list(
    objects = objects,
    times = polygonMocks$times,
    metSpinupYears = polygonMocks$metSpinupYears
  )
}
