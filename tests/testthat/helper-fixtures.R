## Shared fixture-building helpers for BiomeBGC_core tests.
## Automatically sourced by testthat (helper-*.R convention) before each
## test file runs, so it does not need to be re-sourced manually.

## Build a named list of parsed ini objects (BiomeBGCR::iniRead() output),
## keyed by pixelGroup id, from BiomeBGCR's example .ini fixtures.
buildIniInputs <- function(iniFileNames, pixelGroupIds = seq_along(iniFileNames)) {
  ini <- lapply(
    file.path(system.file("inputs", package = "BiomeBGCR"), "ini", iniFileNames),
    BiomeBGCR::iniRead
  )
  names(ini) <- as.character(pixelGroupIds)
  ini
}

## Six-site Boisvenue (2010) example fixture set shipped in BiomeBGCR
## (Deer Point, Boise, Glacier, Missoula, Priest River, Yellowstone).
boisvenueSiteSuffixes <- c("b", "bc", "g", "m", "pr", "y")

buildMultiSiteIni <- function() {
  list(
    bbgcSpinup.ini = buildIniInputs(paste0("spinup_", boisvenueSiteSuffixes, ".ini")),
    bbgc.ini        = buildIniInputs(paste0("cccmat63_", boisvenueSiteSuffixes, ".ini"))
  )
}
