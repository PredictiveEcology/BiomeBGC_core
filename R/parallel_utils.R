# function to split the target data (x) into chunks that will be sent to the workers
split_into_chunks <- function(x, n) {
  split(x, rep_len(seq_len(n), length(x)))
}

# Run the spinup + main simulation for a single pixelGroup and read back the
# requested output tables. Shared by both the sequential and parallel (future_lapply)
# code paths in Init() so there is exactly one implementation of this logic.
runPixelGroupSimulation <- function(pixelGroupName, spinupIniPath, iniPath, argv, bbgcPath,
                                     readDaily, readMonthly, readAnnual) {

  log <- capture.output({
    resi <- bgcExecuteSpinup(argv = argv, iniFiles = spinupIniPath, path = bbgcPath)
  })
  if (resi[[1]] != 0) {
    stop("Spinup error for pixelGroup ", pixelGroupName, ":\n", paste(log, collapse = "\n"))
  }

  log <- capture.output({
    resi <- bgcExecute(argv = argv, iniFiles = iniPath, path = bbgcPath)
  })
  if (resi[[1]] != 0) {
    stop("Simulation error for pixelGroup ", pixelGroupName, ":\n", paste(log, collapse = "\n"))
  }

  out <- list()
  if (readDaily) {
    out$daily <- readDailyOutput(resi[[2]][[1]])
    out$daily$pixelGroup <- pixelGroupName
    setcolorder(out$daily, "pixelGroup")
  }
  if (readMonthly) {
    out$monthly <- readMonthlyAverages(resi[[2]][[1]])
    out$monthly$pixelGroup <- pixelGroupName
    setcolorder(out$monthly, "pixelGroup")
  }
  if (readAnnual) {
    out$annual <- readAnnualAverages(resi[[2]][[1]])
    out$annual$pixelGroup <- pixelGroupName
    setcolorder(out$annual, "pixelGroup")
  }
  out
}

# Function used by the workers to run the spinup and the go simulations
simulation_worker <- function(spinupIniPaths, argv, bbgcPath, readDaily, readMonthly, readAnnual) {
  bbgcPath <- normalizePath(bbgcPath)
  iniPaths <- gsub("_spinup", "", spinupIniPaths)
  pixelGroups <- as.numeric(tools::file_path_sans_ext(basename(iniPaths)))

  lapply(seq_along(iniPaths), function(i) {
    runPixelGroupSimulation(
      pixelGroupName = pixelGroups[i],
      spinupIniPath = spinupIniPaths[i],
      iniPath = iniPaths[i],
      argv = argv,
      bbgcPath = bbgcPath,
      readDaily = readDaily,
      readMonthly = readMonthly,
      readAnnual = readAnnual
    )
  })
}


# Dispatch simulation_worker() calls to a multisession future cluster.
#
# simulation_worker/runPixelGroupSimulation/read*() are taken as explicit arguments
# (rather than resolved by name from this function's own environment) and passed to
# future_lapply() via a *named list* for future.globals, not a character vector. 
run_parallel_sims <- function(spinup_chunks, argv, bbgcPath, readDaily, readMonthly,
                               n_cores, libPaths, simulation_worker, runPixelGroupSimulation,
                               readDailyOutput, readMonthlyAverages, readAnnualAverages) {
  future::plan(future::multisession, workers = n_cores, rscript_libs = libPaths)
  on.exit(future::plan(future::sequential), add = TRUE)

  future.apply::future_lapply(
    X = spinup_chunks,
    FUN = simulation_worker,
    argv = argv,
    bbgcPath = bbgcPath,
    readDaily = readDaily,
    readMonthly = readMonthly,
    readAnnual = TRUE,
    future.packages = c("BiomeBGCR", "data.table"),
    future.globals = list(
      simulation_worker = simulation_worker,
      runPixelGroupSimulation = runPixelGroupSimulation,
      readDailyOutput = readDailyOutput,
      readMonthlyAverages = readMonthlyAverages,
      readAnnualAverages = readAnnualAverages
    )
  )
}
