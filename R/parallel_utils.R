# function to split the target data (x) into chunks that will be sent to the workers
split_into_chunks <- function(x, n) {
  split(x, rep_len(seq_len(n), length(x)))
}

# Function used by the workers to run the spinup and the go simulations
simulation_worker <- function(spinupIniPaths, argv, bbgcPath, readDaily, readMonthly, readAnnual) {

  # Prepare the inputs/outputs
  bbgcPath <- normalizePath(bbgcPath)
  
  results <- vector("list", length(spinupIniPaths))
  
  iniPaths <- gsub("_spinup", "", spinupIniPaths)
  
  pixelGroups <- as.numeric(tools::file_path_sans_ext(basename(iniPaths)))
  
  # For each path, run the spinup and simulation
  for (i in seq_along(iniPaths)) {
    
    spinupIniPath <- spinupIniPaths[i]
    iniPath <- iniPaths[i]
    
    # run the spinup. Silence the simulation
    log <- capture.output({
      resi <- bgcExecuteSpinup(argv = argv,
                               iniFiles = spinupIniPath,
                               path = bbgcPath)
    })
    
    # if resi[[1]] == 0, there was an error during the spinup
    if (resi[[1]] != 0) {
      stop(sprintf("Spinup error for %s", spinupIniPath))
    }
    
    # run the go simulation
    resi <- bgcExecute(argv = argv,
                       iniFiles = iniPath,
                       path = bbgcPath)
    
    if (resi[[1]] != 0) {
      stop(sprintf("Simulation error for %s", iniPath))
    }
    
    out <- list()
    
    if (readDaily){
      out$daily <- readDailyOutput(resi[[2]][[1]])
      out$daily$pixelGroup <- pixelGroups[i]
      setcolorder(out$daily, "pixelGroup")
    }
    
    
    if (readMonthly){
      out$monthly <- readMonthlyAverages(resi[[2]][[1]])
      out$monthly$pixelGroup <- pixelGroups[i]
      setcolorder(out$monthly, "pixelGroup")
    }
    
    if (readAnnual){
      out$annual <- readAnnualAverages(resi[[2]][[1]])
      out$annual$pixelGroup <- pixelGroups[i]
      setcolorder(out$annual, "pixelGroup")
    }
    
    results[[i]] <- out
    
  }
  
  return(results)
}


# Dispatch simulation_worker() calls to a multisession future cluster.
#
# This is a standalone wrapper (rather than inlining plan()/future_lapply()
# directly in Init()) so that its own environment() can be rebound to
# globalenv() before it is called. future_lapply()'s automatic global/package
# detection resolves names starting from the *calling* frame, not just from
# FUN's environment - and when this code runs as part of a SpaDES module's
# test suite, Init() (and therefore its calling frame) is defined inside a
# throwaway package namespace that SpaDES.core::convertToPackage() generates
# on the fly (e.g. "BiomeBGC.core"), which is never actually installed to a
# library. Left as-is, future tries to have each worker library() that
# nonexistent package and fails with "there is no package called
# 'BiomeBGC.core'". Rebinding this function's environment to globalenv()
# keeps the calling frame's lexical parent out of that namespace.
run_parallel_sims <- function(spinup_chunks, argv, bbgcPath, readDaily, readMonthly,
                               n_cores, libPaths) {
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
    future.globals = c(
      "simulation_worker",
      "argv",
      "bbgcPath",
      "readDaily",
      "readMonthly",
      "readDailyOutput",
      "readMonthlyAverages",
      "readAnnualAverages"
    )
  )
}
