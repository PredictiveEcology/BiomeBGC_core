# function to split the target data (x) into chunks that will be sent to the workers
split_into_chunks <- function(x, n) {
  split(x, rep_len(seq_len(n), length(x)))
}

# Function used by the workers to run the spinup and the go simulations
simulation_worker <- function(spinupIniPaths, argv, bbgcPath, readDaily, readMonthly, readAnnual) {
  # load needed library
  library(BiomeBGCR)
  
  # Prepare the inputs/outputs
  bbgcPath <- normalizePath(bbgcPath)
  
  results <- vector("list", length(spinupIniPaths))
  
  iniPaths <- gsub("_spinup", "", spinupIniPaths)
  
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
    
    if (readDaily)
      out$daily <- readDailyOutput(resi[[2]][[1]])
    
    if (readMonthly)
      out$monthly <- readMonthlyAverages(resi[[2]][[1]])
    
    if (readAnnual)
      out$annual <- readAnnualAverages(resi[[2]][[1]])
    
    results[[i]] <- out
    
  }
  
  return(results)
}

