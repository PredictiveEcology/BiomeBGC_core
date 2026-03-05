
split_into_chunks <- function(x, n) {
  split(x, rep_len(seq_len(n), length(x)))
}


simulation_worker <- function(spinupIniPaths, iniPaths, argv, bbgcPath) {
  
  library(BiomeBGCR)
  
  bbgcPath <- normalizePath(bbgcPath)
  
  results <- vector("list", length(iniPaths))
  
  for (i in seq_along(iniPaths)) {
    
    spinupIniPath <- spinupIniPaths[i]
    iniPath <- iniPaths[i]
    
    resi <- bgcExecuteSpinup(argv = argv,
                             iniFiles = spinupIniPath,
                             path = bbgcPath)
    
    if (resi[[1]] != 0) {
      stop(sprintf("Spinup error for %s", iniPath))
    }
    
    resi <- bgcExecute(argv = argv,
                       iniFiles = iniPath,
                       path = bbgcPath)
    
    if (resi[[1]] != 0) {
      stop(sprintf("Spinup error for %s", iniPath))
    }
    
    results[[i]] <- resi[[2]][[1]]
    
  }
  
  results
}