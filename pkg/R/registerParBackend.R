registerParBackend <- function(backend = 'mc', cores = NULL) {
  if(!(class(cores) %in% c("NULL", "numeric")))
    stop("Argument 'cores' must be numeric or NULL")
  
  ## This checks if the paralle package is installed and if the GUI is
  ## appropriate for using this backend for parallel computations.
  if(backend == "mc") {
    if(.Platform$GUI %in% c("X11", "unknown")) {
      if(require("parallel")) {
        if(!is.null(cores))
          options(mc.cores = cores)
        
        assign("lapplyParallel", parallel::mclapply, .ppstatGlobals)
        assign("backend", "mc", .ppstatGlobals)
        message("Registering 'mclapply' from package multicore as parallel backend\n for ppstat.")
      } else {
        stop("The 'parallel' package is not available.")
      }
    } else {
      stop(paste("It is not recommended/possible to use 'mclapply' with the GUI:", .Platform$GUI))
    }
  } else if(backend == "sequential") {
    assign("lapplyParallel", lapply, .ppstatGlobals)
    assign("backend", "sequential", .ppstatGlobals)
  } else {
    stop(paste("The backend", backend, "is not supported."))
  }
  return(invisible())
}

getRegisteredParBackend <- function() {
  get("backend", .ppstatGlobals)
}
