.onLoad <- function(libname, pkgname) {
  ## Registration of parallel backend
  if (isTRUE(getOption("ppstatParallel"))) {
    registerParBackend()
  }  else {
    registerParBackend(backend = "sequential")
  }
}

.onUnload <- function(libpath) {
  library.dynam.unload("ppstat", libpath)
}
