### Based on http://rwiki.sciviews.org/doku.php?id=developers:runit

### Unit tests will not be done if RUnit is not available.
if(require("RUnit", quietly = TRUE)) {
 
  ## --- Setup ---
 
  pkg <- "ppstat"
  ## Path to unit tests for installed package 
  ## PKG.Rcheck/tests/../PKG/unitTests
  path <- system.file(package = pkg, "unitTests")
  if(Sys.getenv("RCMDCHECK") == "FALSE") {
    ## Path to report for standalone running using make
    path <- file.path(getwd(), "unitTests")
  }
  
  cat("\nRunning unit tests\n")
  print(list(pkg = pkg, pathToReport = path))
 
  library(package=pkg, character.only = TRUE)
 
  ## If desired, load the name space to allow testing of private functions
  ## if (is.element(pkg, loadedNamespaces()))
  ##     attach(loadNamespace(pkg), name=paste("namespace", pkg, sep=":"), pos=3)
  ##
  ## or simply call PKG:::myPrivateFunction() in tests
 
  ## --- Testing ---
 
  ## Define tests
  testSuite <- defineTestSuite(name = paste(pkg, "unit testing"),
                                           dirs = path)
  ## Run
  RUnitOp <- getOption("RUnit")
  RUnitOp$verbose <- 0
  RUnitOp$silent <- TRUE
  RUnitOp <- options(RUnit = RUnitOp)
  tests <- runTestSuite(testSuite)
  options(RUnit = RUnitOp$RUnit)
 
  ## Default report name
  pathReport <- file.path(path, "report")
 
  ## Report to stdout and text files
  cat("------------------- UNIT TEST SUMMARY ---------------------\n\n")
  printTextProtocol(tests, showDetails=FALSE)
  printTextProtocol(tests, showDetails=FALSE,
                    fileName=paste(pathReport, "Summary.txt", sep=""))
  printTextProtocol(tests, showDetails=TRUE,
                    fileName=paste(pathReport, ".txt", sep=""))
 
  ## Report to HTML file
  printHTMLProtocol(tests, fileName=paste(pathReport, ".html", sep=""))
 
  ## Return stop() to cause R CMD check stop in case of
  ##  - failures i.e. FALSE to unit tests or
  ##  - errors i.e. R errors
  tmp <- getErrors(tests)
  if(tmp$nFail > 0 | tmp$nErr > 0) {
    stop(paste("\n\nunit testing failed (#test failures: ", tmp$nFail,
               ", #R errors: ",  tmp$nErr, ")\n\n", sep=""))
  }
} else {
  warning("Cannot run unit tests -- package RUnit is not available")
}
