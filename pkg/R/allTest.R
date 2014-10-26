allTest  <- function(dirs =  file.path(path.package(package = "processdata"), "unitTests")) {
  if(!require(RUnit))
    stop("Package 'RUnit' is not installed")

  processdataTestSuite <- defineTestSuite("processdata",
                                          dirs = dirs)

  RUnitOp <- getOption("RUnit")
  RUnitOp$verbose <- 0
  RUnitOp$silent <- TRUE
  RUnitOp <- options(RUnit = RUnitOp)
  testResults <- runTestSuite(processdataTestSuite)
  options(RUnit = RUnitOp$RUnit)
  
  return(testResults)
}


                                        
