## Tests of constructors, data extractors getId, getTime, getNumerics,
## getColumns, getFactors, and attribute extractors dim and colNames.

test.continuousProcessDataFrame <- function() {
  ## Automatic generation of id and time
  continuousData <- data.frame(value = c(0.1,0.3,0.2,0.4))
  CP <- continuousProcess(continuousData)
  checkEquals(getId(CP), factor(c(1,1,1,1)))
  checkEquals(getTime(CP), c(0,1,2,3))
  checkEquals(getNumerics(CP)[,"value"], c(0.1,0.3,0.2,0.4))
  checkEquals(getColumns(CP, "value"), c(0.1,0.3,0.2,0.4))
  checkEquals(colNames(CP), c("value"))

  ## Automatic generation of id
  continuousData <- data.frame(time = c(2,3,4,5),
                               value = c(0.1,0.3,0.2,0.4))
  CP <- continuousProcess(continuousData)
  checkEquals(getId(CP), factor(c(1,1,1,1)))
  checkEquals(getTime(CP), c(2,3,4,5))
  checkEquals(getNumerics(CP)[,"value"], c(0.1,0.3,0.2,0.4))
  checkEquals(getColumns(CP, "value"), c(0.1,0.3,0.2,0.4))
  checkEquals(colNames(CP), c("value"))

  ## Check that everything comes back as expected
  continuousData <- data.frame(id = c(1,1,2,2),
                               time = c(1,2,1,2),
                               value = c(0.1,0.3,0.2,0.4),
                               group = factor(c(1,2,1,2))
                               )
  CP <- continuousProcess(continuousData)
  checkEquals(getId(CP), factor(c(1,1,2,2)))
  checkEquals(getTime(CP), c(1,2,1,2))
  checkEquals(getNumerics(CP)[,"value"], c(0.1,0.3,0.2,0.4))
  checkEquals(getColumns(CP, "value"), c(0.1,0.3,0.2,0.4))
  checkEquals(getFactors(CP)[["group"]], factor(c(1,2,1,2)))
  checkEquals(getColumns(CP, "group"), factor(c(1,2,1,2)))
  checkEquals(getColumns(CP, c("value", "group")),
              list(value = c(0.1,0.3,0.2,0.4), group = factor(c(1,2,1,2))))
  checkEquals(colNames(CP), c("value", "group"))

  ## Wrong order of times within id
  continuousData <- data.frame(id = c(1,1,2,2),
                               time = c(2,1,1,2),
                               value = c(0.3,0.1,0.2,0.4),
                               group = factor(c(2,1,1,2))
                               )
  CP <- continuousProcess(continuousData)
  checkEquals(getId(CP), factor(c(1,1,2,2)))
  checkEquals(getTime(CP), c(1,2,1,2))
  checkEquals(getNumerics(CP)[,"value"], c(0.1,0.3,0.2,0.4))
  checkEquals(getColumns(CP, "value"), c(0.1,0.3,0.2,0.4))
  checkEquals(getFactors(CP)[["group"]], factor(c(1,2,1,2)))
  checkEquals(getColumns(CP, "group"), factor(c(1,2,1,2)))
   checkEquals(getColumns(CP, c("value", "group")),
              list(value = c(0.1,0.3,0.2,0.4), group = factor(c(1,2,1,2))))
  checkEquals(colNames(CP), c("value", "group"))

  ## Mixed order of id
  continuousData <- data.frame(id = c(1,2,1,2),
                               time = c(1,1,2,2),
                               value = c(0.1,0.2,0.3,0.4),
                               group = factor(c(1,1,2,2))
                               )
  CP <- continuousProcess(continuousData)
  checkEquals(getId(CP), factor(c(1,1,2,2)))
  checkEquals(getTime(CP), c(1,2,1,2))
  checkEquals(getNumerics(CP)[,"value"], c(0.1,0.3,0.2,0.4))
  checkEquals(getColumns(CP, "value"), c(0.1,0.3,0.2,0.4))
  checkEquals(getFactors(CP)[["group"]], factor(c(1,2,1,2)))
  checkEquals(getColumns(CP, "group"), factor(c(1,2,1,2)))
   checkEquals(getColumns(CP, c("value", "group")),
              list(value = c(0.1,0.3,0.2,0.4), group = factor(c(1,2,1,2))))
  checkEquals(colNames(CP), c("value", "group"))

  ## Mixed order of id, and wrong order of times
  continuousData <- data.frame(id = c(1,2,1,2),
                               time = c(1,2,2,1),
                               value = c(0.1,0.4,0.3,0.2),
                               group = factor(c(1,2,2,1))
                               )
  CP <- continuousProcess(continuousData)
  checkEquals(getId(CP), factor(c(1,1,2,2)))
  checkEquals(getTime(CP), c(1,2,1,2))
  checkEquals(getNumerics(CP)[,"value"], c(0.1,0.3,0.2,0.4))
  checkEquals(getColumns(CP, "value"), c(0.1,0.3,0.2,0.4))
  checkEquals(getFactors(CP)[["group"]], factor(c(1,2,1,2)))
  checkEquals(getColumns(CP, "group"), factor(c(1,2,1,2)))
   checkEquals(getColumns(CP, c("value", "group")),
              list(value = c(0.1,0.3,0.2,0.4), group = factor(c(1,2,1,2))))
  checkEquals(colNames(CP), c("value", "group"))

  ## Other id and position variable names with mixed orders
  continuousData <- data.frame(unitNr = c(1,2,1,2),
                              position = c(1,2,2,1),
                              value = c(0.1,0.4,0.3,0.2),
                              group = factor(c(1,2,2,1))
                               )
  CP <- continuousProcess(continuousData, idVar = "unitNr", positionVar = "position")
  checkEquals(getId(CP), factor(c(1,1,2,2)))
  checkEquals(getTime(CP), c(1,2,1,2))
  checkEquals(getPosition(CP), c(1,2,1,2))
  checkEquals(getNumerics(CP)[,"value"], c(0.1,0.3,0.2,0.4))
  checkEquals(getColumns(CP, "value"), c(0.1,0.3,0.2,0.4))
  checkEquals(getFactors(CP)[["group"]], factor(c(1,2,1,2)))
  checkEquals(getColumns(CP, "group"), factor(c(1,2,1,2)))
   checkEquals(getColumns(CP, c("value", "group")),
              list(value = c(0.1,0.3,0.2,0.4), group = factor(c(1,2,1,2))))
  checkEquals(colNames(CP), c("value", "group"))

  ## Including unit data
  continuousData <- data.frame(id = c(1,1,2,2),
                               time = c(1,2,1,2),
                               value = c(0.1,0.3,0.2,0.4),
                               group = factor(c(1,2,1,2))
                               )
  unitData <- data.frame(id = c(1,2), gender = factor(c("Male", "Female")))
  CP <- continuousProcess(continuousData, unitData = unitData)
  checkEquals(getId(CP), factor(c(1,1,2,2)))
  checkEquals(getTime(CP), c(1,2,1,2))
  checkEquals(getNumerics(CP)[,"value"], c(0.1,0.3,0.2,0.4))
  checkEquals(getColumns(CP, "value"), c(0.1,0.3,0.2,0.4))
  checkEquals(getFactors(CP)[["group"]], factor(c(1,2,1,2)))
  checkEquals(getColumns(CP, "group"), factor(c(1,2,1,2)))
  checkEquals(getColumns(CP, c("gender", "value", "group")),
              list(gender = factor(c("Male", "Male", "Female", "Female")),
                   value = c(0.1,0.3,0.2,0.4), group = factor(c(1,2,1,2))))
  checkEquals(colNames(CP), c("gender", "value", "group"))
  checkEquals(getUnitData(CP)[, "gender"], factor(c("Male", "Female")))
  
  ## Mixed order of id, and wrong order of times, wrong order of unit
  ## data
  continuousData <- data.frame(id = c(1,2,1,2),
                               time = c(1,2,2,1),
                               value = c(0.1,0.4,0.3,0.2),
                               group = factor(c(1,2,2,1))
                               )
  unitData <- data.frame(id = c(2,1), gender = factor(c("Female", "Male")))
  CP <- continuousProcess(continuousData, unitData = unitData)
  checkEquals(getId(CP), factor(c(1,1,2,2)))
  checkEquals(getTime(CP), c(1,2,1,2))
  checkEquals(getNumerics(CP)[,"value"], c(0.1,0.3,0.2,0.4))
  checkEquals(getColumns(CP, "value"), c(0.1,0.3,0.2,0.4))
  checkEquals(getFactors(CP)[["group"]], factor(c(1,2,1,2)))
  checkEquals(getColumns(CP, "group"), factor(c(1,2,1,2)))
  checkEquals(getColumns(CP, c("gender", "value", "group")),
              list(gender = factor(c("Male", "Male", "Female", "Female")),
                   value = c(0.1,0.3,0.2,0.4), group = factor(c(1,2,1,2))))
  checkEquals(colNames(CP), c("gender", "value", "group"))
  checkEquals(getUnitData(CP)[, "gender"], factor(c("Male", "Female")))
  
}

test.continuousProcessNumeric <- function() {
  continuousData <-  c(0.1,0.3,0.2,0.4)
  CP <- continuousProcess(continuousData)
  checkEquals(getId(CP), factor(c(1,1,1,1)))
  checkEquals(getTime(CP), c(0,1,2,3))
  checkEquals(getNumerics(CP)[,"V"], c(0.1,0.3,0.2,0.4))
  checkEquals(getColumns(CP, "V"), c(0.1,0.3,0.2,0.4))
  checkEquals(getFactors(CP), list())
  checkEquals(colNames(CP), c("V"))
}

test.continuousProcessMatrix <- function() {
  continuousData <-  matrix(c(0.1,0.3,0.2,0.4), 2, 2)
  CP <- continuousProcess(continuousData)
  checkEquals(getId(CP), factor(c(1,1)))
  checkEquals(getTime(CP), c(0, 1))
  checkEquals(getNumerics(CP)[,"V1"], c(0.1,0.3))
  checkEquals(getNumerics(CP)[,"V2"], c(0.2,0.4))
  checkEquals(getColumns(CP, "V1"), c(0.1,0.3))
  checkEquals(getColumns(CP, "V2"), c(0.2,0.4))
  checkEquals(getFactors(CP), list())
  checkEquals(colNames(CP), c("V1", "V2"))
}

test.continuousProcessList <- function() {
  continuousData <- list(id = c(1,1,2,2),
                         time = c(1,2,1,2),
                         value = c(0.1,0.3,0.2,0.4),
                         group = factor(c(1,2,1,2))
                         )
  CP <- continuousProcess(continuousData)
  checkEquals(getId(CP), factor(c(1,1,2,2)))
  checkEquals(getTime(CP), c(1,2,1,2))
  checkEquals(getNumerics(CP)[,"value"], c(0.1,0.3,0.2,0.4))
  checkEquals(getColumns(CP, "value"), c(0.1,0.3,0.2,0.4))
  checkEquals(getFactors(CP)[["group"]], factor(c(1,2,1,2)))
  checkEquals(getColumns(CP, "group"), factor(c(1,2,1,2)))
  checkEquals(colNames(CP), c("value", "group"))

  continuousData$id <- c(1,2,2)
  checkException(continuousProcess(continuousData), silent = TRUE)
  continuousData$id <- c(1,1,1,2,2)
  checkException(continuousProcess(continuousData), silent = TRUE)
}

test.continuousProcesscontinuousProcess <- function() {
  continuousData <- data.frame(id = c(1,1,2,2),
                               time = c(1,2,1,2),
                               value = c(0.1,0.3,0.2,0.4),
                               group = factor(c(1,2,1,2))
                               )
  CP <- continuousProcess(continuousData)[1:3,2]
  CP2 <- continuousProcess(CP)
  checkEquals(getId(CP), getId(CP2))
  checkEquals(getTime(CP), getTime(CP2))
  checkEquals(getColumns(CP,1), getColumns(CP2,1))
  checkEquals(getColumns(CP, "group"), getColumns(CP2, "group"))
}

test.markedPointProcessDataFrame <- function() {
  ## Check that everything comes back as expected
  continuousData <- data.frame(id = c(1,1,2,2),
                               time = c(1,2,1,2),
                               value = c(0.1,0.3,0.2,0.4),
                               group = factor(c(1,2,1,2))
                               )
  pointData <- data.frame(id = c(1,1,2,2),
                          time = c(1.1,1.4,1.3,1.7),
                          markType = c("pA", "pB", "pB", "pB"),
                          size = c(0.1,0.4,2,0.4))
  MP <- markedPointProcess(pointData, continuousData, coarsen = 'right')
  checkEquals(getId(MP), factor(c(1,1,2,2)))
  checkEquals(getTime(MP), c(1,2,1,2))
  checkEquals(getPointTime(MP), c(2,2,2,2))
  checkEquals(getMarkType(MP), factor(c("pA", "pB", "pB", "pB")))
  checkEquals(getMarkValue(MP), data.frame(size = c(0.1,0.4,2,0.4)))
  checkEquals(getNumerics(MP)[,"value"], c(0.1,0.3,0.2,0.4))
  checkEquals(getColumns(MP, "value"), c(0.1,0.3,0.2,0.4))
  checkEquals(getFactors(MP)[["group"]], factor(c(1,2,1,2)))
  checkEquals(getColumns(MP, "group"), factor(c(1,2,1,2)))
  checkEquals(getColumns(MP, "pA"), 2)
  checkEquals(getColumns(MP, "pB"), c(2,2,2))
  checkEquals(getColumns(MP, "size"), c(0.1,0.4,2,0.4))
  checkEquals(getColumns(MP, c("value", "group")),
              list(value = c(0.1,0.3,0.2,0.4), group = factor(c(1,2,1,2))))
  checkEquals(colNames(MP), c("value", "group", "pA", "pB", "size"))


  MP <- markedPointProcess(pointData, continuousData, coarsen = 'left')
  checkEquals(getId(MP), factor(c(1,1,2,2)))
  checkEquals(getTime(MP), c(1,2,1,2))
  checkEquals(getPointTime(MP), c(1,1,1,1))
  checkEquals(getMarkType(MP), factor(c("pA", "pB", "pB", "pB")))
  checkEquals(getMarkValue(MP), data.frame(size = c(0.1,0.4,2,0.4)))
  checkEquals(getNumerics(MP)[,"value"], c(0.1,0.3,0.2,0.4))
  checkEquals(getColumns(MP, "value"), c(0.1,0.3,0.2,0.4))
  checkEquals(getFactors(MP)[["group"]], factor(c(1,2,1,2)))
  checkEquals(getColumns(MP, "group"), factor(c(1,2,1,2)))
  checkEquals(getColumns(MP, "pA"), 1)
  checkEquals(getColumns(MP, "pB"), c(1,1,1))
  checkEquals(getColumns(MP, "size"), c(0.1,0.4,2,0.4))
  checkEquals(getColumns(MP, c("value", "group")),
              list(value = c(0.1,0.3,0.2,0.4), group = factor(c(1,2,1,2))))
  checkEquals(colNames(MP), c("value", "group", "pA", "pB", "size"))

  MP <- markedPointProcess(pointData, continuousData)
  checkEquals(getId(MP), factor(c(1,1,1,1,2,2,2,2)))
  checkEquals(getTime(MP), c(1,1.1,1.4,2,1,1.3,1.7,2))
  checkEquals(getPointTime(MP), c(1.1,1.4,1.3,1.7))
  checkEquals(getMarkType(MP), factor(c("pA", "pB", "pB", "pB")))
  checkEquals(getMarkValue(MP), data.frame(size = c(0.1,0.4,2,0.4)))
  checkEquals(getNumerics(MP)[,"value"], c(0.1,0.1,0.1,0.3,0.2,0.2,0.2,0.4))
  checkEquals(getColumns(MP, "value"), c(0.1,0.1,0.1,0.3,0.2,0.2,0.2,0.4))
  checkEquals(getFactors(MP)[["group"]], factor(c(1,1,1,2,1,1,1,2)))
  checkEquals(getColumns(MP, "group"), factor(c(1,1,1,2,1,1,1,2)))
  checkEquals(getColumns(MP, "pA"), 1.1)
  checkEquals(getColumns(MP, "pB"), c(1.4,1.3,1.7))
  checkEquals(getColumns(MP, "size"), c(0.1,0.4,2,0.4))
  checkEquals(getColumns(MP, c("value", "group")),
              list(value = c(0.1,0.1,0.1,0.3,0.2,0.2,0.2,0.4),
                   group = factor(c(1,1,1,2,1,1,1,2))))
  checkEquals(colNames(MP), c("value", "group", "pA", "pB", "size"))

  ## Wrong order of times within id
  pointData <- data.frame(id = c(1,1,2,2),
                          time = c(1.4,1.1,1.7,1.3),
                          markType = c("pB", "pA", "pB", "pB"),
                          size = c(0.4,0.1,0.4,2))
  MP <- markedPointProcess(pointData, continuousData)
  checkEquals(getId(MP), factor(c(1,1,1,1,2,2,2,2)))
  checkEquals(getTime(MP), c(1,1.1,1.4,2,1,1.3,1.7,2))
  checkEquals(getPointTime(MP), c(1.1,1.4,1.3,1.7))
  checkEquals(getMarkType(MP), factor(c("pA", "pB", "pB", "pB")))
  checkEquals(getMarkValue(MP), data.frame(size = c(0.1,0.4,2,0.4)))
  checkEquals(getNumerics(MP)[,"value"], c(0.1,0.1,0.1,0.3,0.2,0.2,0.2,0.4))
  checkEquals(getColumns(MP, "value"), c(0.1,0.1,0.1,0.3,0.2,0.2,0.2,0.4))
  checkEquals(getFactors(MP)[["group"]], factor(c(1,1,1,2,1,1,1,2)))
  checkEquals(getColumns(MP, "group"), factor(c(1,1,1,2,1,1,1,2)))
  checkEquals(getColumns(MP, "pA"), 1.1)
  checkEquals(getColumns(MP, "pB"), c(1.4,1.3,1.7))
  checkEquals(getColumns(MP, "size"), c(0.1,0.4,2,0.4))
  checkEquals(getColumns(MP, c("value", "group")),
              list(value = c(0.1,0.1,0.1,0.3,0.2,0.2,0.2,0.4),
                   group = factor(c(1,1,1,2,1,1,1,2))))
  checkEquals(colNames(MP), c("value", "group", "pA", "pB", "size"))


  ## Mixed order of id
  pointData <- data.frame(id = c(1,2,1,2),
                          time = c(1.1,1.3,1.4,1.7),
                          markType = c("pA", "pB", "pB", "pB"),
                          size = c(0.1,2,0.4,0.4))
  MP <- markedPointProcess(pointData, continuousData)
  checkEquals(getId(MP), factor(c(1,1,1,1,2,2,2,2)))
  checkEquals(getTime(MP), c(1,1.1,1.4,2,1,1.3,1.7,2))
  checkEquals(getPointTime(MP), c(1.1,1.4,1.3,1.7))
  checkEquals(getMarkType(MP), factor(c("pA", "pB", "pB", "pB")))
  checkEquals(getMarkValue(MP), data.frame(size = c(0.1,0.4,2,0.4)))
  checkEquals(getNumerics(MP)[,"value"], c(0.1,0.1,0.1,0.3,0.2,0.2,0.2,0.4))
  checkEquals(getColumns(MP, "value"), c(0.1,0.1,0.1,0.3,0.2,0.2,0.2,0.4))
  checkEquals(getFactors(MP)[["group"]], factor(c(1,1,1,2,1,1,1,2)))
  checkEquals(getColumns(MP, "group"), factor(c(1,1,1,2,1,1,1,2)))
  checkEquals(getColumns(MP, "pA"), 1.1)
  checkEquals(getColumns(MP, "pB"), c(1.4,1.3,1.7))
  checkEquals(getColumns(MP, "size"), c(0.1,0.4,2,0.4))
  checkEquals(getColumns(MP, c("value", "group")),
              list(value = c(0.1,0.1,0.1,0.3,0.2,0.2,0.2,0.4),
                   group = factor(c(1,1,1,2,1,1,1,2))))
  checkEquals(colNames(MP), c("value", "group", "pA", "pB", "size"))
  
  ## Other markType variable name with mixed orders
  pointData <- data.frame(id = c(1,1,2,2),
                          time = c(1.1,1.4,1.3,1.7),
                          someMarks = c("pA", "pB", "pB", "pB"),
                          size = c(0.1,0.4,2,0.4))[c(4,2,3,1),]
  MP <- markedPointProcess(pointData, continuousData, markVar = "someMarks")
  checkEquals(getId(MP), factor(c(1,1,1,1,2,2,2,2)))
  checkEquals(getTime(MP), c(1,1.1,1.4,2,1,1.3,1.7,2))
  checkEquals(getPointTime(MP), c(1.1,1.4,1.3,1.7))
  checkEquals(getMarkType(MP), factor(c("pA", "pB", "pB", "pB")))
  checkEquals(getMarkValue(MP), data.frame(size = c(0.1,0.4,2,0.4)))
  checkEquals(getNumerics(MP)[,"value"], c(0.1,0.1,0.1,0.3,0.2,0.2,0.2,0.4))
  checkEquals(getColumns(MP, "value"), c(0.1,0.1,0.1,0.3,0.2,0.2,0.2,0.4))
  checkEquals(getFactors(MP)[["group"]], factor(c(1,1,1,2,1,1,1,2)))
  checkEquals(getColumns(MP, "group"), factor(c(1,1,1,2,1,1,1,2)))
  checkEquals(getColumns(MP, "pA"), 1.1)
  checkEquals(getColumns(MP, "pB"), c(1.4,1.3,1.7))
  checkEquals(getColumns(MP, "size"), c(0.1,0.4,2,0.4))
  checkEquals(getColumns(MP, c("value", "group")),
              list(value = c(0.1,0.1,0.1,0.3,0.2,0.2,0.2,0.4),
                   group = factor(c(1,1,1,2,1,1,1,2))))
  checkEquals(colNames(MP), c("value", "group", "pA", "pB", "size"))


  ## Event times outside of range
  pointData <- data.frame(id = c(1,1,2,2),
                          time = c(0.5,1.4,1.3,2.1),
                          markType = c("pA", "pB", "pB", "pB"),
                          size = c(0.1,0.4,2,0.4))
  MP <- markedPointProcess(pointData, continuousData)
  checkEquals(getId(MP), factor(c(1,1,1,1,2,2,2,2)))
  checkEquals(getTime(MP), c(0.5,1,1.4,2,1,1.3,2,2.1))
  checkEquals(getPointTime(MP), c(0.5,1.4,1.3,2.1))
  checkEquals(getMarkType(MP), factor(c("pA", "pB", "pB", "pB")))
  checkEquals(getMarkValue(MP), data.frame(size = c(0.1,0.4,2,0.4)))
  checkEquals(getNumerics(MP)[,"value"], c(0.1,0.1,0.1,0.3,0.2,0.2,0.4,0.4))
  checkEquals(getColumns(MP, "value"), c(0.1,0.1,0.1,0.3,0.2,0.2,0.4,0.4))
  checkEquals(getFactors(MP)[["group"]], factor(c(1,1,1,2,1,1,2,2)))
  checkEquals(getColumns(MP, "group"), factor(c(1,1,1,2,1,1,2,2)))
  checkEquals(getColumns(MP, "pA"), 0.5)
  checkEquals(getColumns(MP, "pB"), c(1.4,1.3,2.1))
  checkEquals(getColumns(MP, "size"), c(0.1,0.4,2,0.4))
  checkEquals(getColumns(MP, c("value", "group")),
              list(value = c(0.1,0.1,0.1,0.3,0.2,0.2,0.4,0.4),
                   group = factor(c(1,1,1,2,1,1,2,2))))
  checkEquals(colNames(MP), c("value", "group", "pA", "pB", "size"))

  MP <- markedPointProcess(pointData, continuousData, coarsen = 'right')
  checkEquals(getId(MP), factor(c(1,1,2,2)))
  checkEquals(getTime(MP), c(1,2,1,2))
  checkEquals(getPointTime(MP), c(1,2,2,2))
  checkEquals(getMarkType(MP), factor(c("pA", "pB", "pB", "pB")))
  checkEquals(getMarkValue(MP), data.frame(size = c(0.1,0.4,2,0.4)))
  checkEquals(getNumerics(MP)[,"value"], c(0.1,0.3,0.2,0.4))
  checkEquals(getColumns(MP, "value"), c(0.1,0.3,0.2,0.4))
  checkEquals(getFactors(MP)[["group"]], factor(c(1,2,1,2)))
  checkEquals(getColumns(MP, "group"), factor(c(1,2,1,2)))
  checkEquals(getColumns(MP, "pA"), 1)
  checkEquals(getColumns(MP, "pB"), c(2,2,2))
  checkEquals(getColumns(MP, "size"), c(0.1,0.4,2,0.4))
  checkEquals(getColumns(MP, c("value", "group")),
              list(value = c(0.1,0.3,0.2,0.4), group = factor(c(1,2,1,2))))
  checkEquals(colNames(MP), c("value", "group", "pA", "pB", "size"))


  MP <- markedPointProcess(pointData, continuousData, coarsen = 'left')
  checkEquals(getId(MP), factor(c(1,1,2,2)))
  checkEquals(getTime(MP), c(1,2,1,2))
  checkEquals(getPointTime(MP), c(1,1,1,2))
  checkEquals(getMarkType(MP), factor(c("pA", "pB", "pB", "pB")))
  checkEquals(getMarkValue(MP), data.frame(size = c(0.1,0.4,2,0.4)))
  checkEquals(getNumerics(MP)[,"value"], c(0.1,0.3,0.2,0.4))
  checkEquals(getColumns(MP, "value"), c(0.1,0.3,0.2,0.4))
  checkEquals(getFactors(MP)[["group"]], factor(c(1,2,1,2)))
  checkEquals(getColumns(MP, "group"), factor(c(1,2,1,2)))
  checkEquals(getColumns(MP, "pA"), 1)
  checkEquals(getColumns(MP, "pB"), c(1,1,2))
  checkEquals(getColumns(MP, "size"), c(0.1,0.4,2,0.4))
  checkEquals(getColumns(MP, c("value", "group")),
              list(value = c(0.1,0.3,0.2,0.4), group = factor(c(1,2,1,2))))
  checkEquals(colNames(MP), c("value", "group", "pA", "pB", "size"))
 
}
  

 
## test.markedPointProcess <- function() {
##   thisClass <- "MarkedPointProcess"
##   attr(thisClass, "package") <- "processdata"
##   checkException(markedPointProcess())
##   checkEquals(class(markedPointProcess(data.frame(time = 1:10))), thisClass)
## }

## test.colNames <- function() {
##   checkEquals(colNames(continuousProcess(data.frame())), character())
##   CP <- continuousProcess(data.frame(value = 1:10))
##   checkEquals(colNames(CP), "value")
##   MPP <- markedPointProcess(data.frame(time = c(3,5,8.3)), CP)
##   checkEquals(colNames(MPP), c("value", "point"))
##   MPP <- markedPointProcess(data.frame(time = c(3,5,8.3),
##                                        markType = c("A", "A", "B")),
##                             CP)
##   checkEquals(colNames(MPP), c("value", "A", "B")) 
## }

## Helper function to check "[" using integers, characters and
## logicals. Compares if the different indexing methods give
## identical results.

subscripting <- function(x) {
  ## Checks of coherence in subscripting
  columns <- colNames(x)
  id <- factor(getId(x))
  checkIdentical(x[,], x)
  ## Checking subscription using column names
  bool <- rep(FALSE, length(columns))
  for(j in seq_along(columns))  {
    checkIdentical(x[ ,columns[j]], x[ ,j])
    bool[j] <- TRUE
    checkIdentical(x[ ,bool], x[ ,j])
    bool[j] <- FALSE
  }
  ## Checking subscription using logicals
  bool <- rep(FALSE, length(id))
  for(i in seq_along(id)) {
    bool[i] <- TRUE
    checkIdentical(x[bool, ], x[i, ])
    bool[i] <- FALSE
  }
  for(i in levels(id)) {
    checkIdentical(x[i, ], x[which(id == i), ])    
  }
  ## Checking aspects of row subscription
  checkIdentical(x[TRUE,], x)  
  checkIdentical(getUnitData(x[,FALSE]), getUnitData(x)[, FALSE])
  checkIdentical(x[seq_along(id)[c(TRUE,FALSE)],], x[c(TRUE, FALSE),])
  invisible()
}


test.subscripting <- function() {
  unitData <- data.frame(A = c(1,2), B = c(3,4))
  continuousData <- data.frame(id = c(1,1,2,2),
                               time = c(1,2,1,2),
                               value = c(0.1,0.3,0.2,0.4),
                               group = factor(c(0, 1, 0, 1)))
  pointData <- data.frame(id = c(1,1,2,2),
                          time = c(1.1,1.4,1.3,1.7),
                          markType = c("pA", "pB", "pB", "pB"),
                          size = c(0.1,0.4,2,0.4))
  PD <- process(unitData)
  CP <- continuousProcess(continuousData, unitData)
  MP <- markedPointProcess(pointData, CP)
  subscripting(PD)
  subscripting(CP)
  subscripting(MP)

  subscripting(PD[2:3, ])
  subscripting(CP[2:3, ])
  subscripting(MP[2:3, ])

  subscripting(PD["1", ])
  subscripting(CP["1", ])
  subscripting(MP["1", ])

  subscripting(PD[, 1])
  subscripting(CP[, c(1,3)])
  subscripting(MP[, c(1,3,5)])

  subscripting(PD[1:3, 1])
  subscripting(CP[1:3, c(1,3)])
  subscripting(MP[1:3, c(1,3,5)])

  checkEquals(class(MP[, 1:4, drop = TRUE]), class(CP))
  checkEquals(dim(MP[c(1,3:8), ]), c(7,7))
  checkEquals(dim(MP[c(1,3:8), , drop = TRUE]), c(7,6))
  checkEquals(MP[c(1, 3:8), c(1, 5:6), drop = TRUE],
              MP[, c(1, 5:6), drop = TRUE][c(1, 3:8), , drop = TRUE])
  checkEquals(dim(MP[c(1,4,5,8), , drop = TRUE]), c(4,4))

  checkEquals(dim(MP[1, 1:7]), c(1,7))
  checkEquals(colNames(MP[1, 1:7]), c("A", "B", "value", "group", "pA", "pB", "size"))
  checkEquals(dim(MP[1, 1:7, drop = TRUE]), c(1,4))
  checkEquals(colNames(MP[1, 1:7, drop = TRUE]),  c("A", "B", "value", "group"))
  
  checkEquals(getPointPosition(MP[2, ][1, ]), 1.1)
  
}



## Helper function to check subsetting. Combines the two expressions
## subset1 and subset2 in different ways and compares the result of
## evaluating the expressions to logical on extracted variables with
## direct calls (succesive or combined) of the subset function.

subsetting <- function(x, subset1, subset2) {
  e1 <- substitute(subset1)
  var1 <- all.vars(e1)
  getVar <- function(v) {
    if(v == "id")
      return(getId(x))
    if(v == "time")
      return(getTime(x))
    return(getColumns(x, v))
  }
  sub1 <- eval(e1, sapply(var1, getVar, simplify = FALSE))
  checkIdentical(do.call(subset, list(x, e1)), x[sub1,])
  if(!missing(subset2)) {
    e2 <- substitute(subset2)
    var2 <- all.vars(e2)
    sub2 <- eval(e2, sapply(var2, getVar, simplify = FALSE))
    checkIdentical(do.call(subset, list(x, e2)), x[sub2,])
    checkIdentical(do.call(subset, list(do.call(subset, list(x, e1)), e2)),
                   x[sub1 & sub2, ])
    e <- parse(text = paste(deparse(e1), "&", deparse(e2)))[[1]]
    checkIdentical(do.call(subset, list(x, e)),
                   x[sub1 & sub2, ])
    e <- parse(text = paste(deparse(e1), "|", deparse(e2)))[[1]]
    checkIdentical(do.call(subset, list(x, e)),
                   x[sub1 | sub2, ])
  }
  
  invisible()
}

test.subsetting <- function() {
  unitData <- data.frame(A = c(1,2), B = c(3,4))
  continuousData <- data.frame(id = c(1,1,2,2),
                               time = c(1,2,1,2),
                               value = c(0.1,0.3,0.2,0.4),
                               group = factor(c(0, 1, 0, 1)))
  pointData <- data.frame(id = c(1,1,2,2),
                          time = c(1.1,1.4,1.3,1.7),
                          markType = c("pA", "pB", "pB", "pB"),
                          size = c(0.1,0.4,2,0.4))
  PD <- process(unitData)
  CP <- continuousProcess(continuousData, unitData)
  MP <- markedPointProcess(pointData, CP)
  ## Checking 'ProcessData' subsetting using unit data variables and
  ## the id variable.
  subsetting(PD, A < 2, id == 1)
  subsetting(PD, A > 1, id == 3)
  subsetting(PD, B < 3, id == 1)
  subsetting(PD, B > 2, id == 2)
  ## Checking 'ContinuousProcess' subsetting using unit data variables and
  ## the id variable.
  subsetting(CP, A < 2, id == 1)
  subsetting(CP, A > 1, id == 3)
  subsetting(CP, B < 3, id == 1)
  subsetting(CP, B > 2, id == 2)
  ## Checking 'MarkedPointProcess' subsetting using unit data variables and
  ## the id variable.
  subsetting(MP, A < 2, id == 1)
  subsetting(MP, A > 1, id == 3)
  subsetting(MP, B < 3, id == 1)
  subsetting(MP, B > 2, id == 2)
  ## Checking 'ContinuousProcess' subsetting using value and group variables and
  ## the id variable.
  subsetting(CP, value < 0.2, id == 1)
  subsetting(CP, value > 0.3, id == 3)
  subsetting(CP, group == 0, id == 1)
  subsetting(CP, group == 1, id == 2)
  ## Checking 'MarkedPointProcess' subsetting using value and group variables and
  ## the id variable.
  subsetting(MP, value < 0.2, id == 1)
  subsetting(MP, value > 0.3, id == 3)
  subsetting(MP, group == 0, id == 1)
  subsetting(MP, group == 1, id == 2)
  ## Checking 'ContinuousProcess' subsetting using value and group variables and
  ## the unit data variables.
  subsetting(CP, value < 0.2, A < 1)
  subsetting(CP, value > 0.3, B > 3)
  subsetting(CP, group == 0, A < 1)
  subsetting(CP, group == 1, B > 3)
  ## Checking 'MarkedPointProcess' subsetting using value and group variables and
  ## the unit data variables.
  subsetting(MP, value < 0.2, A < 1)
  subsetting(MP, value > 0.3, B > 3)
  subsetting(MP, group == 0, A < 1)
  subsetting(MP, group == 1, B > 3)

  ## Checking 'MarkedPointProcess' subsetting of points.
  MPsub <- subset(MP, pointSubset = size > 0.3)
  checkEquals(getPointTime(MPsub), c(1.4,1.3,1.7))
  checkEquals(getPointId(MPsub), factor(c(1,2,2)))
  checkEquals(dim(MPsub), c(8,7))
  MPsub <- subset(MP, pointSubset = size > 0.5)
  checkEquals(getPointTime(MPsub), 1.3)
  checkEquals(getPointId(MPsub), factor(2))
  checkEquals(dim(MPsub), c(8,7))
         
  ## Checking 'ProcessData' selection of rows.
  checkEquals(subset(PD, select = A), PD[,1])
  checkEquals(subset(PD, select = B), PD[,2])

  ## Checking 'ContinuousProcess' selection of rows.
  checkEquals(subset(CP, select = A), CP[, 1])
  checkEquals(subset(CP, select = -A), CP[, -1])
  checkEquals(subset(CP, select = A:value), CP[, 1:3])
  checkEquals(subset(CP, select = c(value,group)), CP[, 3:4])

  ## Checking 'MarkedPointProcess' selection of rows.
  checkEquals(subset(MP, select = -pA), MP[, -5])
  checkEquals(subset(MP, select = c(A, value, pB)), MP[, c(1,3,6)])
  checkEquals(subset(MP, select = A:size), MP)
  checkEquals(subset(MP, select = c(A, size)), MP[, c(1,7)])

}
