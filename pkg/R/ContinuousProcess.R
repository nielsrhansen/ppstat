## A factor.frame is an informal S3 class extending a data.frame.  A
## factor.frame is supposed to have three or four columns. The
## 'variable' column and 'value' column are factors specifying which
## variables takes which values at the positions given by the
## 'position' column. An 'id' column is optional. All variables need
## to have the same factor levels.

range2factor.frame <- function(rangeData, id = 'id', start = 'start',
                               end = 'end', variable = 'variable',
                               delta = 1, frameData = NULL, ...) {
  ## rangeData is assumed to have four columns.

  rangeData[ , id] <- as.factor(rangeData[ , id])
  rangeData[ , end] <- rangeData[ , end] + delta
  
  if(is.null(frameData)) {
    ip <- split(seq_len(dim(rangeData)[1]), rangeData$id)
    idStarts <- sapply(seq_along(ip), function(i) min(rangeData[ip[[i]], start] - delta))
    idEnds <- sapply(seq_along(ip), function(i) max(rangeData[ip[[i]], end] + delta))
    frameData <- data.frame(levels(rangeData[ , id]), idStarts, idEnds)
    names(frameData) <- c(id, start, end)
  }
  factorFrame <- reshape2::melt(rangeData, measure.vars = c(start, end), variable_name = ".value")
  names(factorFrame)[match(c(".value", "value"), names(factorFrame))] <- c("value", "position")
  factorFrame$value <- factor(factorFrame$value, levels = c(end, start), labels = c(0, 1))
  factorFrame$variable <- as.factor(factorFrame$variable)
  
  ii <- rep(seq_along(levels(factorFrame[ , id])),
            each = length(levels(factorFrame[ , variable])))

  startFrame <- cbind(frameData[ii, id, drop = FALSE],
                      levels(factorFrame[ , variable]),
                      factor(0, levels = c(0, 1)),
                      frameData[ii, start, drop = FALSE])

  endFrame <- cbind(frameData[ii, id, drop = FALSE],
                    levels(factorFrame[ , variable]),
                    factor(0, levels = c(0, 1)),
                    frameData[ii, end, drop = FALSE])
                      
  names(startFrame) <- names(endFrame) <- names(factorFrame)
  factorFrame <- rbind(factorFrame, startFrame, endFrame)
  factorFrame <- factorFrame[order(factorFrame[ , id], factorFrame$position, factorFrame$value), ]
  
  class(factorFrame) <- c("factor.frame", "data.frame")
  return(factorFrame)
}


setMethod("continuousProcess", "factor.frame",
          function(continuousData, unitData = data.frame(), metaData = list(),
                   positionVar = 'time', idVar = 'id',
                   valueVar = 'value',  variableVar = 'variable', equiDistance = 0, ...) {

            ip <- split(seq_len(dim(continuousData)), continuousData[ , idVar])
            positions <- sapply(names(ip),
                                function(i) continuousData[ip[[i]], positionVar])
            variables <- sapply(names(ip),
                                function(i) continuousData[ip[[i]], variableVar])
            varLevels <- levels(continuousData[ , variableVar])
            value <- continuousData[ , valueVar]

            dup <- which(unlist(lapply(positions, duplicated)))
            if(length(dup) > 0)
              continuousData <- continuousData[-dup, ]
            
            continuousData <- continuousProcess(as.list(continuousData[ , c(idVar, positionVar)]),
                                                unitData = unitData,
                                                metaData = metaData,
                                                positionVar = positionVar,
                                                idVar = idVar,
                                                equiDistance = equiDistance)
            cpositions <- split(getPosition(continuousData),
                                getId(continuousData))

            for(v in varLevels) {
              iv <- unlist(lapply(names(ip),
                                  function(i) {
                                    j <- which(variables[[i]] == v)
                                    c(1, ip[[i]][j] + 1)[findInterval(cpositions[[i]],
                                                                      positions[[i]][j]) + 1]
                                  }), use.names = FALSE)
              assign(paste("var", "_", v, sep = ""),
                     factor(c(1, value)[iv], labels = levels(value)),
                     envir = continuousData@valueEnv)
            }
            continuousData@factorColNames <- varLevels
            return(continuousData)            
          }
          )

setAs("ContinuousProcess", "list",
      def = function(from) {        
        to <- c(list(getId(from),
                     getPosition(from)),
                getColumns(from, c(colNames(from, "numeric"), colNames(from, "factor")), drop = FALSE))
        names(to)[1:2] <- c(from@idVar, from@positionVar)
        return(to)
      }
      )
        
setMethod("continuousProcess", "data.frame",
          function(continuousData, unitData = data.frame(), metaData = list(), positionVar = 'time', idVar = 'id', equiDistance = 0, ...) {
            callGeneric(as.list(continuousData), unitData = unitData, metaData = metaData,
                        positionVar = positionVar, idVar = idVar,  equiDistance =  equiDistance,
                        .dont_test_dimensions = TRUE, ...)
          }
          )

setMethod("continuousProcess", "numeric",
          function(continuousData, unitData = data.frame(), metaData = list(), positionVar = 'time', idVar = 'id', equiDistance = 0, ...) {
            callGeneric(list(V = continuousData), unitData = unitData, metaData = metaData,
                        positionVar = positionVar, idVar = idVar,  equiDistance =  equiDistance,
                        .dont_test_dimensions = TRUE, ...)
          }
          )

setMethod("continuousProcess", "matrix",
          function(continuousData, unitData = data.frame(), metaData = list(), positionVar = 'time', idVar = 'id',  equiDistance = 0, ...) {
            callGeneric(as.data.frame(continuousData), unitData = unitData, metaData = metaData,
                        positionVar = positionVar, idVar = idVar, equiDistance = equiDistance,
                        .dont_test_dimensions = TRUE, ...)
          }
          )

setMethod("continuousProcess", "ContinuousProcess",
          function(continuousData, ...) {
            contData <- as(continuousData, "list")
                               
            callGeneric(continuousData = contData,
                        unitData = getUnitData(continuousData),
                        metaData = continuousData@metaData,
                        positionVar = continuousData@positionVar,
                        idVar = continuousData@idVar,
                        .dont_test_dimensions = TRUE, ...)
          }
          )
                   
setMethod("continuousProcess", "list",
          function(continuousData, unitData = data.frame(), metaData = list(), positionVar = 'time', idVar = 'id', equiDistance = 0, ...){

            ## Checking, if required, if the dimensions are correct
            ## and that there are names on all entries in the list.
            
            args <- list(...)
            if(!(".dont_test_dimensions" %in% names(args) &&
                 args$.dont_test_dimensions)) {
              ## TODO: What to do with a list containing combinations of vectors and matrices?           
              l <- sapply(continuousData, length, USE.NAMES = FALSE)
              if(any(l != l[1]))
                stop("The list entries in 'continuousData' are not of the same length.")
              if(is.null(names(continuousData))) {
                colNames <- paste("V", seq_along(continuousData), sep = "")
              } else {
                colNames <- names(continuousData)
              }
              emptyNames <- colNames == ""
              if(any(emptyNames)) 
                colNames[emptyNames] <- paste("V", seq_len(sum(emptyNames)), sep = "")
                                
                names(continuousData) <- colNames
            } else {
              colNames <- names(continuousData)
            }

            ## Extracting or setting the 'id' variable.
            if(!(idVar %in% colNames)) {
              if(is.null(dim(continuousData[[1]]))) {
                d1 <- length(continuousData[[1]])
              } else {
                d1 <- dim(continuousData[[1]])[1]
              }
              id <- factor(rep("1", d1), levels = "1")
            } else {
              id <- as.factor(continuousData[[idVar]])
            }
            
            ## Sorting the rows if needed.
            ord <- NULL
            if(is.unsorted(id)) {
              ord <- order(id)              
              id <- id[ord]
            }
            idLevels <- split(seq_along(id), id)
            
            ## Extracting or setting the 'position' variable
            if(!is.numeric(equiDistance) | equiDistance < 0) 
              stop("The argument 'equiDistance' is not a non-negative number or equal to 'auto'.")
            
            if(!(positionVar %in% colNames)) {
              position <- numeric(length(id))
              if(equiDistance == 'auto') {
                equiDistance <- eqd <- 1
              } else if(equiDistance == 0) {
                eqd <- 1
              } else {
                eqd <- equiDistance
              } 
              for(v in levels(id)) {
                position[idLevels[[v]]] <- eqd*c(0L, seq_len(length(idLevels[[v]])-1))
              }
            } else {
              if(is.null(ord)) {
                position <- continuousData[[positionVar]]
              } else {
                position <- continuousData[[positionVar]][ord]
              }
              if(equiDistance == 'auto') {
                uniqueDiff <- unique(unlist(lapply(idLevels,
                                                   function(i) {
                                                     unique(diff(position[i]))
                                                   }
                                                   )))
                if(max(uniqueDiff) - min(uniqueDiff) < .Machine$double.eps ^ 0.5) {
                  equiDistance <- median(uniqueDiff)
                } else {
                  equiDistance <- 0
                }
              }
            }
            
            ## Sorting the rows if needed
            if(any(sapply(levels(id),
                          function(i) is.unsorted(position[idLevels[[i]]])))) {
              ## Computing the correct order within id.
              ord2 <- unlist(lapply(levels(id),
                                   function(i) idLevels[[i]][order(position[idLevels[[i]]])]),
                            use.names=FALSE)
              position <- position[ord2]
              if(!is.null(ord)) {
                ord <- ord[ord2]
              } else {
                ord <- ord2
              }
            }

            ## Setting up the unit data
            if(dim(unitData)[2] == 0)
              unitData <- data.frame(row.names = levels(id))
            if(idVar %in% names(unitData)) {
              row.names(unitData) <- unitData[ , names(unitData) == idVar]
              unitData <- unitData[ , names(unitData) != idVar, drop = FALSE]
            }
            if(!all(levels(id) %in% row.names(unitData))) {
              stop("The 'unitData' data frame does not contain data for all units.")
            }
            unitData <- unitData[levels(id), , drop = FALSE] 
            if(any(row.names(unitData) != levels(id))) {
              ord <- match(levels(id), row.names(unitData))
              unitData <- unitData[ord, ]
            }

            ## Setting up the environment for data storage.
            valueEnv <- new.env(parent = .GlobalEnv)
            valueEnv$id <- id
            valueEnv$position <- position
            varNames <- colNames[!(colNames %in% c(idVar, positionVar))]
            numerics <- sapply(continuousData, is.numeric)[!(colNames %in% c(idVar, positionVar))]
            
            if(is.null(ord)) {
              for(v in varNames) {
                assign(paste("var", "_", v, sep = ""),
                       continuousData[[v]], envir = valueEnv)
              }
            } else {
              for(v in varNames) {
                assign(paste("var", "_", v, sep = ""),
                       continuousData[[v]][ord], envir = valueEnv)
              }
            }
                   
            object <- new("ContinuousProcess",
                          metaData = metaData,
                          equiDistance = equiDistance,
                          iSubset = -1L,
                          jSubset = -1L,
                          idVar = idVar,
                          positionVar = positionVar,
                          factorColNames = varNames[!numerics],
                          numericColNames = varNames[numerics],
                          valueEnv = valueEnv)

            setUnitData(object) <- unitData
            return(object)
          }
          )

setMethod("colNames", c("ContinuousProcess", "missing"),
          function(object, type, ...) {
            colnames <- c(object@numericColNames, object@factorColNames)
            if(!identical(object@jSubset[1], -1L)) 
              colnames <- colnames[object@jSubset]

            colnames <- c(callNextMethod(object), colnames)
            return(colnames)
          }
          )

setMethod("colNames", c("ContinuousProcess", "character"),
          function(object, type, ...) {
            colnames <- callGeneric(object = object, ...)
            if(type == "factor") {
              colnames <- colnames[colnames %in% object@factorColNames]
            } else if(type == "numeric") {
              colnames <- colnames[colnames %in% object@numericColNames]              
            } else {
              colnames <- callGeneric(as(object, "ProcessData"), type = type, ...) 
            }
            return(colnames)
          }
          )

setMethod("dim", "ContinuousProcess",
          function(x) {
            if(identical(x@iSubset[1], -1L)) {
              d1 <- length(x@valueEnv$id)
            } else {
              d1 <- length(x@iSubset)
            }

            if(identical(x@jSubset[1], -1L)) {
              d2 <- length(x@factorColNames) + length(x@numericColNames) 
            } else {
              d2 <- length(x@jSubset)
            }
            d2 <- d2 + callNextMethod(x)[2]

            return(c(d1,d2))
          }
          )

setMethod("getEquiDistance", "ContinuousProcess",
          function(object, ...) {
            ## TODO: Can this way to handle subsets wrt equi-distance
            ## be improved? This solution just ignores the
            ## equiDistance slot for subsets!
            if(identical(object@iSubset[1], -1L)) {
              return(object@equiDistance)
            } else {
              return(0)
            }
          }
          )

setMethod("iSubset", "ContinuousProcess",
          function(object) {
            if(identical(object@iSubset[1], -1L)) {
              i <- seq_along(object@valueEnv$id)
            } else {
              i <- object@iSubset
            }
              
            return(i)
          }
          )

setMethod("jSubset", "ContinuousProcess",
          function(object) {
            if(identical(object@jSubset[1], -1L)) {
              j <- seq_len(length(object@factorColNames) + length(object@numericColNames))
            } else {
              j <- object@jSubset
            }
              
            return(j)
          }
          )

setReplaceMethod("iSubset", c(object = "ContinuousProcess", value = "numeric"),
                 function(object, value) {
                   value <- value[!is.na(value)]
                   if(length(value) == length(object@valueEnv$id) && 
                      identical(value, seq_along(object@valueEnv$id))) {
                     object@iSubset <- -1L
                   } else {              
                     object@iSubset <- value
                   }
                   
                   return(object)
                 }
                 )

setReplaceMethod("jSubset", c(object = "ContinuousProcess", value = "numeric"),
                 function(object, value) {
                   value <- value[!is.na(value)]
                   d2 <- length(object@numericColNames) + length(object@factorColNames)
                   if(length(value) == d2 && identical(value, seq_len(d2))) {
                     object@jSubset <- -1L
                   } else {              
                     object@jSubset <- value
                   }
                   
                   return(object)
                 }
                 )

setMethod("[", c(x = "ContinuousProcess", i = "integer", j = "missing"),
          function(x, i, j, ... , drop = FALSE) {
            iSubset(x) <- iSubset(x)[i]
            as(x, "ProcessData") <- callGeneric(as(x, "ProcessData"), levels(getId(x)), , drop = drop)
            return(x)
          }
          )

setMethod("[", c(x = "ContinuousProcess", i = "missing", j = "character"),
         function(x, i, j, ... , drop = FALSE) {
           if(drop && length(j) == 1) {
             return(getColumns(x, j))
           } else {
             as(x, "ProcessData")  <- callGeneric(as(x, "ProcessData"), ,j)
             jSubset(x) <- which(c(x@numericColNames, x@factorColNames) %in% j)
           }
          
           return(x)
         }
         )

setMethod("getColumns", c("ContinuousProcess", "character"),
          function(object, j, drop = TRUE) {
            checkColumns <- j %in% colNames(object)
            if(!all(checkColumns)) 
              stop(paste(c("No column '", j[!checkColumns][1], "' in the object."),
                         collapse = ""), call. = FALSE)
            
            if(length(j) == 1 && drop) {
              if(j %in% object@unitColNames) {
                column <- object@valueEnv$unitData[getId(object, drop = FALSE), j]
              } else {
                column <- get(paste("var", "_", j, sep = ""),
                              object@valueEnv)
                if(!identical(object@iSubset[1], -1L)) 
                  column <- column[object@iSubset]
              }
            } else {
              column <- list()
              for(jj in j) {
                if(jj %in% object@unitColNames) {
                  column[[jj]] <- object@valueEnv$unitData[getId(object, drop = FALSE), jj]
                } else {
                  column[[jj]] <- get(paste("var", "_", jj, sep = ""),
                                      object@valueEnv)
                  if(!identical(object@iSubset[1], -1L)) 
                    column[[jj]] <- column[[jj]][object@iSubset]
                }
              }
            }
              
            return(column)
          }
          )

setMethod("subset", "ContinuousProcess",
          function(x, subset, select, ...) {
            if (missing(subset)) 
              r <- TRUE 
            else {
              e <- substitute(subset)
              variables <- all.vars(e)

              ## Environment created for evaluation and populated with
              ## relevant variables.
              
              tmpEnv <- new.env(parent = .GlobalEnv)
              if(identical(x@iSubset[1], -1L)) {
                assign(x@idVar, x@valueEnv$id, envir = tmpEnv)
                assign(x@positionVar, x@valueEnv$position, envir = tmpEnv)
              } else {
                if(x@idVar %in% variables) 
                  assign(x@idVar, getId(x), envir = tmpEnv)
                if(x@positionVar %in% variables) 
                  assign(x@positionVar, getPosition(x), envir = tmpEnv)
              }
              for(v in variables[variables %in% colNames(x)]) {
                assign(v, getColumns(x, v), envir = tmpEnv) 
              }
              r <- eval(e, tmpEnv, parent.frame())
              if (!is.logical(r)) 
                stop("'subset' must evaluate to logical")
              r <- r & !is.na(r)
            }
            if (missing(select)) 
              vars <- TRUE
            else {
              nl <- as.list(seq_along(colNames(x)))
              names(nl) <- colNames(x)
              vars <- eval(substitute(select), nl, parent.frame())
            }
            return(x[r, vars])
          }
          )

setMethod("getId", "ContinuousProcess",
          function(object, drop = TRUE, ...) {
            if(identical(object@iSubset[1], -1L)) {
              value <- object@valueEnv$id
            } else {
              value <- object@valueEnv$id[iSubset(object), drop = drop]  ## Default, dropping unused factor levels.
            }
   
            return(value)
           }
          )

setMethod("getFactors", "ContinuousProcess",
          function(object, ...) {
            factors <- getColumns(object, colNames(object, "factor"), drop = FALSE)
           
            return(factors)
           }
          )
          
setMethod("getNumerics", "ContinuousProcess",
          function(object, ...) {
            columns <- getColumns(object, colNames(object, "numeric"), drop = FALSE)
            if(length(columns) == 0) {
              columns <- matrix(nrow = dim(object)[1], ncol = 0)
            } else {
              columns <- do.call(cbind, columns)
            }
            return(columns)
          }
          )

setMethod("getValue", "ContinuousProcess",
          function(object, ...) {
            getNumerics(object, ...)
          }
          )

setMethod("getPlotData", "ContinuousProcess",
          function(object, y = '@bottom', nPoints = 200, allUnitData = FALSE, selectPoints = NULL, dropLevels = 1, ...){

            ## The computation of the data needed for producing a plot
            ## is split into two parts. First all 'factor' variables
            ## are extracted, and a data frame is computed with the
            ## information of changes in the factor values. The result
            ## is a type of 'run length encoding' useful for plotting
            ## horizontal lines, or bars, at the different levels of
            ## the factor in a non-anticipating "limits from the left,
            ## continuous from the right" point of view.
            
            factorPlotData <- data.frame()
            factors <- getFactors(object)
            if(length(factors) > 0) {

              ## The runs are computed for each factor.
              ## TODO: don't extract using getFactors, getValue or getNumerics.
              ## Use the column specific extractor?
             
              lenId <- cumsum(table(getId(object)))
              patterns <- sapply(names(factors),
                                 function(name) {
                                   x <- factors[[name]]
                                   i <- sort(unique(c(which(x[-1L] != x[-length(x)]), lenId)))
                                   uniqueNames <- seq_along(i)
                                   n <- length(i)
                                   uniqueNames <- paste(name, c(uniqueNames, uniqueNames[-n]+1), sep = "")
                                   
                                   i <- c(i, i[-n]+1)
                                   
                                   orderI <- order(i)
                                   i <- i[orderI]
                                   uniqueNames <- uniqueNames[orderI]
                                   
                                   i <- c(1,i)
                                   uniqueNames <- c(paste(name, "1", sep = ""),
                                                    uniqueNames)
                                   
                                 
                                   result <- data.frame(.index = i,
                                                        .group = uniqueNames,
                                                        x[i])
                                   names(result)[3] <- name
                                   if(is.list(dropLevels))
                                     dropLevels <- dropLevels[[name]]
                                   
                                   return(result[!(as.numeric(x[i]) %in% dropLevels), , drop = FALSE])
                                 },
                                 simplify = FALSE)

              ## Removing empty data frames in the list
              patterns <- patterns[sapply(patterns, function(frame) dim(frame)[1] != 0)]             
              patterns <- reshape2::melt(patterns, id.vars = c(1,2))
              index <- patterns$.index
              id = getId(object)
              leftEndIndex <- seq(2, length(index), 2)
              i <- id[index[leftEndIndex]] == id[index[leftEndIndex]+1]
              i <- i & !is.na(i)
              index[leftEndIndex][i] <- index[leftEndIndex][i]+1
                          
              factorPlotData <- data.frame(id = id[index],
                                           position = getPosition(object)[index],
                                           group = patterns$.group,
                                           variable = patterns$variable,
                                           value = paste(patterns$variable,
                                             patterns$value,
                                             sep = "_"))
              names(factorPlotData)[1] <- object@idVar
                          
              if(isTRUE(allUnitData))            
                factorPlotData <- cbind(factorPlotData, getUnitData(object)[factorPlotData$id, , drop = FALSE])

              factorPlotData$type <- as.factor("Track")
              
            }
            
            iList <- unlist(tapply(seq_along(getId(object)),
                                     getId(object),
                                     list,
                                     simplify = FALSE),
                            recursive = FALSE)
            
            i <- unlist(lapply(iList,
                               function(ii) {
                                 if(is.null(ii))
                                   return(NULL)
                                 l <- length(ii)
                                   as.integer(seq(ii[1], ii[l], length.out = min(l,nPoints)))
                               }))
            
            if(!is.null(selectPoints))
              i <- unique(sort(c(i, selectPoints)))
            
            object <- object[i, ]
            tmp <- getNumerics(object)
           
            measureVar <- colnames(tmp)
            ### There was previously a protection using 'I(i)' in this call, but
            ### it created problems with 'melt' from the reshape2
            ### version 1.2.1
            tmp <- cbind(tmp, data.frame(iSubset = i))
            
            continuousPlotData <- data.frame(getId(object))
            names(continuousPlotData)[1] <- object@idVar
            
            if(isTRUE(allUnitData))            
              continuousPlotData <- cbind(continuousPlotData, getUnitData(object)[getId(object), , drop = FALSE])
            if(is.null(measureVar)) {
              continuousPlotData <- cbind(continuousPlotData,
                                          position = getPosition(object),
                                          as.data.frame(tmp))
            } else {
              continuousPlotData <- reshape2::melt(cbind(continuousPlotData,
                                                         position = getPosition(object),
                                                         as.data.frame(tmp)),
                                                   measure.vars = measureVar)
            }
            
            continuousPlotData$type <- as.factor("Continuous")

            if("value" %in% names(continuousPlotData)) {
              limits <- range(continuousPlotData$value)
              breaks <- pretty(limits, 4)
              limits <- range(c(breaks, limits))
              labels <- as.character(breaks)
            } else {
              limits <- c(-1, 0)
              breaks <- numeric()
              labels <- character()
            }

            
             if(y == "@top") {
               y <- "top"
             } else {
               y <- "bottom"
             }
             
            
            plotData <- new("ProcessPlotData",
                            continuousPlotData = continuousPlotData,
                            factorPlotData = factorPlotData,
                            pointPlotData = data.frame(),
                            position = "bottom",
                            limits = limits,
                            breaks = breaks,
                            labels = labels,
                            idVar = object@idVar,
                            positionVar = object@positionVar)
            
            return(plotData)
          }
          )

setMethod("plot", c("ContinuousProcess", "missing"),
          function(x, y, nPoints = 200, ...){
            plotData <- getPlotData(object = x, y = "@bottom", nPoints = nPoints, ...)
            return(plot(plotData, ...))
          }
          )

setMethod("plot", c("ContinuousProcess", "character"),
          function(x, y, nPoints = 200, ...){
            plotData <- getPlotData(object = x, y = y, nPoints = nPoints, ...)
            return(plot(plotData, ...))
          }
          )


setMethod("plot", c("ContinuousProcess", "numeric"),
          function(x, y, nPoints = 200, ...){
            callGeneric(x = x, y = as.character(y), nPoints = nPoints, ...)
          }
          )

setMethod("getPosition", "ContinuousProcess",
          function(object, ...) {
            if(identical(object@iSubset[1], -1L)) {
              value <- object@valueEnv$position
            } else {
              value <- object@valueEnv$position[iSubset(object)]
            }
            
            return(value)
           }
          )

setMethod("getTime", "ContinuousProcess",
          function(object, ...) {        
           return(getPosition(object, ...))
           }
          )

setMethod("summarizeData", "ContinuousProcess",
          function(object, ...) {
            unitData <- getUnitData(object)
            id <- getId(object)
            if(length(id) > 0) {
              splitEntries <- split(seq_along(id), id)
              ranges <- lapply(splitEntries, function(e) signif(range(getPosition(object)[e]), 5))
              if(is.list(ranges)) {
                positionSummary <- as.data.frame(sapply(ranges, function(r) paste("[", r[1],";",r[2],"]", sep="")), stringsAsFactors = FALSE) 
              } else {
                positionSummary <- data.frame(1)[FALSE, ]
              }
              summaryById <- as.numeric(table(id))
              sumVal <- list()
              
              for(j in colNames(object, "factor")) {
                sumVal[[j]] <- rep("Fac", length(splitEntries))
              }

              for(j in colNames(object, "numeric")) {
                column <- getColumns(object, j)
                sumVal[[paste("median(", j ,")", sep ="")]] <- 
                  sapply(splitEntries, function(e) median(column[e]))
              }
              
              summaryById <- cbind(summaryById, positionSummary)

              if(dim(unitData)[2] > 0)
                summaryById <- cbind(summaryById, unitData)

              if(length(sumVal) > 0)
                summaryById <- cbind(summaryById, as.data.frame(sumVal, optional = TRUE))
                                  
              colnames(summaryById)[1:2] <- c("grid-points", paste(object@positionVar,"range", sep = "-"))
            } else {
              summaryById <- unitData
            }
                                                    
            return(summaryById)
          }
          )

setMethod("simpleSummary", "ContinuousProcess",
          function(object, ...) {
            idLevels <- levels(getId(object))
            if(length(idLevels) > 10)
              idLevels <- c(idLevels[1:9], "...", idLevels[length(idLevels)])
            if(length(idLevels) > 1) {
              structure <- paste(length(levels(getId(object))), " units.\n   ",
                                 object@idVar,": ", paste(idLevels, collapse = " "),
                                 "\n\n", sep = "")
            } else {
              structure <- ""
            }
            colnames <- colNames(object)
            if(length(colnames) > 10)
              colnames <- c(colnames[1:9], "...", colnames[length(colnames)])

            if(length(colnames) == 1) {
              variables <- "variable"
            } else {
              variables <- "variables"
            }
             
            structure <- paste(structure,
                               length(colNames(object)), " ", variables, ".\n   ", paste(colnames, collapse = " "), "\n\n", sep = "")
            return(structure)
          }
          )
        
setMethod("show", "ContinuousProcess",
          function(object) {
            d <- dim(object)
            op <- options()
            options(list(quote = FALSE, digits = 3))
            cat(paste("Class:", class(object), "\n"))
            cat(paste("Dimensions:", d[1], "x", d[2], "\n"))
            cat(paste("Index variable:", object@positionVar, "\n\n"))
            cat(simpleSummary(object))
            options(op)
            return(invisible(object))
          }
          )

setMethod("summary", "ContinuousProcess",
           function(object, ...) {
             print(object)
             if(dim(object)[1] > 0) 
               print(summarizeData(object, ...))
             return(invisible(NULL))
           }
           )

setMethod("unsubset", "ContinuousProcess",
          function(x, ...) {
            as(x, "ProcessData") <- callGeneric(as(x, "ProcessData"))
            x@iSubset <- -1L
            x@jSubset <- -1L
            return(x)
          }
          )
