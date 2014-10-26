setMethod("markedPointProcess", c("data.frame", "ContinuousProcess"),
          function(pointData, continuousData, markVar = 'markType', coarsen = NULL, ...) {            

            ## Initialize an empty object - bypasses validity
            ## check. Validity function called before returning object
            ## at the end of the contructor.
            pointProcess <- new("MarkedPointProcess") 
            
            if(continuousData@idVar %in% names(pointData)) {
              id <- pointData[ , continuousData@idVar]
              if(!identical(levels(id), levels(getId(continuousData))))
                id <- factor(id, levels = levels(getId(continuousData)))
            } else {
              id <- factor(rep(getId(continuousData)[1], dim(pointData)[1]))
            }

            ## Sorting the rows if needed.
            if(is.unsorted(id)) {
              ord <- order(id)              
              pointData <-  pointData[ord, , drop = FALSE]
              id <- id[ord]
            }

            if(continuousData@positionVar %in% names(pointData)) {
              position <- pointData[ , continuousData@positionVar]
            } else {
              stop(paste("pointData needs a column named", pointProcess@positionVar))
            }
            
            ip <- split(seq_along(id), id)
            if(any(sapply(names(ip), function(i) is.unsorted(position[ip[[i]]])))) {
              ord <- sapply(names(ip),
                            function(i) order(position[ip[[i]]]), simplify = FALSE)
              ord <- unlist(lapply(names(ip),
                                   function(i) ip[[i]][ord[[i]]]), use.names=FALSE)
              pointData <-  pointData[ord, , drop = FALSE]
              position <- position[ord]
              id <- id[ord]
              ip <- split(seq_along(id), id)
            }
                        
            if(markVar %in% names(pointData)) {
              markType <- pointData[, markVar]
            } else {
              markType <-  rep("point", dim(pointData)[1])
            }
            markValue <- pointData[, !(names(pointData) %in%
                                       c(continuousData@idVar,
                                         continuousData@positionVar,
                                         markVar)),
                                   drop = FALSE]

            pointProcessEnv <- new.env(parent = .GlobalEnv)
            pointProcessEnv$id <- id
            pointProcessEnv$markType <- as.factor(markType)
            for(v in names(markValue)) {
              assign(paste("var_", v, sep = ""),
                     markValue[,v], envir = pointProcessEnv)
            }
            
            pointProcess@iPointSubset <- -1L
            pointProcess@jPointSubset <- -1L
            
            ic <- split(iSubset(continuousData), getId(continuousData))         
            contPosition <- getPosition(continuousData)
            ii <- sapply(names(ic),
                         function(i) { 
                           findInterval(position[ip[[i]]],
                                        contPosition[ic[[i]]])
                         }, simplify = FALSE                         
                         )
            ## Set any 0-pointer to point to the left-most end point.
            ## Computes the pointers in terms of the full data set.
            l <- cumsum(c(0L, sapply(ic, length)[-length(ic)]))
            leftPoints <- which(unlist(ii) == 0L)
            ii <- as.integer(unlist(lapply(seq_along(l),
                                           function(i) ii[[i]] + l[[i]])),
                             use.names = FALSE)
            ii[leftPoints] <- ii[leftPoints] + 1L

            if(is.null(coarsen)) {
              ## Finding exactly duplicated position entries.
              dup <- duplicated(position)
              iii <- !(position %in% contPosition[ii]) & !dup
              ## Setting pointer information.
              pointPointer <-  ii + cumsum(iii) 
              pointPointer[leftPoints] <- pointPointer[leftPoints] - 1L
              ## Creating copy of the continuous data part if necessary.
              if(any(iii)) {
                i <- c(unlist(ic, use.names = FALSE), ii[iii])
                iSubset(continuousData) <- sort(i)
                as(pointProcess, "ContinuousProcess") <- continuousProcess(continuousData)
                ## TODO: Move pointer to environment, remove points from pointprocess.
                pointProcess@valueEnv$position[pointPointer[!dup]] <- position[!dup]
              } else {
                as(pointProcess, "ContinuousProcess") <- continuousData
              }
            } else if(coarsen == 'right') {
              ## Setting pointer information
              pointPointer <-  pmin(ii + 1L, dim(continuousData)[1])
              pointPointer[leftPoints] <- pointPointer[leftPoints] - 1L
              as(pointProcess, "ContinuousProcess") <- continuousData
            } else if(coarsen == 'left') {
              ## Setting pointer information
              pointPointer <-  ii
              as(pointProcess, "ContinuousProcess") <- continuousData
            } else {
              stop("Argument 'coarsen' can only be 'NULL', 'left' or 'right'")
            }

            pointProcessEnv$pointPointer <- pointPointer
            pointProcess@pointPointer <- -1L
            pointProcess@pointProcessEnv <- pointProcessEnv
            lockEnvironment(pointProcess@pointProcessEnv, bindings = TRUE)
            
            pointProcess@markColNames <-levels(getMarkType(pointProcess))
            pointProcess@markValueColNames <- names(markValue)
            
            lockEnvironment(pointProcess@valueEnv, bindings = TRUE)
            validObject(pointProcess)
            return(pointProcess)            
          }
          )

setMethod("markedPointProcess", c("MarkedPointProcess", "missing"),
          function(pointData, continuousData, ...) {
            continuousData <- continuousProcess(pointData)
            tmp <- data.frame(getPointId(pointData),
                              getPointPosition(pointData),
                              getMarkType(pointData))
            names(tmp) <- c(pointData@idVar,
                            pointData@positionVar,
                            "markType")
            pointData <- cbind(tmp, getMarkValue(pointData))
            if(dim(pointData)[1] > 0) {
              return(callGeneric(pointData, continuousData, ...))
            } else {
              warning("No point process data in marked point process, returning a 'ContinuousProcess'", call = FALSE)
              return(continuousData)
            }
          }
          )


setMethod("markedPointProcess", c("data.frame", "data.frame"),
          function(pointData, continuousData, ..., markVar = 'markType', coarsen = NULL) {
            callGeneric(pointData = pointData,
                        continuousData = continuousProcess(continuousData, ...),
                        markVar = markVar, coarsen = coarsen)
          }
          )

setMethod("markedPointProcess", c("data.frame", "factor.frame"),
          function(pointData, continuousData, ..., markVar = 'markType', coarsen = NULL) {
            callGeneric(pointData = pointData,
                        continuousData = continuousProcess(continuousData, ...),
                        markVar = markVar, coarsen = coarsen)
          }
          )

setMethod("markedPointProcess", c("data.frame", "vector"),
          function(pointData, continuousData, positionVar = 'time', idVar = 'id', markVar = 'markType', ...) {
            if(!(idVar %in% names(pointData))) {
            continuousData <- data.frame(continuousData)
            names(continuousData) <- positionVar
            callGeneric(pointData = pointData,
                        continuousData = continuousData,
                        positionVar = positionVar, idVar = idVar,
                        markVar = markVar, ...)
          } else {
            id <- factor(rep(levels(pointData[, idVar]),
                             each = length(continuousData))) 
            continuousData <- rep(continuousData, length(levels(pointData[, idVar])))
            continuousData <- data.frame(id, continuousData)
            names(continuousData) <- c(idVar, positionVar)
            callGeneric(pointData = pointData,
                        continuousData = continuousData,
                        positionVar = positionVar, idVar = idVar,
                        markVar = markVar, ...)
            }
          }
          )

computeGrid <- function(x, positionVar, idVar, k = 4, ...) {
  if(!(idVar %in% names(x))) {
    pRange <- range(x[, positionVar])
    fudge <- sum(pRange) / (50 * (diff(pRange) + 1))
    logN <- ceiling(log2(nrow(x)))
    y <- data.frame(seq(pRange[1] - fudge, 
                        pRange[2] + fudge, 
                        length.out = 2^(logN + k))
                    )
    names(y) <- positionVar
  }
  else {
    id <- x[, idVar]
    idLevels <- levels(x[, idVar])
    y <- list()
    for (i in seq_along(idLevels)) {
      xx <- x[id == idLevels[i], ]
      pRange <- range(xx[, positionVar])
      fudge <- sum(pRange) / (10 * (diff(pRange) + 1))
      logN <- ceiling(log2(nrow(xx)))
      kk <- 2^(logN + k)
      y[[i]] <- data.frame(seq(pRange[1] - fudge, 
                               pRange[2] + fudge, 
                               length.out = kk),
                           factor(rep(idLevels[i], kk), levels = idLevels)
                           )
      names(y[[i]]) <- c(positionVar, idVar)
    }
    y <- do.call(rbind, y)
  }
  y
}

setMethod("markedPointProcess", c("data.frame", "missing"),
          function(pointData, continuousData, positionVar = 'time', 
                   idVar = 'id', markVar = 'markType', ...) {
            if(!(positionVar %in% names(pointData)))
              stop(paste("pointData must have a column named", positionVar))
            continuousData <- computeGrid(x = pointData,
                                          positionVar = positionVar, 
                                          idVar = idVar, ...)
            callGeneric(pointData = pointData,
                        continuousData = continuousData,
                        positionVar = positionVar, idVar = idVar, markVar = markVar, ...)
          }
          )

setMethod("markedPointProcess", c("vector", "ANY"),
          function(pointData, continuousData, positionVar = 'time', idVar = 'id', markVar = 'markType',...) {
           pointData <- data.frame(pointData)
           names(pointData) <- positionVar
           callGeneric(pointData = pointData, continuousData = continuousData,
                       positionVar = positionVar, idVar = idVar, markVar = markVar, ...)         
          }
          )

## setMethod("markedPointProcess", c("vector", "vector"),
##           function (pointData, continuousData, ...) 
##           {
##             cVar <- deparse(substitute(continuousData))
##             .local <- function(pointData, continuousData, positionVar = "time", 
##                                idVar = "id", markVar = "markType", ...) {
##               pointData <- data.frame(pointData)
##               names(pointData) <- positionVar
##               continuousData <- data.frame(continuousData)
##               names(continuousData) <- cVar
##               callGeneric(pointData = pointData, continuousData, positionVar = positionVar, 
##                           idVar = idVar, markVar = markVar, ...)
##             }
##             .local(pointData, continuousData, ...)
##           }
##           )
          

setMethod("markedPointProcess", c("vector", "data.frame"),
          function(pointData, continuousData, positionVar = 'time', idVar = 'id', markVar = 'markType', ...) {
           pointData <- data.frame(pointData)
           names(pointData) <- positionVar
           callGeneric(pointData = pointData, continuousData = continuousData,
                       positionVar = positionVar, idVar = idVar, markVar = markVar, ...)         
          }
          )

setMethod("colNames", c("MarkedPointProcess", "missing"),
          function(object, ...) {
            colnames <- c(object@markColNames, object@markValueColNames)
            if(!identical(object@jPointSubset[1], -1L)) 
              colnames <- colnames[object@jPointSubset]

            colnames <- c(callNextMethod(object), colnames)
            return(colnames)
          }
          )

setMethod("colNames", c("MarkedPointProcess", "character"),
          function(object, type, ...) {
            colnames <- callGeneric(object = object, ...)
            if(type == "mark") {
              colnames <- colnames[colnames %in% object@markColNames]
            } else if(type == "markValue") {
              colnames <- colnames[colnames %in% object@markValueColNames]             
            } else {
              colnames <- callGeneric(as(object, "ContinuousProcess"), type = type, ...) 
            }
            return(colnames)
          }
          )

setMethod("dim", "MarkedPointProcess",
          function(x) {
            d <- callNextMethod(x)
          
            if(identical(x@jPointSubset[1], -1L)) {
              d2 <- length(x@markColNames) + length(x@markValueColNames) 
            } else {
              d2 <- length(x@jPointSubset)
            }
            d[2] <- d[2] + d2

            return(d)
          }
          )


setMethod("integrator", "MarkedPointProcess",
          function(object, f = 1, jumpVar = '', result = 'JumpProcess', ...)
          {

            id <- getPointId(object)
            jump <- NULL
            
            ## The counting process
            process <- rep(0,length(getId(object)))
            process[getPointPointer(object)] <- 1
            process <- tapply(process, getId(object), cumsum)
            
            if(is.function(f))
              f <- f(getPointPosition(object), ...)

            if(length(f) == 1 && f == 1 && jumpVar %in% colnames(getMarkValue(object))) {
              jump <- getMarkValue(object)[ , jumpVar]
            } else if(length(f) > 1 || any(f != 1)) {
              if(jumpVar %in% colnames(getMarkValue(object))) {
                jump <- f*getMarkValue(object)[ , jumpVar]
              } else {
                jump <- f*rep(1, length(id))
              }
            }

            if(is.null(jump)){
              jump <- rep(1, length(id))
              process <- unlist(process, use.names = FALSE)
            } else {
              compound <- tapply(jump, id, function(s) cumsum(c(0,s)))
              process <- unlist(lapply(levels(id), function(i) compound[[i]][process[[i]]+1]))
            }

            if(result == 'numeric') 
              return(process)

            CP <- continuousProcess(c(as(object[ , FALSE], "list"),
                                      list(integrated = process)),
                                    idVar = object@idVar,
                                    positionVar = object@positionVar,
                                    unitData = getUnitData(object))
            MP <- data.frame(id, getPointPosition(object), "integrated", jump)
            names(MP) <- c(object@idVar, object@positionVar, "markType", "jump")
                                        
            return(jumpProcess(MP, CP, idVar = object@idVar,
                                      positionVar = object@positionVar))
          }
          )          
                 
setMethod("iPointSubset", "MarkedPointProcess",
          function(object) {
            if(identical(object@iPointSubset[1], -1L)) {
              i <- seq_along(object@pointProcessEnv$id)
            } else {
              i <- object@iPointSubset
            }
            return(i)
          }
          )

setMethod("jPointSubset", "MarkedPointProcess",
          function(object) {
            j <- seq_len(length(object@markColNames) + length(object@markValueColNames))
            if(!isTRUE(object@jPointSubset == -1L))
              j <- j[object@jPointSubset]
             
            return(j)
          }
          )

setReplaceMethod("iPointSubset", c("MarkedPointProcess", "ANY"),
                 function(object, value) {
                   value <- value[!is.na(value)]
                   if(length(value) == length(object@pointProcessEnv$id) &&
                      identical(value, seq_along(object@pointProcessEnv$id))) {
                     object@iPointSubset <- -1L
                   } else {
                     object@iPointSubset <- value
                   }
                   return(object)
                 }
                 )

setReplaceMethod("jPointSubset", c("MarkedPointProcess", "ANY"),
                 function(object, value) {
                   value <- value[!is.na(value)]
                   d2 <- length(object@markColNames) + length(object@markValueColNames)
                   if(length(value) == d2 && identical(value, seq_len(d2))) {
                     object@jPointSubset <- -1L
                   } else {              
                     object@jPointSubset <- value
                   }
                   return(object)
                 }
                 )

setMethod("getPointId", "MarkedPointProcess",
          function(object, drop = TRUE, ...){
            if(isTRUE(object@iPointSubset == -1L)) {
              value <- object@pointProcessEnv$id
            } else {
              value <- object@pointProcessEnv$id[iPointSubset(object), drop = TRUE]
              
              if(!drop)
                value <- factor(value, levels = levels(getId(object)))
            }
            return(value)
          }
          )

setMethod("getPointPosition", "MarkedPointProcess",
          function(object, ...){
            return(getPosition(object)[getPointPointer(object, ...)])
          }
          )

setMethod("getPointTime", "MarkedPointProcess",
          function(object, ...) {
            getPointPosition(object, ...)
          }
          )

setMethod("getMarkType", "MarkedPointProcess",
          function(object, drop = TRUE, ...){
            if(isTRUE(object@iPointSubset == -1L)) {
              value <- object@pointProcessEnv$markType
            } else {
              value <- object@pointProcessEnv$markType[iPointSubset(object), drop = TRUE]
              if(!drop)
                value <- factor(value, levels = colNames(object, "mark"))
            }
            
            return(value)
          }
          )

setMethod("getMarkValue", "MarkedPointProcess",
          function(object, ...){
            j <- colNames(object, "markValue")
            if(length(j) > 0) {
              value <- as.data.frame(getColumns(object, j, drop = FALSE))
            } else {
              value <- data.frame()[seq_along(getPointId(object)),]
            }
              
            return(value)
            }
          )

setMethod("getPlotData", "MarkedPointProcess",
          function(object, y = '@mark', nPoints = 200, allUnitData = FALSE,
                   allMarkValueData = isTRUE(y %in% names(getMarkValue(object))), ...){
            if(length(getMarkType(object)) == 0) {

              plotData <- callGeneric(object = as(object, "ContinuousProcess"),
                                      nPoints = nPoints,
                                      allUnitData = allUnitData, ...)
              
            } else {
              
              pointPlotData <- data.frame(id = getPointId(object),
                                          position = getPointPosition(object),
                                          variable = factor(getMarkType(object)))

              names(pointPlotData)[1] <- object@idVar
              
              plotData <- callGeneric(object = as(object, "ContinuousProcess"),
                                      nPoints = nPoints,
                                      allUnitData = allUnitData,
                                      selectPoints = getPointPointer(object), ...)
              
              if(allMarkValueData)
                pointPlotData <- cbind(pointPlotData, getMarkValue(object))

              if(isTRUE(allUnitData))
                pointPlotData <- cbind(pointPlotData, getUnitData(object)[as.numeric(getPointId(object)), , drop = FALSE])

              pointPlotData$type <- as.factor('Track')

              if(is.numeric(y)) {
                pointPlotData$value <- y
                plotData@breaks <- c(breaks, y)
                plotData@labels <- c(labels, as.character(y))
              } else {
                pointPlotData$value <- as.factor(y)
              }
              
              if(y == "@mark") 
                pointPlotData$value <- pointPlotData$variable
              if(y == object@idVar) 
                pointPlotData$value <- pointPlotData$id
              if(isTRUE(y %in% names(getMarkValue(object)))) 
                pointPlotData$value <-  getColumns(object, y)
              if(y == "@top") {
                pointPlotData$value <- pointPlotData$type
                plotData@position <- "top"
              }
              if(y == "@bottom") {
                pointPlotData$value <- pointPlotData$type
                plotData@position <- "bottom"
              }
              if("value" %in% names(plotData@continuousPlotData)) {
                if(y == "@variable") {
                  pointPlotData$value <- numeric(dim(pointPlotData)[1])
                  for(variable in levels(pointPlotData$variable)) {
                    pointPlotData$value[pointPlotData$variable == variable] <-
                      getColumns(object[getPointPointer(object, variable),
                                        colNames(as(object, "ContinuousProcess"))],
                                        variable)
                  }
                } else if(isTRUE(y %in% levels(plotData@continuousPlotData$variable))) {
                    pointPlotData$value <- getColumns(object, y)[getPointPointer(object)]
                  }
                
              }
         
              plotData@pointPlotData <- pointPlotData             
            }
            
            return(plotData)
          }  
          )

setMethod("plot", c("MarkedPointProcess", "missing"),
          function(x, y, ...) callGeneric(x = x, y = '@mark', ...)
          )

setMethod("plot", c("MarkedPointProcess", "character"),
          function(x, y, nPoints = 200, ...){
            plotData <- getPlotData(object = x, y = y, nPoints = nPoints, ...)
            return(plot(plotData, ...))
          }
          )

setMethod("subset", "MarkedPointProcess",
          function(x, ... , pointSubset) {
            y <- callNextMethod()
            if (missing(pointSubset)) 
              r <- TRUE  
            else {
              e <- substitute(pointSubset)
              variables <- all.vars(e)
              tmpEnv <- new.env(parent = .GlobalEnv)

              for(v in variables) {
                assign(v, getColumns(x, v), envir = tmpEnv) 
              }
             
              r <- eval(e, tmpEnv, parent.frame())
              if (!is.logical(r)) 
                stop("'pointSubset' must evaluate to logical")
              r <- r & !is.na(r) 
            }
            if(!all(r)) {
              iPointSubset(y) <- iPointSubset(y)[r]
              setPointPointer(y) <- getPointPointer(y)[r]
            } 
            return(y)
          }
          )


setMethod("getColumns", c("MarkedPointProcess", "character"),
          function(object, j, drop = TRUE) {
            checkColumns <- j %in% colNames(object)
            if(!all(checkColumns)) 
              stop(paste(c("No column '", j[!checkColumns][1], "' in the object."),
                         collapse = ""), call. = FALSE)
            
            contj <- j %in% colNames(as(object, "ContinuousProcess"))
            
            if(drop && length(j) == 1) {
              if(contj) {
                column <- callNextMethod(object, j, drop = TRUE)
              } else {
                if(j %in% object@markColNames) {
                  column <- getPointPosition(object, j)
                } else {
                  column <- get(paste("var_", j, sep = ""),
                                envir = object@pointProcessEnv)
                }
                if(!identical(object@iPointSubset[1], -1L))
                  column <- column[object@iPointSubset]
              }
              } else {
                column <- callNextMethod(object, j[contj], drop = drop)
                for(jj in j[!contj]) {
                  if(jj %in% object@markColNames) {
                    column[[jj]] <- getPointPosition(object, jj)
                  } else {
                    column[[jj]] <- get(paste("var_", jj, sep = ""),
                                        envir = object@pointProcessEnv)
                  }
                  if(!identical(object@iPointSubset[1], -1L))
                    column[[jj]] <- column[[jj]][object@iPointSubset]
                }
              }
                        
            return(column)
          }
          )

setMethod("[", c(x = "MarkedPointProcess", i = "integer", j = "missing"),
          function(x, i, j, ... , drop = FALSE) {
            orgPoint <- iSubset(x)[getPointPointer(x)]
            as(x, "ContinuousProcess") <- callGeneric(as(x, "ContinuousProcess"), i, , )
            newPointPointer <- match(orgPoint, iSubset(x))
            notNA <- which(!is.na(newPointPointer))
            iPointSubset(x) <- iPointSubset(x)[notNA]
            setPointPointer(x) <- newPointPointer[notNA]
            
            if(drop) {
              marks <- colNames(x, "mark")
              dropCol <- colNames(x) %in% marks[!(marks %in% levels(getMarkType(x)))]
              if(any(dropCol)) 
                x <- x[ , !dropCol, drop = TRUE]
            }
              
            return(x)
          }
          )

setMethod("[", c(x = "MarkedPointProcess", i = "missing", j = "character"),
          function(x, i, j, ... , drop = FALSE) {
            if(drop && length(j) == 1) {
              x <- getColumns(x, j)
            } else {
              as(x, "ContinuousProcess") <- callGeneric(as(x, "ContinuousProcess"), , j)             
              i <- getMarkType(x) %in% j
              if(!any(i)) {
                if(drop) {
                  x <- as(x, "ContinuousProcess")
                } else {
                  iPointSubset(x) <- integer()
                  jPointSubset(x) <- which(c(x@markColNames, x@markValueColNames) %in% j)
                }
              } else {
                iPointSubset(x) <- iPointSubset(x)[i]
                setPointPointer(x) <- getPointPointer(x)[i]
                jPointSubset(x) <- which(c(x@markColNames, x@markValueColNames) %in% j)
              }
            }
            
            return(x)
          }
          )

setMethod("getPointPointer", c("MarkedPointProcess", "missing"),
          function(object, mark, ...) {
            if(identical(object@pointPointer[1], -1L)) {
              value <- object@pointProcessEnv$pointPointer
            } else {
              value <- object@pointPointer
            }
            return(value)
          }
          )

setMethod("getPointPointer", c("MarkedPointProcess", "character"),
          function(object, mark, ...) {
            getPointPointer(object)[getMarkType(object) %in% mark]
          }
          )

setMethod("object.size", "ProcessData", 
          function(x) {
            size <- sum(sapply(ls(x@vpointProcessEnv),
                               function(name) object.size(get(name, envir = x@vpointProcessEnv))))
            size <- size + object.size(as(x, "ContinuousProcess"))
            return(structure(size, class = "object_size"))
          }
          )

setReplaceMethod("setPointPointer", c("MarkedPointProcess", "ANY"),
          function(object, value) {
            if(identical(object@iSubset[1], -1L) &&
               identical(object@iPointSubset[1], -1L)) {
              object@pointPointer <- -1L
            } else {
              object@pointPointer <- value
            }
            
            return(object)
          }
          )

setMethod("summarizeData", "MarkedPointProcess",
          function(object, maxsum = 100, ....) {
            summaryById <- callNextMethod()
            if(length(colNames(object, "mark")) > 0) {
              id <- getPointId(object, drop = FALSE)
              splitEntries <- split(seq_along(id), id)
              names(splitEntries) <- NULL
              pointSummary <- do.call("rbind", lapply(splitEntries, function(e) table(getMarkType(object, drop = FALSE)[e])))
              colnames(pointSummary) <- paste("#", colnames(pointSummary), sep = "")
              if (ncol(pointSummary) > maxsum) {
                drop <- maxsum:ncol(pointSummary)
                tt <- colSums(pointSummary)
                o <- order(tt, decreasing = TRUE)
                pointSummary <- cbind(pointSummary[, o[-drop], drop = FALSE],
                                      data.frame(Other = rowSums(pointSummary[, o[drop], drop = FALSE]))
                                      )
              }
              summaryById <- cbind(summaryById, pointSummary)
              sumVal <- list()
              for(j in colNames(object, "markValue")) {
                column <- getColumns(object, j)
                if(is.numeric(column)) {
                  sumVal[[paste("mean(", j ,")", sep ="")]] <- 
                    sapply(splitEntries, function(e) mean(column[e]))
                } else {
                  sumVal[[paste("most freq. ", j , sep ="")]] <-
                  sapply(splitEntries, function(e)  names(which.max(table(column[e]))))
                }
              }
              if(length(sumVal) > 0) 
                summaryById <- cbind(summaryById,
                                     as.data.frame(sumVal, optional = TRUE))
              
            }            
            return(summaryById)            
          }
          )

setMethod("unsubset", "MarkedPointProcess",
          function(x, ...) {
            as(x, "ContinuousProcess") <- callGeneric(as(x, "ContinuousProcess"))
            
            x@iPointSubset <- -1L
            x@jPointSubset <- -1L
            x@pointPointer <- -1L
            return(x)
          }
          )
