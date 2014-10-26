setMethod("process", "data.frame",
          function(unitData, idVar = "id", ...) {
            object <- new("ProcessData")
            setUnitData(object) <- unitData
            object@idVar <- idVar
            validObject(object)
            return(object)
          }
          )

setMethod("dim", "ProcessData",
          function(x) {
            if(identical(x@iUnitSubset[1], -1L)) {
              d1 <- dim(x@valueEnv$unitData)[1]
            } else {
              d1 <- length(x@iUnitSubset)
            }

            if(identical(x@jUnitSubset[1], -1L)) {
              d2 <- length(x@unitColNames)
            } else {
              d2 <- length(x@jUnitSubset)
            }

            return(c(d1,d2))
          }
          )
                     
setMethod("colNames", c("ProcessData", "missing"),
          function(object, type, ...) {
            colnames <- object@unitColNames
            if(!identical(object@jUnitSubset[1], -1L)) 
              colnames <- colnames[object@jUnitSubset]
                                
            return(colnames)
          }
          )

setMethod("colNames", c("ProcessData", "character"),
          function(object, type, ...) {
            if(type == "unit") {
              colnames <- callGeneric(object = object, ...)
            } else {
              stop("Argument type", type, "not valid")
            }
            return(colnames)
          }
          )

setMethod("getUnitData", "ProcessData",
          function(object, ...) {
            if(identical(object@iUnitSubset[1], -1L)) {
               if(identical(object@jUnitSubset[1], -1L)) {
                 unitData <- object@valueEnv$unitData
               } else {
                 unitData <- object@valueEnv$unitData[ , object@jUnitSubset, drop = FALSE]
               }
             } else {
               if(identical(object@jUnitSubset[1], -1L)) {
                 unitData <- object@valueEnv$unitData[object@iUnitSubset, , drop = FALSE]
               } else {
                 unitData <- object@valueEnv$unitData[object@iUnitSubset, object@jUnitSubset, drop = FALSE]
               }
             }
                 
            return(unitData)
          }
          )

setMethod("getId", "ProcessData",
          function(object, ...) {
            id <- row.names(object@valueEnv$unitData)
            if(!identical(object@iUnitSubset[1], -1L))
              id <- id[object@iUnitSubset]
            
            return(id)
          }
          )

setMethod("iUnitSubset", "ProcessData",
          function(object) {
            if(identical(object@iUnitSubset[1], -1L)) {
              i <- seq_len(dim(object@valueEnv$unitData)[1])
            } else {             
              i <- object@iUnitSubset
            }
            
            return(i)
          }
          )

setMethod("jUnitSubset", "ProcessData",
          function(object) {
            if(identical(object@jUnitSubset[1], -1L)) {
              j <- seq_along(object@unitColNames)
            } else {
              j <- object@jUnitSubset
            }
             
            return(j)
          }
          )

setReplaceMethod("iUnitSubset", c(object = "ProcessData", value = "numeric"),
                 function(object, value) {
                   value <- value[!is.na(value)]
                   if(length(value) == dim(object@valueEnv$unitData)[1] &&
                      identical(value, seq_len(dim(object@valueEnv$unitData)[1]))) {
                     object@iUnitSubset <- -1L
                   } else {
                     object@iUnitSubset <- value
                   }
                   
                   return(object)
                 }
                 )


setReplaceMethod("jUnitSubset", c(object = "ProcessData", value = "numeric"),
                 function(object, value) {
                   value <- value[!is.na(value)]
                   if(length(value) == length(object@unitColNames) &&
                      identical(value, seq_along(object@unitColNames))) {
                     object@jUnitSubset <- -1L
                   } else {              
                     object@jUnitSubset <- value
                   }
                   
                   return(object)
                 }
                 )

setMethod("[", c(x = "ProcessData", i = "integer", j = "missing"),
          function(x, i, j, ... , drop = FALSE) {
            iUnitSubset(x) <- iUnitSubset(x)[i]
            return(x)
          }
          )

setMethod("[", c(x = "ProcessData", i = "numeric", j = "missing"),
          function(x, i, j, ... , drop = FALSE) {
            i <- as.integer(i)
            x <- callGeneric(x, i, , drop = drop)
            return(x)
          }
          )

setMethod("[", c(x = "ProcessData", i = "logical", j = "missing"),
          function(x, i, j, ... , drop = FALSE) {
            if(!isTRUE(i)) { 
              d <- dim(x)[1]
              if(length(i) == d) {
                i <- which(i)
              } else {
                i <- seq_len(d)[i]
              }
              x <- callGeneric(x, i, , drop = drop)
            }
            return(x)
          }
          )

setMethod("[", c(x = "ProcessData", i = "character", j = "missing"),
          function(x, i, j, ... , drop = FALSE) {
            i <- which(getId(x) %in% i)
            x <- callGeneric(x, i, , drop = drop)
            return(x)
          }
          )

setMethod("[", c(x = "ProcessData", i = "missing", j = "numeric"),
          function(x, i, j, ... , drop = FALSE) {
            j <- colNames(x)[j]
            x <- callGeneric(x, , j, drop = drop)
            return(x)
          }
          )

setMethod("[", c(x = "ProcessData", i = "missing", j = "logical"),
          function(x, i, j, ... , drop = FALSE) {
            if(!isTRUE(j)) {
              j <- colNames(x)[j]
              x <- callGeneric(x, , j, drop = drop)
            }
            return(x)
          }
          )

setMethod("[", c(x = "ProcessData", i = "missing", j = "character"),
          function(x, i, j, ... , drop = FALSE) {
            if(drop && length(j) == 1) {
              return(getColumns(x, j))
            } else {
              jUnitSubset(x) <- jUnitSubset(x)[x@unitColNames %in% j]
            }
            
            return(x)
          }
          )

setMethod("[", "ProcessData",
          function(x, i, j, ... , drop = FALSE) {

            if(!(class(i) %in% c("logical", "numeric", "integer", "character")))
              stop("i must be a 'logical', a 'numeric' or a 'character' vector.")
            if(!(class(j) %in% c("logical", "numeric", "integer", "character")))
              stop("j needs to be a 'logical', a 'numeric' or a 'character' vector.")
            
            x <- callGeneric(x, , j, drop = drop)  ## Order matters. Subsetting of rows
            x <- callGeneric(x, i, , drop = drop)  ## with drop = TRUE can remove cols.
            return(x)
          }
          )

setMethod("[", c("ProcessData", "missing", "missing", "missing"),
          function(x, i, j, ... , drop) {
            return(x)
          }
          )

setMethod("getColumns", c("ProcessData", "numeric"),
          function(object, j, drop = TRUE) {
            callGeneric(object = object, colNames(object)[j], drop = drop)
          }
          )

setMethod("getColumns", c("ProcessData", "logical"),
          function(object, j, drop = TRUE) {
            if(isTRUE(j)){
              return(callGeneric(object = object, colNames(object), drop = drop))
            } else {
              return(callGeneric(object = object, colNames(object)[j], drop = drop))
            }
          }
          )

setMethod("getColumns", c("ProcessData", "character"),
          function(object, j, drop = TRUE) {
            if(length(j) == 1 && drop) {
              columns <- getUnitData(object)[ , j, drop = TRUE]
            } else {
              columns <- as.list(getUnitData(object)[ , j, drop = FALSE])
            }
              
            return(columns)
          }
          )

setMethod("$", "ProcessData",
          function(x, name) {
           getColumns(x, name) 
          }
          )


setMethod("object.size", "ProcessData", 
          function(x) {
            size <- sum(sapply(ls(x@valueEnv),
                               function(name) object.size(get(name, envir = x@valueEnv))))
            size <- size + object.size(unclass(x))
            return(structure(size, class = "object_size"))
          }
          )

setMethod("subset", "ProcessData",
          function(x, subset, select, ...) {
            if (missing(subset)) 
              r <- TRUE
            else {
              e <- substitute(subset)
              variables <- all.vars(e)
              
              tmpEnv <- new.env(parent = .GlobalEnv)
              if(x@idVar %in% variables)
                  assign(x@idVar, getId(x), envir = tmpEnv)
              
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
              nl <- as.list(seq(1L, dim(x)[2]))
              names(nl) <- colNames(x)
              vars <- eval(substitute(select), nl, parent.frame())
            }
            return(x[r, vars])
          }
          )

          
setReplaceMethod("setUnitData", c("ProcessData", "data.frame"),
          function(object, value) {
            object@valueEnv$unitData <- value
            object@unitColNames <- names(value)
            object@iUnitSubset <- -1L
            object@jUnitSubset <- -1L
            return(object)
          }
          )

setMethod("show", "ProcessData",
          function(object) {
            print(getUnitData(object))
            return(invisible(object))
          }
          )

setMethod("str", "ProcessData",
          function(object, ...) {
            slotnames <- slotNames(object)
            cl <- class(object)
            cat("Formal class", " '", paste(cl, collapse = "', '"), 
                "' [package \"", attr(cl, "package"), "\"] with ", 
                length(slotnames), " slots\n", sep = "")            
            envirSlots <- sapply(slotnames, function(name) class(slot(object, name)) == "environment")

            niceSlotnames <- format(slotnames,
                                    width = max(nchar(slotnames[!envirSlots], type = "w")),
                                    justify = "left")
            for(i in which(!envirSlots)) {
              cat("  ..@", niceSlotnames[i] , ":")  
              str(slot(object, slotnames[i]))  
            }
            for(name in slotnames[envirSlots]) {
              varNames <- ls(slot(object, name))
              niceNames <- format(varNames,
                                  width = max(nchar(varNames, type = "w")),
                                  justify = "left")
              cat("  ..@", name, "[environment] with", length(varNames), "variables\n")
              for(i in seq_along(varNames)) {
                cat("    ..$", niceNames[i] , ":")  
                str(get(varNames[i], envir = slot(object, name)),
                    give.attr = FALSE, 
                    indent.str =  paste("      .."))
              }
            }
            cat("\n")
          }
          )
            
setMethod("unsubset", "ProcessData",
          function(x, ...) {
            x@iUnitSubset <- -1L
            x@jUnitSubset <- -1L
            return(x)
          }
          )            
    
