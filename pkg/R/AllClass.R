setOldClass(c("factor.frame", "data.frame"))

setClass("ProcessData",
         representation(
                        ## metaData is a list, in general unrestricted.
                        ## The slot can contain information
                        ## on the experiment or procedure by which the
                        ## data is obtained.
                        metaData = "list",

                        ## Column names.
                        unitColNames = "character",

                        ## The valueEnv is assumed to have a data
                        ## frame, unitData, that holds general
                        ## information for each unit for which we have
                        ## observations. Each row name is the unit id. 
                        valueEnv = "environment",

                        ## Name of the id variable.
                        idVar = "character",
                        
                        ## Two vectors indexing a subset of the full data set
                        ## in the valueEnv environment.
                        iUnitSubset = "integer",
                        jUnitSubset = "integer"
                        ),
         validity = function(object) {
           
           return(TRUE)
         }
         ## validity = function(object) {
         ##   if(!("id" %in% names(object@unitData)))
         ##     stop("The 'id' column for the 'unitData' data frame in 'ProcessData' is not defined")
         ##   if(class(object@unitData$id) !=  "factor")
         ##     stop("The 'id' column for the 'unitData' data frame in 'ProcessData' must be a 'factor'")
           
         ##   return(TRUE)
         ## }
         )

setClass("ContinuousProcess",
         representation(

                        ## Additional column names.
                        factorColNames = "character",
                        numericColNames = "character",

                        ## Two vectors indexing a subset of the full data set
                        ## in the valueEnv environment.
                        iSubset = "integer",
                        jSubset = "integer",
                        
                        ## Name of the position variable.
                        positionVar = "character",
                        equiDistance = "numeric"

                        ## The 'valueEnv' will also contain the following 
                        ## components of the data:
                        ## position = "numeric",
                        ## and any number of numeric and non-numeric vectors
                        ),
         contains = "ProcessData",
         validity = function(object){
           ## Checks that the environment contains the correct components.
           if(!is.factor(object@valueEnv$id))
             stop(paste("No", object@idVar, "or", object@idVar, "is not a factor."))
           if(!is.numeric(object@valueEnv$position))
             stop(paste("No", object@positionVar, "or", object@positionVar, "is not a numeric."))
 
           id <- getId(object)
           position <- getPosition(object)
           idLevels <- split(seq_along(id), id)
           if(is.unsorted(id) || any(sapply(length(idLevels),
                                            function(i) is.unsorted(position[idLevels[[i]]]))))
             stop(paste(object@positionVar ,"not sorted within", object@idVar))
           
           ## Checks that the components in the environment have the correct
           ## format. 
           if((length(object@valueEnv$id) != length(object@valueEnv$position)))
             stop("Mismatch in the size of slots for 'ContinuousProcess'.")

           ## TODO: Check length of all other vectors.
          
           return(TRUE)
         }
         )


setClass("MarkedPointProcess",
         representation(
                        ## Column names.
                        markColNames = "character",
                        markValueColNames = "character",
                        
                        iPointSubset = "integer",
                        jPointSubset = "integer",

                        ## The pointer for subsetted objects to the
                        ## point positions.
                        pointPointer = "integer",
                        
                        ## The 'pointProcessEnv' environment contains
                        ## the following four components:
                        ## id = "factor",
                        ## markType = "factor",
                        ## pointPointer = "integer",
                        ## markValue = "data.frame"
                        pointProcessEnv = "environment"
                        ),
         contains = "ContinuousProcess",
         validity = function(object) {
           len <- length(object@pointProcessEnv$id)
           if(len != length(object@pointProcessEnv$markType))
             stop("Sizes of the slots 'id' and 'markType' do not match.")
          
           if(any(levels(getPointId(object)) != rownames(getUnitData(object)))) 
             stop(paste("The point process levels of", object@idVar, "and the row names of unitData are not of equel length or in the same order."))
           if(any(unlist(lapply(split(getPointPosition(object), getPointId(object)), is.unsorted))))
             stop(paste(object@positionVar,"for the point process data not sorted within", object@idVar))
           
           return(TRUE)
         }
         )

setClass("JumpProcess",
         representation(jumpVar = "character"),
         contains = "MarkedPointProcess",
         validity = function(object) {
           return(TRUE)
         }
         )

## TODO: Validity checks, constructors etc. 

setClass("ProcessPlotData",
         representation(continuousPlotData = "data.frame",
                        factorPlotData = "data.frame",
                        pointPlotData = "data.frame",
                        position = "character",
                        limits = "numeric",
                        breaks = "numeric",
                        labels = "character",
                        idVar = "character",
                        positionVar = "character")
         )
                        
