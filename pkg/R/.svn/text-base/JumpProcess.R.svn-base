setMethod("jumpProcess", "MarkedPointProcess",
           function(object, ...) {
             .Object <- new("JumpProcess")
             as(.Object, "MarkedPointProcess") <- object
             .Object@jumpVar <- "jump"
             return(.Object)
           }
          )

setMethod("jumpProcess", "data.frame",
          function(object, continuousData, ...) {
            callGeneric(markedPointProcess(object, continuousData), ...)
          }
          )

setMethod("plot", c("JumpProcess", "missing"),
          function(x, y, ...) {
            callGeneric(x = as(x, "MarkedPointProcess"), y = "@variable", ...)
          }
          )


## TODO: Reimplement the jump process plots with the new plotting framework.
