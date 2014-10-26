setMethod("anticipating", "PointProcess",
          function(model, ...) {
            antipmodels <- c("Gibbs")
            if(family(model@family) %in% antipmodels)
              return(TRUE)

            return(FALSE)
          }
          )

setMethod("computeMinusLogLikelihood", "PointProcess",
          function(model, coefficients = NULL, ...) {
            if(isTRUE(response(model) == ""))
              stop("No response variable specified.")
            
            eta <- computeLinearPredictor(model, coefficients, ...)
            pointer <- getPointPointer(processData(model), response(model))
            if(model@family@link == "log"){
              mll <- sum(exp(eta) * model@delta) -
                sum(eta[pointer]) 
            } else {
              phieta <- model@family@phi(eta)
              mll <- sum(phieta * model@delta) -
                sum(safeLog(phieta[pointer])) 
            }
            
            return(mll)
          }
          )

setMethod("computeQuadraticContrast", "PointProcess",
          function(model, coefficients = NULL, ...) {
            if(isTRUE(response(model) == ""))
              stop("No response variable specified.")

            eta <- computeLinearPredictor(model, coefficients, ...)
            pointer <- getPointPointer(processData(model), response(model))

            phieta <- model@family@phi(eta)
            sum(phieta^2 * model@delta) / 2 -
              sum(phieta[pointer]) 
          }
          )          

setMethod("computeLoss", "PointProcess",
          function(model, loss = 'default', ...) {
            if (loss == 'default')
              loss <- model@loss
            switch(loss,
                   likelihood = {
                     loss <- computeMinusLogLikelihood(model)
                   },
                   quadratic = {
                     loss <- computeQuadraticContrast(model)
                   },
                   stop(paste("No loss method", loss))
                   )
            return(loss)
          }                  
          )

setMethod("family", "PointProcess",
          function(object,...) {
            return(object@family)
          }
          )

setMethod("formula", "PointProcess",
          function(x, ...){
            return(x@formula)
          }
          )


setMethod("response", "PointProcess",
          function(model, ...){
            return(model@response)
          }
          )

setReplaceMethod("formula", c(model = "PointProcess", value = "formula"),
                 function(model, value){
                   if(attr(terms(value), "response") != 0) {
                     ## TODO: Is the response always in position 2 in this list/call?
                     response <- attr(terms(value), "variables")[[2]]
                     model@response <- all.vars(response)                  
                   } else {
                     model@response <- ""
                   }
                   model@formula <- value
                   return(model)
                 }
                 )

setMethod("processData", "PointProcess",
          function(model, ...){
            return(model@processData)
          }
          )

setReplaceMethod("processData", c(model = "PointProcess", value = "MarkedPointProcess"),
          function(model, value){
            model@processData <- value
            return(model)
          }
          )




