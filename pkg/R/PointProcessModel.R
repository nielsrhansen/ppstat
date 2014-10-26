pointProcessModel <- function(
  formula,
  data,
  family,
  support = 1,
  N = 200,
  Delta,
  lambda,
  coefficients,
  modelMatrix = TRUE,
  fit = modelMatrix,
  varMethod = 'Fisher',
  basisEnv,
  selfStart = TRUE,
  ...) {
  
  if (length(support) == 1)
    support <- c(0, max(support[1], 0))
  
  if (missing(Delta))
    Delta <- (support[2] - support[1])/N
  
  basisPoints <- sort(unique(c(0, seq(support[1], support[2], Delta))))
  
  delta <- as.numeric(unlist(tapply(getPosition(data),
                                    getId(data),
                                    function(x) c(0, diff(x))),
                             use.names=FALSE)
  )
  
  model <- new("PointProcessModel",
               delta = delta,
               family = family,
               formula = .~.,
               call = match.call(),
               processData = data,
               support = support,
               basisPoints = basisPoints,
               Delta = Delta,
               varMethod = varMethod,
               responseMatrix = Matrix(ncol = 0, nrow = 0),
               loss = 'likelihood')
  
  ## Parsing the formula: Either the model is multivariate with the
  ## formula a list of formulas, one for each model, or the response
  ## is a vector -- assuming that the right hand side of each model is 
  ## the same as a starting point. Alternatively, the formula is just a
  ## single formula. 
  
  if (is.list(formula)) {
    ## Extract all terms used in any of the formulae to create a
    ## super formula.
    superFormula <- reformulate(unlist(lapply(formula, function(f) attr(terms(f), "term.labels"))))
    formula(model) <- superFormula
    response <- NULL
  } else {
    response <- attr(terms(formula), "variables")[[2]]
    if (length(response) > 2 && response[[1]] == as.symbol("c")) {
      response <- response[seq.int(2, length(response))]
      formula(model) <- update(formula, as.formula(paste(deparse(response[[1]]), "~ .")))
      call <- as.list(model@call)
      call$formula <- formula(model)
      model@call <- as.call(call)
    } else {
      formula(model) <- formula
      response <- NULL
    }
  }
  
  if (!anticipating(model)) {
    model@basisPoints <- model@basisPoints[model@basisPoints >= 0]
    model@support[1] <- max(0, model@support[1])
  }
  
  if (missing(basisEnv))  {
    setBasis(model) <- list()
    model <- computeBasis(model)
  } else {
    setBasis(model) <- basisEnv$basis
  }
  
  if (modelMatrix) {
    model <- computeModelMatrix(model)
  } else {
    model <- updateModelMatrix(model,
                               Matrix(ncol = 0, nrow = 0),
                               assign = numeric(),
                               form = formula(~0)
    )
  }
  
  if (missing(coefficients)) {
    coefficients <- .Machine$double.eps
    ## selfStart <- TRUE
  } else {
    selfStart <- FALSE
  }
  
  coefficients(model) <- coefficients
  
  if (!missing(lambda)) 
    penalty(model) <- lambda
  
  if (!is.null(response)) {
    models <- list()
    models[[1]] <- model
    for(i in seq.int(2, length(response))) {
      models[[i]] <- update(model, as.formula(paste(deparse(response[[i]]), "~ .")), fit = FALSE, ...)
    }
    model <- new("MultivariatePointProcess")
    setModels(model) <- models
  }
  
  if (is.list(formula)) {
    models <- list()
    for(i in seq_along(formula)) {
      models[[i]] <- update(model, formula[[i]], fit = FALSE, ...)
    }
    model <- new("MultivariatePointProcess")
    setModels(model) <- models 
  }
  
  if (fit) {
    model <- ppmFit(model, selfStart = selfStart, ...)
  } else {
    ## Initializing the variance matrix without computing it.
    model <- computeVar(model, method = "none")  
    ##  TODO: correct to work with multivariate models
    ##    model@optimResult <- list(value = computeMinusLogLikelihood(model),
    ##                              counts = c(0, 0),
    ##                              convergence = NA)
  }
  
  return(model)
}

## TODO: Implement the use of interaction terms.

setMethod("computeBasis", "PointProcessModel",
          function(model, ...) {
            
            ## Basis evaluations are computed if not
            ## already computed and available in
            ## 'model@basisEnv$basis'.
            
            basisComputed <- exists("basisComputed", model@basisEnv) &&
              get("basisComputed", model@basisEnv)
            
            if (!basisComputed) {
              processData <- processData(model)
              DcontinuousVar <- paste(colnames(getValue(processData)), ".d", sep="")
              DpositionVar <- paste(processData@positionVar, ".d", sep="")
              markLevels <- levels(getMarkType(processData, drop = FALSE))
              x <- as.data.frame(model@basisPoints)
              
              ## The formula object is extracted and decomposed into terms.
              ## Each term label (in 'termLabels') is processed below,
              ## and the corresponding basis functions are computed.
              
              mt <- delete.response(terms(formula(model)))
              termLabels <- attr(mt, "term.labels")
              filterTerms <- numeric()
              for(i in seq_along(termLabels)) {
                term <- termLabels[i]
                variable <- all.vars(mt[i])
                
                if (all(variable %in% c(markLevels, DcontinuousVar, DpositionVar))) {                  
                  if (length(variable) > 1){
                    stop(paste("Basis computations with two or more variables in '", term,
                               "' is currently not supported.", sep=""))
                  } else {
                    filterTerms <- c(filterTerms, i)
                    form <- update(mt[i], ~ . -1)
                    colnames(x) <- variable
                    mm <- model.matrix(form, x)
                    ### The basis functions are renormalized to have L2-norm 1 numerically.
                    cs <- sqrt(apply(mm^2, 2, sum) * model@Delta)
                    mm <- sweep(mm, 2, cs, "/")
                    model@basisEnv$basis[[term]] <- mm
                  }
                }
              }
              setFilterTerms(model) <- filterTerms
              assign("basisComputed", TRUE, model@basisEnv)
            }
            ## TODO: Check correct dimensions of basis computations.
            return(model)
          }
)

setMethod("coefficients", "PointProcessModel",
          function(object, ...){
            return(object@coefficients)
          }
)

setReplaceMethod("coefficients", c(model = "PointProcessModel", value = "numeric"),
                 function(model, value){
                   nc <- length(model@coefficients)
                   d <- ncol(getModelMatrix(model))
                   if (d == nc) {
                     model@coefficients[] <- value
                   } else {
                     nv <- length(value)
                     if (nv == 1 && d > 1) {
                       value <- rep(value, d)
                     } else if (nv != d) {
                       value <- rep(.Machine$double.eps, d)
                       warning(paste("Incorrect length of parameter vector. Parameters all set to", .Machine$double.eps))
                     }
                     if (d > 0) 
                       names(value) <- colnames(getModelMatrix(model))
                     model@coefficients <- value
                   }           
                   
                   return(model)
                 }
)

setMethod("computeMinusLogLikelihood", "PointProcessModel",
          function(model, coefficients = NULL, fastIdentity = TRUE, ...) {
            if(fastIdentity && model@family@link == 'identity') {
              if(isTRUE(response(model) == ""))
                stop("No response variable specified.")
              if (is.null(coefficients)) 
                coefficients <- coefficients(model)
              
              Z <- getResponseMatrix(model)
              
              mll <- sum(getz(model) * coefficients) -
                sum(safeLog(as.numeric(Z %*% coefficients)))
            } else {
              mll <- callNextMethod()
            }
            
            return(mll)
          }
)


setMethod("computeQuadraticContrast", "PointProcessModel",
          function(model, coefficients = NULL, fastIdentity = TRUE, ...) {
            if(fastIdentity && model@family@link == 'identity') {
              G <- getCrossProd(model)
              if(is.null(G)) {
                qc <- callNextMethod()
              } else {
                if(isTRUE(response(model) == ""))
                  stop("No response variable specified.")
                if (is.null(coefficients)) 
                  coefficients <- coefficients(model)
                
                Z <- getResponseMatrix(model)
                
                qc <- as.numeric(t(coefficients) %*% G$G %*% coefficients) / 2 -
                  sum(as.numeric(Z %*% coefficients))
              }
            } else {
              qc <- callNextMethod()
            }
            return(qc)
          }
)          


setMethod("computeDMinusLogLikelihood", "PointProcessModel",
          function(model, coefficients = NULL, eta = NULL, fastIdentity = TRUE, ...){
            if (isTRUE(response(model) == ""))
              stop("No response variable specified.")
            
            Z <- getResponseMatrix(model)
            
            if (fastIdentity && model@family@link == 'identity') {
              
              if (is.null(coefficients)) 
                coefficients <- coefficients(model)
              
              dmll <- getz(model) -
                as.numeric(t(1/Z %*% coefficients) %*% Z)
              
            } else {
              if (is.null(eta))
                eta <- computeLinearPredictor(model, coefficients, ...)
              
              if (model@family@link == 'identity') {
            
                etaP <- eta[getPointPointer(processData(model), response(model))]
                
                dmll <- getz(model) -
                  as.vector((1/model@family@phi(etaP)) %*% Z)              
                
              } else if (model@family@link == "log") {
                
                dmll <- as.vector((exp(eta) * model@delta) %*%  getModelMatrix(model)) -
                  colSums(Z)
                
              } else {
                
                etaP <- eta[getPointPointer(processData(model), response(model))]
                
                dmll <-  as.vector((model@family@Dphi(eta) * model@delta) %*% getModelMatrix(model)) -
                  as.vector((model@family@Dphi(etaP)/model@family@phi(etaP)) %*% Z)
                
              }
            }
            
            return(dmll)
            
          }
)

setMethod("computeDDMinusLogLikelihood", "PointProcessModel",
          function(model, coefficients = NULL, eta = NULL, ...){
            if (isTRUE(response(model) == ""))
              stop("No response variable specified.")
            
            Z <- getResponseMatrix(model)
            
            if (model@family@link == 'identity') {
              
              if (is.null(coefficients)) 
                coefficients <- coefficients(model)
              
              w <- Diagonal(x = as.numeric(1/(Z %*% coefficients)^2))
              ddmll <-  as(crossprod(Z, w %*% Z), "matrix")
              
            } else {           
              
              if (is.null(eta))
                eta <- computeLinearPredictor(model, coefficients, ...)
              
              X <- getModelMatrix(model)
              
              if (model@family@link == "log"){
                
                w <- Diagonal(x = exp(eta) * model@delta)
                ddmll <-  as(crossprod(X, w %*% X), "matrix")                            
              } else {
                
                etaP <- eta[getPointPointer(processData(model), response(model))]
                w1 <- Diagonal(x = model@family@D2phi(eta) * model@delta)
                w2 <- Diagonal(x = (model@family@D2phi(etaP) * model@family@phi(etaP) -
                                      model@family@Dphi(etaP)^2) / model@family@phi(etaP)^2)
                
                ddmll <-  as(crossprod(X, w1 %*% X), "matrix") -
                  as(crossprod(Z, w2 %*% Z), "matrix")
                
              }
            }
            
            return(ddmll)
            
          }
)

setMethod("computeFisherInformation", "PointProcessModel",
          function(model, coefficients = NULL, eta = NULL, ...){
            if (isTRUE(response(model) == ""))
              stop("No response variable specified.")
            if (is.null(eta))
              eta <- computeLinearPredictor(model, coefficients, ...)
            
            if (model@family@link == "log") {
              w <- Diagonal(x = exp(eta) * model@delta)
            } else if (model@family@link == "identity") {
              w <- Diagonal(x = 1/eta * model@delta)
            } else {
              w <- Diagonal(x = model@family@Dphi(eta)^2/model@family@phi(eta) *
                              model@delta)
            }
            X <- getModelMatrix(model)
            as(crossprod(X, w %*% X), "matrix")            
          }
)


setMethod("computeWeights", "PointProcessModel",
          function(model, coefficients = NULL, method = c("Fisher", "empirical"), ...) {
            if (isTRUE(response(model) == ""))
              stop("No response variable specified.")
            
            eta <- computeLinearPredictor(model, coefficients, ...)
            
            if (model@family@link == "log"){
              
              w <-  exp(eta)*model@delta
              
            } else if (method[1] == "empirical") {
              if (model@family@link == "identity"){
                
                points <- getPointPointer(processData(model), response(model))
                etaP <- eta[points]
                w <- rep(0, length(eta))
                w[points] <- 1/etaP^2
                
              } else {
                
                points <- getPointPointer(processData(model), response(model))
                etaP <- eta[points]
                w <- model@family@D2phi(eta)*model@delta
                w[points] <- w[points] - (model@family@D2phi(etaP)*model@family@phi(etaP) - model@family@Dphi(etaP)^2)/model@family@phi(etaP)^2
                
              }
              
            } else if (method[1] == "Fisher") {
              if (model@family@link == "identity") {
                w <- model@delta/eta                 
              } else {
                w <- (model@family@Dphi(eta)^2*model@delta)/model@family@phi(eta)
              }
            } else {
              stop(paste("Method", method[1], "is not a valid weight method."))
            }
            
            ## Negative and small weights are set equal to 0. 
            
            w[w < .Machine$double.eps] <- 0
            
            return(w)
          }
)

setMethod("computeWorkingResponse", "PointProcessModel",
          function(model, coefficients = NULL, ...){
            if (isTRUE(response(model) == ""))
              stop("No response variable specified.")
            eta <- computeLinearPredictor(model, coefficients, ...)
            points <- getPointPointer(processData(model), response(model))
            w <- computeWeights(model)
            ## Small weights should not play a role. For numeric
            ## stability, they are put equal to 1 here.
            w[w < .Machine$double.eps] <- 1
            
            if (model@family@link == "log") {
              
              wr <- exp(eta)*model@delta
              wr[points] <- wr[points] - 1
              wr <- eta - wr/w
              
            } else {
              
              etaP <- eta[points]
              
              wr <-  model@family@Dphi(eta)*model@delta
              wr[points] <- wr[points] - model@family@Dphi(etaP)/model@family@phi(etaP)
              wr <- eta - wr/w
              
            }
            
            return(list(weights = w,
                        workingResponse = wr
            )
            )
          }
)

setMethod("computeLinearPredictor", "PointProcessModel",
          function(model, coefficients = NULL, ...){
            if (is.null(coefficients)) 
              coefficients <- coefficients(model)
                                               
            as.numeric(getModelMatrix(model) %*% coefficients)
          }
)

setMethod("computeModelMatrix", "PointProcessModel",
          function(model, evaluationPositions = NULL, exTerms = integer(), ...){
            
            ## Making sure that the basis evaluations are computed.
            ## The call to computeBasis is invoked for its side effect
            ## of computing the basis evaluations if that is not already
            ## done.
            
            model <- computeBasis(model, ...)
            
            ## The 'model' of class PointProcessModel contains the data
            ## as an object of class MarkedPointProcess and the formula for the
            ## model specification. The 'evalPositions' below corresponding to
            ## the model matrix rows are either given by the 'evaluationPositions'
            ## argument or extracted from the the MarkedPointProcess object (default).
            
            if (is.null(evaluationPositions)) {
              evalPositions <- tapply(getPosition(processData(model)),
                                      getId(processData(model)), list)
            } else {
              evalPositions <- evaluationPositions
            }
            
            ## Checks if the model is allowed to be anticipating and sets
            ## the 'zero' accordingly.
            
            if (anticipating(model)) {
              zero <- which(model@basisPoints == 0) - 1
            } else {
              zero <- 0
            }
            
            ## The observed points ('positions') for the marked point process,
            ## the corresponding 'id' labels and 'marks' are extracted.
            
            processData <- processData(model)
            positions <- getPointPosition(processData)
            DcontinuousVar <- paste(colnames(getValue(processData)), ".d", sep="")
            DpositionVar <- paste(processData@positionVar, "d.", sep = "")
            
            id <- factor(getPointId(processData))
            idLevels <- levels(id)
            
            marks <- getMarkType(processData, drop = FALSE)
            markLevels <- levels(marks)
            
            ## The formula object is extracted and decomposed into terms.
            ## Each term label (in 'termLabels') is processed below,
            ## and the corresponding columns in the model matrix are computed.
            
            mt <- delete.response(terms(formula(model)))
            termLabels <- attr(mt, "term.labels")
            filterTerms <- getFilterTerms(model)
            
            if(length(exTerms) > 0)
              filterTerms <- filterTerms[!filterTerms %in% exTerms]
            
            ## The points where the basis functions are evaluated are extracted
            ## and the list of model matrices ('design') is set up, which holds model
            ## matrices for the different terms. 'assign' will be an attribute to  
            ## the model matrix of length equal to the number of columns, and for
            ## each column pointing to the term number. 
            
            ## Model matrix computations for the terms involving filters:
            
            design <- lapplyParallel(filterTerms,
                                     function(i, ...) {
                                       term <- termLabels[i]
                                       variable <- all.vars(mt[i])
                                       
                                       ## The model matrix computed separately for terms
                                       ## involving marks and terms being linear filters of
                                       ## continuous processes and is done by a loop over 
                                       ## each value of 'id' whose result is stored in
                                       ## 'designList'.
                                       
                                       ## TODO: Can the C level computation return a sparse matrix
                                       ## directly?
                                       
                                       if (all(variable %in% markLevels))
                                       {
                                         
                                         designList <- list()
                                         
                                         ## Central loop over 'idLevels' and computations of
                                         ## the model matrix in the C function
                                         ## 'computeFilterMatrix'. Result is converted to a
                                         ## sparse matrix, bound together in one matrix below
                                         ## and stored in the variable 'localDesign'.
                                         basis <- getBasis(model, term)
                                         
                                         for(i in idLevels) {
                                           posi <- positions[marks == variable & id == i]
                                           ## posi is sorted for a valid data object. This is
                                           ## assumed in the following computation.
                                           designList[[i]] <- Matrix(.Call(computeFilterMatrix,
                                                                           evalPositions[[i]],
                                                                           basis,
                                                                           model@Delta,
                                                                           posi,
                                                                           zero,
                                                                           'p'))
                                         }
                                         localDesign <- do.call("rBind", designList)
                                         colnames(localDesign) <- colnames(basis)
                                       } else if (all(variable %in% c(DcontinuousVar, DpositionVar))) {
                                         
                                         designList <- list()    
                                         
                                         ## Central loop over 'idLevels' and computations of
                                         ## the model matrix in the C function
                                         ## 'computeFilterMatrix'. Result is converted to a
                                         ## sparse matrix, bound together in one matrix below
                                         ## and stored in the variable 'localDesign'.
                                         
                                         if (variable == DpositionVar) {
                                           values <- getPosition(processData)
                                         } else {
                                           values <- getNumerics(processData)[ , variable == DcontinuousVar]
                                         }
                                         
                                         basis <- getBasis(model, term)
                                         
                                         for(i in idLevels) {
                                           valuesi <- values[getId(processData) == i]
                                           
                                           designList[[i]] <- Matrix(.Call(computeFilterMatrix,
                                                                           evalPositions[[i]],
                                                                           basis,
                                                                           model@Delta,
                                                                           valuesi,
                                                                           zero,
                                                                           'c'))
                                         }
                                         localDesign <- do.call("rBind", designList)
                                         colnames(localDesign) <- colnames(basis)
                                       } 
                                       localDesign ## The return value
                                     },
                                     ## With multicore backend the
                                     ## jobs are spawned sequentially
                                     ## and not prescheduled. This is
                                     ## expected to be a sensible
                                     ## strategy here because
                                     ## different types of filter
                                     ## terms may involve highly
                                     ## different computation times,
                                     ## and the total number of terms
                                     ## is generally relatively small.
                                     mc.preschedule = FALSE 
            ) ## End lapplyParallel
            
            
            
            assign <- unlist(lapply(filterTerms,
                                    function(i) {
                                      rep(i, ncol(getBasis(model, termLabels[i])))
                                    }
            )
            )
            names(design) <- termLabels[filterTerms]
            
            ## Terms that do not involve filters
            if (length(filterTerms) > 0 || length(exTerms) > 0) {
              notFilterTerms <- seq_along(termLabels)[-c(filterTerms, exTerms)]
            } else {
              notFilterTerms <- seq_along(termLabels)
            }
            
            ## Model matrix computations for terms involving 'id',
            ## 'position/time', unit variables and non-filtered
            ## continuous time process components.
            
            if (length(notFilterTerms) > 0){
              form <-  mt[notFilterTerms]
              attr(form, "intercept") <-  attr(mt, "intercept")
              variables <- all.vars(form)
              otherVariables <- c(colNames(processData, type = "numeric"),
                                  colNames(processData, type = "factor"),
                                  colNames(processData, type = "unit"))
              
              if (all(variables %in% c(processData@idVar,
                                       processData@positionVar,
                                       otherVariables))) {
                
                ## TODO: Investigate if this build of the model matrix can exploit
                ## sparse matrices more directly, if it is necessary to create the
                ## values object, or if we could refer to a suitable environment?
                
                values <- vector("list", 2)
                
                if (processData@idVar %in% variables) {
                  values[[1]] <- getId(processData)
                  names(values)[1] <- processData@idVar
                }
                
                if (processData@positionVar %in% variables) {
                  values[[2]] <- getPosition(processData)
                  names(values)[2] <-  processData@positionVar
                }
                
                otherVariables <-  otherVariables[otherVariables %in% variables]
                if (length(otherVariables) > 0) {
                  values <- c(values, getColumns(processData, otherVariables, drop = FALSE))
                  names(values)[-c(1, 2)] <- otherVariables
                }
                
                values <- values[!sapply(values, is.null)]
                
                ## This sparse.model.matrix solution avoids the dense
                ## model matrix, but does not produce the assign
                ## attribute ...
                ## modelMatrix0 <- sparse.model.matrix(form, values)
                
                tmp <- model.matrix(form, values)
                assign <- c(c(0, notFilterTerms)[attr(tmp, "assign") + 1], assign)
                modelMatrix0 <- Matrix(tmp, dimnames = dimnames(tmp))
                assign <- c(c(0, notFilterTerms)[attr(modelMatrix0, "assign") + 1], assign)
              } else {
                stop(paste("Use of non existing variable(s) in:", form))
              }
            } else if (attr(mt, "intercept") == 1) {
              modelMatrix0 <- Matrix(rep(1, dim(processData)[1]))
              colnames(modelMatrix0) <- "(Intercept)"
              assign <- c(0, assign)
            } 
            
            if (exists("modelMatrix0")) {
              modelMatrix <- cBind(modelMatrix0, do.call("cBind", design))
            } else {
              modelMatrix <- do.call("cBind", design)
            }
            form <- formula(model)
            attr(form, "filterTerms") <- which(!(seq(along=termLabels) %in% notFilterTerms))
            model <- updateModelMatrix(model, modelMatrix, assign, form)
            lockEnvironment(model@modelMatrixEnv)
            return(model)
          }
)

setMethod("computeVar", "PointProcessModel",
          function(model, method = 'default', ...){
            if (method == 'default')
              method <- model@varMethod
            lambda <- penalty(model)
            model@df <- length(model@coefficients)
            switch(method,
                   subset = {
                     varMatrix <- vcov(model)
                     i <- which(rownames(varMatrix) %in% names(coefficients(model)))
                     model@var <- varMatrix[i, i, drop = FALSE]
                   },
                   none = {
                     vcov <- matrix(0, length(coefficients(model)),
                                    length(coefficients(model)))
                     rownames(vcov) <- colnames(vcov) <- names(model@coefficients)
                     model@var <- vcov
                   },
                   dfOnly = {
                     vcov <- matrix(0, length(coefficients(model)), length(coefficients(model)))
                     rownames(vcov) <- colnames(vcov) <- names(model@coefficients)
                     model@var <- vcov
                     ## eta <- predict(model)
                     ## TODO: implement something more intelligent. 
                     model@df <- model@df
                   },
                   lsSandwich = {
                     KJ <- computeSandwichKJ(model)
                     K <- KJ$K
                     J <- KJ$J
                     if (length(lambda) > 0)
                       diag(J) <- diag(J) + lambda  ## It should !not! be 2 * lambda here!
                     Jinv <- try(solve(J), silent = TRUE)
                     if (class(Jinv) == "try-error") {
                       vcov <- matrix(0, nrow = nrow(X), ncol = ncol(X))
                       message("Model matrix not of full column rank:\n", Jinv[1], " Check parametrization.")
                     } else {
                       JK <- Jinv %*% K
                       model@df <- sum(diag(JK))
                       vcov <- as(JK %*% Jinv, "matrix")
                     }
                     rownames(vcov) <- colnames(vcov) <- names(model@coefficients)
                     model@var <-  (vcov + t(vcov))/2   ## To ensure symmetry
                   },
                   lasso = {
                     ## TODO: Find a reasonable way to compute estimates of standard errors.
                     X <- getModelMatrix(model)
                     points <- getPointPointer(processData(model), response(model))
                     vcov <- matrix(0, nrow = nrow(X), ncol = ncol(X)[2])
                     switch(family(model)@link,
                            identity = {
                              II <- crossprod(sqrt(model@delta) * X)
                            },
                            log = {
                              II <- computeDDMinusLogLikelihood(model)
                            },
                            II <- diag(1, dim(vcov))
                     )
                     if (length(lambda) > 0)
                       diag(II) <- diag(II) + 2 * lambda
                     K <- crossprod(X[points, ])
                     nonZeroCoef <- coefficients(model) != 0
                     K <- K[nonZeroCoef, nonZeroCoef]
                     II <- II[nonZeroCoef, nonZeroCoef]
                     Iinv <- try(solve(II), silent = TRUE)
                     if (class(Iinv) == "try-error") {
                       message("Model matrix not of full column rank:\n", Iinv[1],
                               " Check parametrization.")
                     } else {
                       vcov[nonZeroCoef, nonZeroCoef] <- as(Iinv %*% K %*% Iinv, "matrix") ### Sandwich estimator                         
                       rownames(vcov) <- names(model@coefficients)
                       colnames(vcov) <- names(model@coefficients)
                     }
                     model@var <- vcov
                   },
                   Fisher = {
                     J <- computeFisherInformation(model)
                     ## With penalization, a sandwich-type estimator
                     ## of the variance based on the expected Fisher
                     ## information is used.
                     if (length(lambda) > 0) {
                       K <- J
                       diag(J) <- diag(J) + 2 * lambda
                     }
                     vcov <- matrix(0, nrow = nrow(J), ncol = ncol(J))
                     tmp <- try(solve(J), silent = TRUE)
                     if (class(tmp) == "try-error") {
                       message("Fisher information singular:\n", tmp[1],
                               " Check convergence status or parametrization.")
                     } else {
                       if (length(lambda) > 0) {
                         JK <- tmp %*% K
                         vcov <- JK %*% tmp
                         model@df <- sum(diag(JK))
                       } else {
                         vcov <- tmp
                       }
                     }
                     rownames(vcov) <- colnames(vcov) <- names(model@coefficients)
                     model@var <- (vcov + t(vcov))/2   ## To ensure symmetry
                   },
                   observedFisher = {
                     vcovInv <- computeDDMinusLogLikelihood(model)
                     if (length(lambda) > 0)
                       stop("The 'observedFisher' method cannot be combined with penalization. Try 'method = Fisher'." )
                     vcov <- matrix(0, nrow = dim(vcovInv)[1],
                                    ncol=dim(vcovInv)[2])
                     tmp <- try(solve(vcovInv), silent = TRUE)
                     if (class(tmp) == "try-error") {
                       message("Fisher information singular:\n", tmp[1], " Check convergence status or parameterization.")
                     } else {
                       vcov <- tmp
                     }
                     rownames(vcov) <- colnames(vcov) <- names(model@coefficients)
                     model@var <- (vcov + t(vcov))/2   ## To ensure symmetry
                   }
            )
            return(model)
          }
)

setMethod("computeSandwichKJ", "PointProcessModel",
          function(model, ...) {
            X <- getModelMatrix(model)
            eta <- predict(model)
            phi <- model@family@phi
            Dphi <- model@family@Dphi
            if (model@family@link == 'identity') {
              J <- getCrossProd(model)$G
              w <- Diagonal(x = eta * model@delta)
              K <- crossprod(w %*% X, X)
            } else {
              wX <- Diagonal(x = Dphi(eta)^2 * model@delta) %*% X
              J <- crossprod(wX, X)
              K <- crossprod(Diagonal(x = phi(eta)) %*% wX, X)
            }
            return(list(K = K, J = J))
          }
)

setMethod("getFilterTerms", "PointProcessModel",
          function(model, ...) {
            model@filterTerms
          }
)

setMethod("getLinearFilter", "PointProcessModel",
          function(model, se = FALSE, nr, ...){
            mt <- delete.response(terms(formula(model)))
            ## model <- computeBasis(model)
            filterTerms <- getFilterTerms(model)
            
            linearFilter <- list()
            design <- list()
            if (isTRUE(se))
              linearFilterSE <- list()
            
            if (missing(nr)) {
              nr <- length(model@basisPoints)
            } else {
              nr <- min(nr, length(model@basisPoints))
            }
            
            NR <- length(model@basisPoints)
            i <- round(seq_len(min(nr, NR))*NR/nr)
            
            for(j in filterTerms) {
              term <- attr(mt[j], "term.labels")
              varName <- paste(all.vars(parse(text = term)), collapse = ".")
              design[[varName]] <- cbind(design[[varName]],
                                         getBasis(model, term, rePar = TRUE)[i, , drop = FALSE])
            }
            
            for(j in seq_along(design)){
              linearFilter[[j]] <- design[[j]] %*% coefficients(model)[colnames(design[[j]])]
              if (isTRUE(se))
                linearFilterSE[[j]] <- sqrt(rowSums(design[[j]] %*% 
                                                      vcov(model)[colnames(design[[j]]), 
                                                                  colnames(design[[j]])] * design[[j]]))
            }
            
            names(linearFilter) <- names(design)
            linearFilter <- as.data.frame(linearFilter)
            if (dim(linearFilter)[2] == 0)
              linearFilter <- data.frame(row.names = model@basisPoints[i])
            
            if (isTRUE(se)) {
              return(list(linearFilter = cbind(data.frame(x = model@basisPoints[i]),
                                               linearFilter), se = linearFilterSE))
            } else {
              return(cbind(data.frame(x = model@basisPoints[i]),
                           linearFilter))
            }
            
          }
)

setMethod("getAssign", "PointProcessModel",
          function(model, col, ...){
            if (missing(col))
              col <- model@modelMatrixCol
            if (length(col) == 0) {
              assign <- model@modelMatrixEnv$assign
            } else {
              assign <- model@modelMatrixEnv$assign[col]
            }
            return(assign)
          }
)

setMethod("getBasis", c(model = "PointProcessModel", term = "ANY"),
          function(model, term, ...){
            if (missing(term))
              return(model@basisEnv$basis)
            return(model@basisEnv$basis[[term]])
          }
)

setMethod("getCrossProd", "PointProcessModel",
          function(model, ...) {
            if (!'G' %in% names(model@crossProd))
              return(NULL)
            return(model@crossProd)
          }
)


setMethod("getModelMatrix", c(model = "PointProcessModel", col = "ANY"),
          function(model, col, ...) {
            if (missing(col))
              col <- model@modelMatrixCol
            if (length(col) == 0) {
              modelMatrix <- model@modelMatrixEnv$modelMatrix
            } else {
              modelMatrix <- model@modelMatrixEnv$modelMatrix[, col, drop = FALSE]
            }
            return(modelMatrix)
          }
)

setMethod("getModelMatrixEnv", "PointProcessModel",
          function(model,...){
            return(list(modelMatrixEnv = model@modelMatrixEnv, modelMatrixCol = model@modelMatrixCol))
          }
)

setMethod("getResponseMatrix", "PointProcessModel",
          function(model, ...) {
            model@responseMatrix
          }
)

setMethod("getz", c(model = "PointProcessModel", col = "ANY"),
          function(model, col, ...) {
            if (missing(col))
              col <- model@modelMatrixCol
            if (length(col) == 0) {
              z <- model@modelMatrixEnv$z
            } else {
              z <- model@modelMatrixEnv$z[col]
            }
            return(z)
          }
)

setReplaceMethod("setBasis", c(model = "PointProcessModel", term = "character", value = "numeric"),
                 function(model, term, value){
                   if (environmentIsLocked(model@basisEnv))
                     model@basisEnv <- new.env(parent = emptyenv())
                   
                   model@basisEnv$basis[[term]] <- value
                   assign("basisComputed", FALSE, model@basisEnv)                   
                   lockEnvironment(model@basisEnv)
                   
                   return(model)
                 }
)

setReplaceMethod("setBasis", c(model = "PointProcessModel", term = "ANY", value = "list"),
                 function(model, term, value){
                   if (environmentIsLocked(model@basisEnv))
                     model@basisEnv <- new.env(parent = emptyenv())
                   
                   assign("basis", value, model@basisEnv)
                   assign("basisComputed", FALSE, model@basisEnv)
                   lockEnvironment(model@basisEnv)
                   
                   return(model)
                 }
)

setReplaceMethod("setModelMatrixEnv", c(model = "PointProcessModel", value = "list"),
                 function(model, value){
                   if (all(c("modelMatrixEnv", "modelMatrixCol") %in% names(value))){
                     model@modelMatrixEnv <- value$modelMatrixEnv
                     model@modelMatrixCol <- value$modelMatrixCol
                   } else {
                     stop("Right hand side of the assignment needs to be a list with two entries named 'modelMatrixEnv' and 'modelMatrixCol'.")
                   }
                   return(model)
                 }
)

setMethod("predict", "PointProcessModel",
          function(object, ...) {
            eta <- computeLinearPredictor(object, ...)
            return(object@family@phi(eta))
          }
)

setMethod("getTermPlotData", "PointProcessModel",
          function(model, alpha = 0.05, trans = NULL, ...) {
            if (alpha <= 0 || alpha > 1)
              stop("The 'alpha' level must be in (0,1]")
            
            if (isTRUE(all.equal(alpha, 1))) {
              se <- FALSE
            } else {
              se <- TRUE
              q <- qnorm(1-alpha/2)
            }
            
            linearFilter <- getLinearFilter(model, se = se, nr = 400)
            if (se) {
              moltenFilter <- reshape2::melt(linearFilter$linearFilter, id.vars = "x")
              plotData <- cbind(moltenFilter,
                                data.frame(cf.lower = moltenFilter$value - q*unlist(linearFilter$se),
                                           cf.upper = moltenFilter$value + q*unlist(linearFilter$se)))
              if (!is.null(trans))
                plotData[, c("value", "cf.lower", "cf.upper")] <-
                do.call(trans, list(plotData[, c("value", "cf.lower", "cf.upper")]))
            } else {
              plotData <- reshape2::melt(linearFilter, id.vars = "x")
              if (!is.null(trans))
                plotData$value <- do.call(trans, plotData$value)
            }
            
            return(plotData)
          }
)


setMethod("termPlot", "PointProcessModel",
          function(model, alpha = 0.05, layer = geom_line(), trans = NULL, 
                   confArg = list(fill = "blue", alpha = 0.2), ...) {
            if (length(getFilterTerms(model)) == 0){
              print("No filter function terms to plot.")
              return(invisible())
            }
            
            plotData <- getTermPlotData(model = model, alpha = alpha, trans = trans, ...)
            
            xLabel <- processData(model)@positionVar
            
            linearFilterPlot <- ggplot(data = plotData, aes(x = x, y = value)) +
              facet_grid(variable ~ ., scales = "free_y") +
              scale_x_continuous(xLabel) +
              scale_y_continuous("") 
            
            if (!isTRUE(all.equal(alpha, 1)))
              linearFilterPlot <- linearFilterPlot +
              geom_ribbon(aes(min = cf.lower, max = cf.upper),
                          fill = confArg$fill, alpha = confArg$alpha)
            
            return(linearFilterPlot + layer)
          }
)

setMethod("ppmFit", "PointProcessModel",
          function(model, control = list(), optim = 'optim', selfStart = TRUE, ...) {
            ## Check if combination of optimization method and link
            ## function is valid.
            link <- family(model)@link
            if (optim == 'ls' && link != 'identity')
              stop("The optim method 'ls' is only supported with the 'identity' link function.")
            if (optim == 'glmnet' && !link %in% c('identity', 'log'))
              stop("The optim method 'poisson' is only supported with the 'identity' or 'log' link function.")
            if (optim == 'poisson' && !link %in% c('log', 'identity', 'sqrt')) 
              stop("The optim method 'poisson' is only supported with the 'identity', 'log' or 'sqrt' link function.")
            
            ## Set the appropriate method for computation of the
            ## asymptotic covariance matrix on the parameter
            ## estimators.
            
            if (optim %in% c('optim', 'iwls', 'poisson')) {
              if (!model@varMethod %in% c('none', 'Fisher', 'observedFisher'))
                model@varMethod <- 'Fisher'
              model@loss <- 'likelihood'
            }
            if (optim == 'ls') {
              if (!model@varMethod %in% c('none', 'dfOnly'))
                model@varMethod <- 'lsSandwich'
              model@loss <- 'quadratic'
            }
            if (optim == 'glmnet') {
              model@varMethod <- 'none'  ## TODO: Change when implemented.
              model@loss <- 'quadratic'
            }
            
            
            ## For the 'identity' and 'root' link a least squares
            ## prefit is computed if 'selfStart' is TRUE.
            
            if (selfStart && (link == "identity" || link == "root"))
              model <- lsFit(model = model, control = control,
                             varMethod = model@varMethod, ...)
            
            model <- switch(optim,
                            optim = optimFit(model = model, control = control, ...),
                            iwls = iwlsFit(model = model, control = control, ...),
                            ls = lsFit(model = model, control = control, ...),
                            poisson = glmFit(model = model, control = control, ...),
                            glmnet = glmnetFit(model = model, control = control, ...),
                            stop(paste("No optimization method '", optim,
                                       "' available.", sep = ""), call. = FALSE)
            )
            
            ## Computation of the estimated covariance matrix and
            ## estimate of effective degrees of freedom.
            model <- computeVar(model)
          }
)

setMethod("optimFit", "PointProcessModel",
          function(model, control = list() , ...) {
            nrPar <- parDim <- length(coefficients(model))
            
            ## If the model is a submodel we temporarily change the
            ## full model matrix to the submodel matrix in this function
            ## to avoid repeated subsetting of the full matrix. 
            
            if (length(model@modelMatrixCol) > 0) {
              modelMatrixEnv <- getModelMatrixEnv(model)
              model <- updateModelMatrix(model)
            }
            
            if (!("maxit" %in% names(control)))
              control <- c(list(maxit = 1000), control)
            
            ## Setting up the initial parameters
            
            if (length(coefficients(model)) == parDim) {
              initPar <- coefficients(model)
            } else {
              initPar <- rep(.Machine$double.eps, nrPar)             
              warning("Length of initial parameter vector wrong. Initial parameters all set to 0.")
            }
            
            ## Setting up the objective function to minimize
            lambda <- penalty(model)
            
            if (length(lambda) == 0){
                mll <- function(par, ...) 
                  computeMinusLogLikelihood(model, par, ...)
                dmll <- function(par, ...) 
                  computeDMinusLogLikelihood(model, par, ...)
            } else {
                mll <- function(par, ...) 
                  computeMinusLogLikelihood(model, par, ...) + sum(lambda * par^2) 
                dmll <- function(par, ...) 
                  computeDMinusLogLikelihood(model, par, ...) + 2 * lambda * par
            } 

            ## The actual minimization
            
            args <- list(...)
            args[["par"]] <- initPar
            args[["fn"]] <- mll
            args[["gr"]] <- dmll
            args[["control"]] <- control
            
            method <- "BFGS"
            lower <- -Inf
            
            if (!("method" %in% names(args))) args[["method"]] <- method
            if (!("lower" %in% names(args))) args[["lower"]] <- lower
            
            model@optimResult <- do.call("optim", args)            
            coefficients(model) <- model@optimResult$par
           
            ## Resetting the full model matrix if required
            
            if (exists("modelMatrixEnv")) {
              setModelMatrixEnv(model) <- modelMatrixEnv
            }
            
            return(model)
          }
)            

setMethod("iwlsFit", "PointProcessModel",
          function(model, control = list(), ...) {
            
            value <- computeMinusLogLikelihood(model)
            reltol <- sqrt(.Machine$double.eps)
            maxit <- 1000  ## Currently hardcoded!
            trace <- 1
            
            i <- 1
            while(i < maxit) {
              ## Computation of weights and working response returned in a list
              ## with entries 'weights' and 'workingReponse'.
              wr <- computeWorkingResponse(model)
              ## TODO: check if lm.fit.sparse is exported. The
              ## following computation relies on an algorithm in the
              ## MatrixModels package still under development and
              ## currently not exported.
              coefficients(model) <- MatrixModels::lm.fit.sparse(getModelMatrix(model),
                                                                 wr$workingResponse,
                                                                 wr$weights)
              val <- computeMinusLogLikelihood(model)
              i <- i + 1
              if (trace > 0)
                cat("Value: ", val, "\n")
              if (val < value && value < reltol * (abs(value) + reltol) + val)
                break
              value <- val
            }
            
            if (i < maxit) {
              convergence <- 0
            } else {
              convergence <- 1
            }
            
            model@optimResult <- list(value = value,
                                      counts = c(i, 0),
                                      convergence = convergence
            )
            return(model)
          }
)

setMethod("glmnetFit", "PointProcessModel",
          function(model, control = list(), ...) {          
            hasGlmnet <- require("glmnet")
            if (!hasGlmnet) 
              stop("Package 'glmnet' is not installed.")
            
            link <- family(model)@link
            
            intercept <- which(getAssign(model) == 0)
            if (length(intercept) == 0) {
              warning("Model has no intercept. Currently, using 'glmnet' the lasso penalized model is fitted with an intercept.")
              X <- getModelMatrix(model)
            } else if (length(intercept) > 1) {
              stop("Internal error: Multiple intercept columns in model matrix")
            } else {
              X <- getModelMatrix(model)[, -intercept]
            }
            
            y <- rep(0, dim(X)[1])
            weights <- rep(1, dim(X)[1])
            points <- getPointPointer(processData(model), response(model))
            
            if (link == 'log')
            {
              y[points] <- 1
              offset <- log(model@delta)
              weights[offset == -Inf] <- 0
              offset[offset == -Inf] <- 0
              
              glmnetFit <- glmnet(x = X,
                                  y = y,
                                  family = "poisson",
                                  offset = offset,
                                  weights = weights,
            ##                      standardize = FALSE,
                                  alpha = 1,
                                  ...)
            } else if (link == 'identity')
            {
              weights <- model@delta
              yweighted <- 1/weights[points]
              yweighted[yweighted == Inf] <- 0
              y[points] <- yweighted
              ## The penalty weights are based on results derived in 
              ## Hansen, Reynaud-Bouret and Rivoirard 2013.
              penaltyWeights <- sqrt(colSums(X[points, ]^2))
              glmnetFit <- glmnet(x = X,
                                  y = y,
                                  family = "gaussian",
                                  weights = weights,
                                  standardize = FALSE,
                                  penalty.factor = penaltyWeights,
                                  alpha = 1,
                                  ...)
              
            }
            
            ## An AIC-type of criterion - ad hoc.
            err <- (1 - glmnetFit$dev.ratio) * glmnetFit$nulldev
            if (link == 'log') {
              selected <- which.min(2*glmnetFit$df + err)
            } else if (link == 'identity') {
              ## These computatations result in the same err vector
              ## hat <- t(glmnetFit$a0 + t(X %*% glmnetFit$beta))
              ## err2 <- colSums(weights * (y - hat)^2)
              
              ## The following uses an average prediction variance
              ## based on the Poisson distribution.
              m <- length(err)
              ## hat <- as.vector((glmnetFit$a0[20] + X %*% glmnetFit$beta[, 20]))
              ## sigmasqHat <- sum(pmax(as.vector(hat), 0))/(dim(X)[1] - glmnetFit$df[m])             
              ## TODO: Using the estimated sigmasq this way is ad hoc!
              ## It should probably be replaced by the more generel 
              ## trace-formula, which seems to concur with validation. 
              sigmasqHat <- err[m]/(dim(X)[1] - glmnetFit$df[m])
              selected <- which.min(2*glmnetFit$df*sigmasqHat + err)
            }
            coefficients(model) <- coefficients(glmnetFit)[, selected]
            
            optimResult <- list(value = computeMinusLogLikelihood(model),
                                counts = c(glmnetFit$npasses, 0),
                                convergence = 0
            )
            model@optimResult <- optimResult
            
            model <- computeVar(model)
            
            nonZeroTerms <- tapply(coefficients(model),
                                   getAssign(model),
                                   function(x) any(x != 0))
            model <- update(model,
                            names(nonZeroTerms)[nonZeroTerms],
                            fit = FALSE)
            
            return(model)
          }
)

setMethod("lsFit", "PointProcessModel",
          function(model, control = list(), ...) {
            
            model <- updateCrossProd(model)
            crossProd <- getCrossProd(model)
            lambda <- penalty(model)
            G <- crossProd$G
            if (length(lambda) > 0) 
              G <- G + diag(lambda) 
            ## TODO: Investigate if the Cholesky decomposition
            ## should be used for solving this equation. 
            coefficients(model) <- as.numeric(solve(G, crossProd$response))
                                                
            model@optimResult <- list(value = computeQuadraticContrast(model),
                                      counts = c(1, 0),
                                      convergence = 0
            )
            
            return(model)
          }
)


setMethod("glmFit", "PointProcessModel",
          function(model, control = list(), ...) {
            offset <- log(model@delta)
            weights <- rep(1, length(offset))
            weights[offset == -Inf] <- 0
            offset[offset == -Inf] <- 0
            y <- rep(0, length(offset))
            points <- getPointPointer(processData(model), response(model))
            y[points] <- 1
            
            link <- family(model)@link
            
            glmFit <- glm.fit(x = as(getModelMatrix(model), "matrix"),
                              y = y,
                              family = poisson(link),
                              offset = offset,
                              weights = weights,
                              control = control,
                              start = coefficients(model)
            )
            
            coefficients(model) <- glmFit$coefficients
            
            if (isTRUE(glmFit$converged)) {
              convergence <- 0
            } else {
              convergence <- glmFit$converged
            }
            
            optimResult <- list(value = computeMinusLogLikelihood(model),
                                counts = c(glmFit$iter, 0),
                                convergence = convergence
            )
            model@optimResult <- optimResult
            
            return(model)
          }
)


setMethod("print", "PointProcessModel",
          function(x, digits = max(3, getOption("digits") - 3), ...){
            cat("\nCall:\n", deparse(x@call), "\n\n", sep = "")
            if (length(coefficients(x))) {
              cat("Coefficients:\n")
              print.default(format(coefficients(x), digits = digits),
                            print.gap = 2, 
                            quote = FALSE)
            }
            else cat("No coefficients\n")
            cat("\n")
            invisible(x)
          }
)

setMethod("show", "PointProcessModel",
          function(object) print(x=object)
)

setMethod("subset", "PointProcessModel",
          function(x, ...){
            pointProcessModel(formula = formula(x),
                              data = subset(processData(x), ...),
                              family = x@family,
                              Delta = x@Delta,
                              support = x@support,
                              basisPoints = x@basisPoints,
                              Omega = x@Omega,
                              coefficients = coefficients(x),
                              basisEnv = x@basisEnv
            )
          }
)

### TODO: Summary should return an S4 object instead with appropriate view method.
### TODO: Implementation of standard model diagnostics.

setMethod("summary", "PointProcessModel",
          function(object,...) {
            result <- list()
            
            result$df <- object@df
            result$call <- object@call
            result$mll <- object@optimResult$value
            result$iter <- object@optimResult$counts
            result$convergence <- object@optimResult$convergence
            result$aic <- getInformation(object)
            
            ## TODO: Implement some way of summarizing 'residuals'
            
            se <- sqrt(diag(object@var))
            z <- object@coefficients/se
            q <- 2*(1-pnorm(abs(z)))
            z[se == 0] <- NA
            q[se == 0] <- NA
            se[se == 0] <- NA
            
            result$coefficients <- matrix(c(object@coefficients, se, z, q), ncol=4)
            zapNames <- sapply(names(object@coefficients),
                               function(y) {
                                 x <- strsplit(y, "")[[1]]
                                 if (length(x) > 18){
                                   end <- c("...", x[(length(x)-2):length(x)])
                                   return(paste(c(x[1:12], end), sep = "", collapse = ""))
                                 }
                                 return(y)
                               })
            
            
            dimnames(result$coefficients) <- list(zapNames,c("Estimate","Std. Error", "z value", "Pr(> |z|)"))
            
            class(result) <- c("summary.ppm")
            return(result)
            
          }
)

setMethod("vcov", "PointProcessModel",
          function(object,...){
            attr(object@var, "method") <- object@varMethod
            return(object@var)
          }
)

setMethod("update", "PointProcessModel",
          function(object, formula = NULL, warmStart = TRUE, 
                   fit = TRUE, lambda = NULL, ...){
            
            if (!is.null(formula)) {
              modelFormula <- formula(object)
              recompCrossProd <- FALSE
              if (class(formula) != "formula") {
                selectTerms <- as.numeric(formula)
                selectTerms <- selectTerms[selectTerms > 0]
                formula <- formula(terms(modelFormula)[selectTerms])
              }
              superFormula <- terms(object@modelMatrixEnv$formula)
              updatedFormula <- update(modelFormula, formula)
              updatedTermLabels <- attr(terms(updatedFormula), "term.labels")
              superTermLabels <- attr(superFormula, "term.labels")
              superFilterTerms <- superTermLabels[attr(object@modelMatrixEnv$formula,
                                                       "filterTerms")]
              setFilterTerms(object) <- which(updatedTermLabels %in%
                                                superFilterTerms)
              
              if (attr(terms(updatedFormula), "intercept") == 1)
                updatedTermLabels <- c("Intercept", updatedTermLabels)
              if (attr(superFormula, "intercept") == 1)
                superTermLabels <- c("Intercept", superTermLabels)
              
              formula(object) <- updatedFormula
              
              if (length(object@modelMatrixCol) == 0){
                tmpCoef <- coefficients(object)
              } else {
                tmpCoef <- rep(.Machine$double.eps, dim(getModelMatrix(object, numeric()))[2])
                tmpCoef[object@modelMatrixCol] <- coefficients(object)
              }
              
              if (all(updatedTermLabels %in% superTermLabels)) {
                if ("Intercept" %in% superTermLabels) {
                  col <- which(superTermLabels[getAssign(object, numeric()) + 1] %in% updatedTermLabels)
                } else {
                  col <- which(superTermLabels[getAssign(object, numeric())] %in% updatedTermLabels)
                }
                ## TODO: The following checks whether there are
                ## interactions in the model. There is a general problem
                ## with choices of contrasts in submodels that should be
                ## dealt with. Removing e.g. the intercept gives a
                ## peculiar model it seems ...
                if (any(attr(terms(modelFormula), "order") >= 2)) {
                  warning(paste(c(object@call, "Original model formula includes interaction terms. Check that updated model is as expected."),
                                collapse="\n"), call. = FALSE)
                }
                if (length(col) == length(tmpCoef)) {
                  object@modelMatrixCol <- numeric()
                } else {
                  object@modelMatrixCol <- col
                }
                recompCrossProd <- TRUE
                if (warmStart) {
                  coefficients(object) <- tmpCoef[col]
                } else {
                  coefficients(object) <- rep(0, length(col))
                }
              } else {
                assign("basisComputed", FALSE, object@basisEnv)
                object <- computeModelMatrix(object)
                coefficients(object) <- rep(.Machine$double.eps,
                                            dim(getModelMatrix(object))[2])
                object@crossProd <- list()
              }
              
              call <- as.list(object@call)
              call$formula <- formula(object)
              object@call <- as.call(call)
            }
            
            if (!is.null(lambda))
              object@lambda <- lambda
            
            ## TODO: Write a check of whether it is necessary to update the response matrix
            object <- updateResponseMatrix(object)
            object <- updateCrossProd(object, recomp = recompCrossProd)
            
            
            if (fit) {
              object <- ppmFit(object, ...)
            } else {
              object <- computeVar(object, method = "subset")
            }
            
            return(object)
          }
)

setMethod("getInformation", "PointProcessModel",
          function(model, k = 2, ...) {
            loss <- computeLoss(model)
            return(2 * loss + k * model@df)
          }
)

setReplaceMethod("setFilterTerms", "PointProcessModel",
                 function(model, value) {
                   model@filterTerms <- value
                   return(model)
                 }
)

setMethod("penalty", "PointProcessModel",
                 function(model, ...) {
                   model@lambda
                 }
)

setReplaceMethod("penalty", c("PointProcessModel", "numeric"),
                 function(model, value) {
                   parDim <- length(coefficients(model)) 
                   penDim <- length(value)
                   if (parDim == penDim) {
                     model@lambda <- value
                   } else if (penDim == 0) {
                     model@lambda <- numeric()
                   } else {
                     model@lambda <- rep(value[1], parDim)
                     if (length(value) > 1)
                       warning("Penalty vector has wrong length. Only first value used.")
                   }
                   
                   model
                 }
)

## TODO: Is it possible to implement the following using parallelization?

setMethod("stepInformation", "PointProcessModel",
          function(model, scope = ~ 0, direction = "both", trace = 1,
                   steps = 1000, warmStart = TRUE, k = 2, ...) {
            if (trace == 1) 
              cat(" Step \t  AIC \t\t Direction\n")
            
            AIC <- list()
            models <- list()
            models[["current"]] <- model
            models$current@varMethod <- 'none'
            step <- 1
            termsToAdd <- attr(terms(scope), "term.labels")
            initialForm <- terms(formula(model))
            initialTerms <- attr(initialForm, "term.labels")
            currentTerms <- initialTerms
            termsToRemove <- currentTerms
            minDirection <- "start"
            while(step < steps) {
              AIC$current <- getInformation(models$current, k, ...)
              
              if (trace == 1)
                cat(" ", step, "\t", AIC$current, "\t ", minDirection, "\n")
              
              if (trace == 2) {
                cat("Step:", step, "\tAIC:", AIC$current,"\n")
                cat("Model:", deparse(formula(models$current)),"\n")
                cat("Direction:", minDirection, "\n\n")
                
              }
              
              if (direction == "both") {
                if (length(termsToAdd) > 0) {
                  AIC$forward <- numeric(length(termsToAdd))
                  names(AIC$forward) <- termsToAdd
                  models$forward <- list()
                  for(term in termsToAdd) {
                    models$forward[[term]] <- update(models$current,
                                                     as.formula(paste(".~. +", term)),
                                                     warmStart = warmStart)
                    AIC$forward[term] <- getInformation(models$forward[[term]], k, ...)
                  }
                }
              }
              
              if (direction %in% c("both", "backward")) {
                if (length(termsToRemove) > 1 || (length(termsToRemove) == 1 & (attr(initialForm, "intercept") == 1))) {
                  AIC$backward <- numeric(length(termsToRemove))
                  names(AIC$backward) <- termsToRemove
                  models$backward <- list()
                  for(term in termsToRemove) {
                    models$backward[[term]] <- update(models$current,
                                                      as.formula(paste(".~. -", term)),
                                                      warmStart = warmStart)
                    AIC$backward[term] <- getInformation(models$backward[[term]], k, ...)
                  }
                }
              }
              
              minDirection <- names(which.min(sapply(AIC, min)))
              
              if (minDirection == "current")              
                break()
              
              if (minDirection == "forward") {
                termIndex <- which.min(AIC$forward)
                models$current <- models$forward[[termIndex]]
                termsToRemove <- currentTerms
                currentTerms <- c(currentTerms, termsToAdd[termIndex]) 
                termsToAdd <- termsToAdd[-termIndex]
              }
              
              if (minDirection == "backward") {
                termIndex <- which.min(AIC$backward)
                models$current <-  models$backward[[termIndex]]
                termsToAdd <- initialTerms[!(initialTerms %in% currentTerms)]
                currentTerms <- termsToRemove[-termIndex]
                termsToRemove <- currentTerms
              }
              
              step <- step + 1
            }
            models$current@varMethod <- model@varMethod
            model <- ppmFit(models$current)
            return(invisible(model))
          }
)

setMethod("updateCrossProd", "PointProcessModel",
          function(model, recomp = FALSE, ...) {
            if(xor('G' %in% names(model@crossProd), recomp))
              return(model)
            
            X <- getModelMatrix(model)
            G <- crossprod(X, Diagonal(x = model@delta) %*% X)            
            model@crossProd <-  list(G = G, response = colSums(model@responseMatrix))

            return(model)
          }
)

setMethod("updateModelMatrix", "PointProcessModel",
          function(model, modelMatrix = getModelMatrix(model), assign = getAssign(model), form){
            force(modelMatrix)
            model@modelMatrixEnv <- new.env(parent = emptyenv())
            model@modelMatrixEnv$modelMatrix <- modelMatrix
            model@modelMatrixEnv$assign <- assign
            if (!missing(form))
              model@modelMatrixEnv$formula <- form
            model@modelMatrixCol <- numeric()
            if(nrow(modelMatrix) > 0) {
              if(model@family@link == 'identity')
                model@modelMatrixEnv$z <-  as.numeric(model@delta %*% modelMatrix)
              model <- updateResponseMatrix(model)
            }
            return(model)
          }
)

setMethod("updateResponseMatrix", "PointProcessModel",
          function(model, ...) {
            if (!isTRUE(response(model) == "")) {
              X <- getModelMatrix(model)
              pointers <- getPointPointer(processData(model),
                                          response(model))
              model@responseMatrix <- X[pointers, , drop = FALSE]
            }
            return(model)
          }
)
