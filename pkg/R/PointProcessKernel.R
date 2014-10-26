ppKernel <- function(
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
  kernel = sobolevKernel,
  specThres = 1e-8,
  ...) {
  
  
  call <- kCall <- match.call()
  kCall[[1]] <- quote(pointProcessModel)
  kCall$fit <- kCall$modelMatrix <- FALSE
  form <- eval(kCall$formula)
  
  terms <- terms(formula, specials = "k")
  response <- NULL
  if(attr(terms, "response") != 0)
    response <- terms[[2]]
  kCall$formula <- reformulate(sub("k\\(", ".__k__(", attr(terms, "term.labels")),
                               response = response)
  kernels <- attr(terms, "specials")$k
  kernelTerms <- which(apply(attr(terms, "factor")[kernels, , drop = FALSE] > 0, 2, any))
  model <- eval(kCall, parent.frame())
  if (class(model) == "MultivariatePointProcess")
    stop("Multivariate models currently not supported with 'ppKernel'.")
  names(model@basisEnv$basis) <- sub(".__k__\\(", "k(", 
                                     names(model@basisEnv$basis))
  formula(model) <- form
  
  model@call <- call
  
  if (modelMatrix) {
    model <- computeModelMatrix(model, exTerms = kernelTerms)
    model <- as(model, "PointProcessKernel")
    model@kernelTerms <- kernelTerms
    model <- computeModelMatrix(model)
  }
  else {
    model <- updateKernelMatrix(as(model, "PointProcessKernel"),
                                Matrix(ncol = 0, nrow = 0),
                                assign = numeric(),
                                form = formula(~0)
    )
  }
  
  model <- computeBasis(model, kernel = kernel, specThres = specThres)
  if (missing(coefficients))
    coefficients <- .Machine$double.eps
  coefficients(model) <- coefficients
  
  d <- ncol(getKernelBasis(model))
  nc <- length(coefficients(model))
  if (missing(lambda)) {
    lambda <- c(rep(0, nc - d), rep(1, d))
  } else if (length(lambda) == 1) {
    lambda <- c(rep(0, nc - d), rep(lambda, d))    
  } else if (length(lambda) == d) {
    lambda <- c(rep(0, nc - d), lambda, d)        
  }  
  penalty(model) <- lambda
  
  if(fit) {
    model <- ppmFit(model, selfStart = FALSE, ...)
  }
  else {
    ## Initializing the variance matrix without computing it.
    model <- computeVar(model, method = 'none')  
  }
  
  return(model)
}

setMethod("computeBasis", "PointProcessKernel",
          function(model, kernel, specThres = specThres, ...) {
            
            ## The basis for the kernel terms is
            ## computed and stored in the basis environment. 
            kernelTerms <- getKernelTerms(model)
            
            if (length(kernelTerms) >= 1) {
              grid <- model@basisPoints
              support <- model@support
              Gram <- outer(grid, grid, kernel, t = support[2])
              GramSpec <- eigen(Gram, symmetric = TRUE)
              ### Selection of effectively non-zero eigen-values.
              specRatio <- GramSpec$values / GramSpec$values[1]
              d <-  specRatio > max(specThres[1], .Machine$double.eps)
              U <- GramSpec$vectors[, d] / sqrt(model@Delta)
              U <- t(t(U) * sqrt(specRatio[d]))  
              colnam <- character()
              for (i in seq_along(kernelTerms)) {
                term <- kernelTerms[i]
                colnames(U) <- paste(names(kernelTerms)[i], seq_len(ncol(U)), sep = "")
                model@basisEnv$basis[[term]] <- U
                colnam <- c(colnam, colnames(U))
              }
              model@U <- bdiag(model@basisEnv$basis[kernelTerms])
              colnames(model@U) <- colnam
            }            
            model
          }
)

setReplaceMethod("coefficients", c(model = "PointProcessKernel", value = "numeric"),
                 function(model, value){
                   nc <- length(model@coefficients)
                   d1 <- ncol(getModelMatrix(model)) 
                   d2 <- ncol(getKernelBasis(model))
                   d <- d1 + d2
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
                       names(value) <- c(colnames(getModelMatrix(model)),
                                         colnames(getKernelBasis(model)))
                     model@coefficients <- value
                   }           
                   model@g <- as.numeric(getKernelBasis(model) %*% 
                                           coefficients(model)[seq(d1 + 1, d)])
                   
                   return(model)
                 }
)

setMethod("computeModelMatrix", "PointProcessKernel",
          function(model, evaluationPositions = NULL, ...){
            
            ## The 'model' of class PointProcessKernel contains the data
            ## as an object of class MarkedPointProcess and the formula for the
            ## model specification. The 'evalPositions' below corresponding to
            ## the model matrix rows are either given by the 'evaluationPositions'
            ## argument or extracted from the the MarkedPointProcess object (default).
            
            if(is.null(evaluationPositions)) {
              evalPositions <- tapply(getPosition(processData(model)),
                                      getId(processData(model)), list)
            } else {
              evalPositions <- evaluationPositions
            }
            
            ## Checks if the model is allowed to be anticipating and sets
            ## the 'zero' accordingly.
            
            if(anticipating(model)) {
              zero <- which(model@basisPoints == 0) - 1
            } else {
              zero <- 0
            }
            
            ## The observed points ('positions') for the marked point process,
            ## the corresponding 'id' labels and 'marks' are extracted.
            
            processData <- processData(model)
            positions <- getPointPosition(processData)
            r <- length(model@basisPoints)
            
            id <- factor(getPointId(processData))
            idLevels <- levels(id)
            
            marks <- getMarkType(processData, drop = FALSE)
            markLevels <- levels(marks)
            
            ## The special terms in the formula that encodes the
            ## linear filters that are modeled non-parametrically are
            ## identified and the formula for the remaining model
            ## specification is constructed.
            
            formula <- formula(model)
            terms <- terms(formula, specials = "k")
            terms <- delete.response(terms)
            kernels <- attr(terms, "specials")$k
            kernelTerms <- which(apply(attr(terms, "factor")[kernels, , drop = FALSE] > 0, 2, any))
            termLabels <- attr(terms, "term.labels")
            
            ## The points where the basis functions are evaluated are extracted
            ## and the list of model matrices ('design') is set up, which holds model
            ## matrices for the different terms. 'assign' will be an attribute to  
            ## the model matrix of length equal to the number of columns, and for
            ## each column pointing to the term number. 
            
            ## Model matrix computations for the terms involving filters:
            
            design <- lapplyParallel(kernelTerms,
                                     function(i, ...) {
                                       term <- termLabels[i]
                                       variable <- all.vars(terms[i])
                                       
                                       if(!variable %in% markLevels)
                                         stop("The use of kernel filters is only implemented for point process variables.")
                                       
                                       
                                       ## The occurrence matrix is computed by a loop over 
                                       ## each value of 'id' whose result is stored in
                                       ## 'designList'.
                                       
                                       ## TODO: C level computation?
                                       
                                       designList <- list()
                                       
                                       ## Central loop over 'idLevels' and computations of
                                       ## the occurrence matrix as a sparse matrix, bound together
                                       ## in one matrix below
                                       ## and stored in the variable 'localDesign'.
                                       
                                       for(i in idLevels) {
                                         posi <- positions[marks == variable & id == i]
                                         ## posi is sorted for a valid data object. This is
                                         ## assumed in the following computation.                                        
                                         
                                         xt <- evalPositions[[i]]
                                         nt <- length(xt)
                                         xs <- posi
                                         ns <- 1
                                         xZ <- numeric(nt*r)
                                         d <- model@Delta
                                         antip <- zero
                                         w <- (r-1)*d
                                         
                                         for(ii in 1:nt) {
                                           target = xt[ii] + antip;
                                           while(ns < length(xs) && target > xs[ns+1])
                                             ns <- ns + 1;
                                           nss = ns;
                                           diff = target - xs[ns];
                                           if(diff > 0) {
                                             while(diff <= w) 
                                             { 
                                               lookupIndex = floor(diff/d + 0.5);
                                               entry = ii + nt*lookupIndex;
                                               xZ[entry] <- xZ[entry] + 1;
                                               ns <- ns - 1;
                                               if(ns < 1) break;
                                               diff = target - xs[ns];
                                             }
                                           }
                                           ns = nss;
                                         }
                                         
                                         designList[[i]] <- Matrix(xZ, nrow = nt, sparse = TRUE)
                                       }
                                       localDesign <- do.call("rBind", designList)
                                       colnames(localDesign) <- paste(term, 1:r, sep = "")
                                       localDesign ## The return value
                                     },     
                                     mc.preschedule = FALSE 
            ) ## End lapplyParallel
            
            assign <- unlist(lapply(kernelTerms,
                                    function(i) {
                                      rep(i, r)
                                    }
            )
            )
            names(design) <- termLabels[kernelTerms]
            kernelMatrix <- do.call("cBind", design)
            form <- formula(model)
            attr(form, "kernelTerms") <- kernelTerms
            model <- updateKernelMatrix(model, kernelMatrix, assign, form)
            lockEnvironment(model@kernelMatrixEnv, bindings = TRUE)
            
            ## formula(model) <- formulaNoKernels
            ## as(model, "PointProcessModel") <-
            ##   computeModelMatrix(as(model, "PointProcessModel"),
            ##                      evaluationPositions = evaluationPositions,
            ##                      ...)
            ## formula(model) <- formula
            
            return(model)
          }
)

setMethod("computeFisherInformation", "PointProcessKernel",
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
            wX <- w %*% X
            H <- getKernelMatrix(model)
            U <- getKernelBasis(model)
            a <- as(crossprod(wX, X), "matrix")
            b <- as(crossprod(wX, H) %*% U, "matrix")
            c <- as(crossprod(U, crossprod(w %*% H, H)) %*% U, "matrix")
            rbind(cbind(a, b), cbind(t(b), c))
          }
)

setMethod("kernelCoefficients", "PointProcessKernel",
          function(object,...){
            object@g
          }
)

setMethod("computeLinearPredictor", "PointProcessKernel",
          function(model, coefficients = NULL, ...) {
            if(!is.null(coefficients)) 
              coefficients(model) <- coefficients
            
            modelMatrix <- getModelMatrix(model)
            d <- ncol(modelMatrix)
            coefficients <- coefficients(model)[seq_len(d)]
            kernelCoefficients <- kernelCoefficients(model)
            
            as.numeric(modelMatrix %*% coefficients) +
              as.numeric(getKernelMatrix(model) %*% kernelCoefficients)             
          }
)

setMethod("computeMinusLogLikelihood", "PointProcessKernel",
          function(model, coefficients = NULL, ...) {
            callNextMethod(model = model, coefficients = coefficients, 
                           fastIdentity = FALSE, ...)
          }
)

setMethod("computeQuadraticContrast", "PointProcessKernel",
          function(model, coefficients = NULL, ...) {
            callNextMethod(model = model, coefficients = coefficients, 
                           fastIdentity = FALSE, ...)
          }
)

setMethod("computeDMinusLogLikelihood", "PointProcessKernel",
          function(model, coefficients = NULL, eta = NULL, ...) {
            if(isTRUE(response(model) == ""))
              stop("No response variable specified.")
            if(is.null(eta))
              eta <- computeLinearPredictor(model, coefficients, ...)
            Z <- getResponseMatrix(model)
            X <- getModelMatrix(model)
            ZZ <- getResponseKernelMatrix(model)
            kM <- getKernelMatrix(model)
            
            if (model@family@link == 'identity') {
              
              etaP <- 1/eta[getPointPointer(processData(model), response(model))]
              
              dmll <- getz(model) - c(as.vector(etaP %*% Z), as.vector(etaP %*% ZZ))
              
            } else if (model@family@link == "log") {
              
              expEta <- exp(eta) * model@delta
              
              dmll <- c(as.vector(expEta %*%  X) - colSums(Z),
                        as.vector((as.vector(expEta %*% kM) - colSums(ZZ)) %*% model@U))
              
            } else {
              
              etaP <- eta[getPointPointer(processData(model), response(model))]
              Dphi <- model@family@Dphi(eta) * model@delta
              DphiPhi <- model@family@Dphi(etaP) / model@family@phi(etaP)
              
              dmll <-  c(as.vector(Dphi %*% X) - as.vector(DphiPhi %*% Z),
                         as.vector((as.vector(Dphi %*% kM) - as.vector(DphiPhi %*% ZZ)) %*% model@U))
              
            }
            
            return(as.numeric(dmll))
          }
)

setMethod("computeDDMinusLogLikelihood", "PointProcessKernel",
          function(model, coefficients = NULL, eta = NULL, ...){
            if (isTRUE(response(model) == ""))
              stop("No response variable specified.")
            
            Z <- getResponseMatrix(model)
            H <- getKernelMatrix(model)
            U <- getKernelBasis(model)
            
            if (model@family@link == 'identity') {
              
              pointers <- getPointPointer(processData(model), response(model))
              HU <- H[pointers, , drop = FALSE] %*% U
              Z <- cBind(Z, HU)
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
                wX <- w %*% X
                a <- as(crossprod(wX, X), "matrix")
                b <- as(crossprod(wX, H) %*% U, "matrix")
                c <- as(crossprod(U, crossprod(w %*% H, H)) %*% U, "matrix")
                
                ddmll <- rbind(cbind(a, b), cbind(t(b), c))
              } else {
                
                pointers <- getPointPointer(processData(model), response(model))
                HU <- H[pointers, , drop = FALSE] %*% U
                Z <- cBind(Z, HU)
                
                etaP <- eta[pointers]
                w1 <- Diagonal(x = model@family@D2phi(eta) * model@delta)
                w2 <- Diagonal(x = (model@family@D2phi(etaP)*model@family@phi(etaP) -
                                      model@family@Dphi(etaP)^2)/model@family@phi(etaP)^2)
                
                wX <- w1 %*% X
                a <- as(crossprod(wX, X), "matrix")
                b <- as(crossprod(wX, H) %*% U, "matrix")
                c <- as(crossprod(U, crossprod(w1 %*% H, H)) %*% U, "matrix")
                
                ddmll <-  rbind(cbind(a, b), cbind(t(b), c)) -
                  as(crossprod(Z, w2 %*% Z), "matrix")
                
              }
            }
            
            return(ddmll)
            
          }
)

setMethod("getKernelMatrix", c(model = "PointProcessKernel", col = "ANY"),
          function(model, col,...){
            if(missing(col))
              col <- model@kernelMatrixCol
            if(length(col) == 0) {
              kernelMatrix <- model@kernelMatrixEnv$kernelMatrix
            } else {
              kernelMatrix <- model@kernelMatrixEnv$kernelMatrix[, col, drop = FALSE]
            }
            return(kernelMatrix)
          }
)

setMethod("getKernelAssign", "PointProcessKernel",
          function(model, col, ...){
            if (missing(col))
              col <- model@kernelMatrixCol
            if (length(col) == 0) {
              assign <- model@kernelMatrixEnv$assign
            } else {
              assign <- model@kernelMatrixEnv$assign[col]
            }
            return(assign)
          }
)

setMethod("getKernelTerms", "PointProcessKernel",
          function(model, ...){
            model@kernelTerms
          }
)

setMethod("getKernelBasis", "PointProcessKernel",
          function(model, ...){
            model@U
          }
)

setMethod("getResponseKernelMatrix", "PointProcessKernel",
          function(model, ...) {
            model@responseKernelMatrix
          }
)

## TODO: Implement update method.

setMethod("update", "PointProcessKernel",
          function(object, ...) {
            message("No 'update' method currently implemented for a 'PointProcessKernel' object.")
            object
          }          
)

setMethod("updateResponseKernelMatrix", "PointProcessKernel",
          function(model, ...) {
            if (!isTRUE(response(model) == "")) {
              X <- getKernelMatrix(model)
              pointers <- getPointPointer(processData(model),
                                          response(model))
              model@responseKernelMatrix <- X[pointers, , drop = FALSE]
            }
            return(model)
          }
)

setMethod("updateKernelMatrix", "PointProcessKernel",
          function(model, kernelMatrix = getKernelMatrix(model), 
                   assign = getKernelAssign(model), form){
            force(kernelMatrix)
            model@kernelMatrixEnv <- new.env(parent = emptyenv())
            model@kernelMatrixEnv$kernelMatrix <- kernelMatrix
            model@kernelMatrixEnv$assign <- assign
            if(!missing(form))
              model@kernelMatrixEnv$formula <- form
            model@kernelMatrixCol <- numeric()
            if(nrow(kernelMatrix) > 0) {
              model <- updateResponseKernelMatrix(model)
            }
            return(model)
          }
)

## TODO: New summary function for an object of class 'PointProcessKernel'.

setMethod("summary", "PointProcessKernel",
          function(object, ...) {
            callNextMethod()
          }
)