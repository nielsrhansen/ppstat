ppSmooth <- function(
  formula,
  data,
  family,
  support = 1,
  knots = 'log',
  N = 200,
  Delta,
  lambda,
  coefficients,
  fit = TRUE,
  varMethod = 'Fisher',
  ...) {
  
  call <- sCall <- match.call()
  sCall[[1]] <- quote(pointProcessModel)
  sCall$fit <- sCall$modelMatrix <- FALSE
  form <- eval(sCall$formula)
  
  ### Construction of the basis expansions
  terms <- terms(formula, "s")
  if(attr(terms, "response") == 0)
    stop("No response variable specified.") 
  
  specials <- attr(terms, "specials")$s
  smoothVar <- lapply(as.list(attr(terms, "variables"))[1 + specials], all.vars)
  sCall$formula <- reformulate(sub("s\\(", ".__s__(", attr(terms, "term.labels")),
                               response = terms[[2]])
  termLabels <- attr(terms, "term.labels")
  smoothTerms <- which(apply(attr(terms, "factor")[specials, , drop = FALSE] > 0, 2, any))
  model <- eval(sCall, parent.frame())
  if (class(model) == "MultivariatePointProcess")
    stop("Multivariate models currently not supported with 'ppSmooth'.")
  names(model@basisEnv$basis) <- sub(".__s__\\(", "s(", 
                                     names(model@basisEnv$basis))
  formula(model) <- form
  model <- as(model, "PointProcessSmooth")
  model@smoothTerms <- smoothTerms
  ## 'knots' is passed further on to 'computeBasis' as the knot-selection strategy.
  model <- computeModelMatrix(model, strategy = knots)
  
  nc <- ncol(getModelMatrix(model))
  rePar <- getAssign(model) %in% smoothTerms
  Vlist <- lapply(model@basisEnv$basis[names(smoothTerms)], function(b) attr(b, "V"))
  model@V <- Diagonal(nc)
  model@V[rePar, rePar] <- bdiag(Vlist)
  misCoef <- missing(coefficients)
  if (misCoef)
    coefficients <- .Machine$double.eps
  coefficients(model) <- coefficients
  if (misCoef)
    coefficients(model) <- as.numeric(solve(model@V, coefficients(model)))
  colnames(model@V) <- names(coefficients(model))
  if (missing(lambda)) 
    lambda <- 1
  if (length(lambda) == 1) {
    lamb <- unlist(sapply(Vlist, function(x) c(0, 0, rep(lambda, ncol(x) - 2))))
    lambda <- rep(0, nc)
    lambda[rePar] <- lamb
  }
  
  penalty(model) <- lambda
  model <- updateResponseMatrix(model)
  
  if(fit) {
    model <- ppmFit(model, ...)
  } else {
    ## Initializing the variance matrix without computing it.
    model <- computeVar(model, method = "none")
  }
  
  model@call <- call
  return(model)
}

setMethod("computeBasis", "PointProcessSmooth",
          function(model, strategy = "log", ...) {
            model <- callNextMethod()
            
            ## The basis for the smooth terms is
            ## computed and stored in the basis environment. 
            smoothTerms <- names(getSmoothTerms(model))
            data <- processData(model)
            terms <- terms(formula(model))
            response <- all.vars(as.list(attr(terms, "variables"))[[1 + attr(terms, "response")]])
            
            if (length(smoothTerms) >= 1) {
              x <- getPointPosition(data)[getMarkType(data) %in% response]
              grid <- model@basisPoints
            
              for (i in seq_along(smoothTerms)) {
                smoothVar <- all.vars(reformulate(smoothTerms[i]))
                y <- getPointPosition(data)[getMarkType(data) %in% smoothVar]
                knots <- computeKnots(x, y, model@support, strategy)
                basis <- bSpline(grid, knots = knots)                
                cs <- sqrt(apply(basis^2, 2, sum) * model@Delta)
                basis <- sweep(basis, 2, cs, "/")
                d <- ncol(basis)
                basis <- cbind(model@basisEnv$basis[[smoothTerms[i]]], basis[, -c(1, d)])
                colnames(basis) <- paste(smoothTerms[i], seq_len(d), sep = "")
                model@basisEnv$basis[[smoothTerms[i]]] <- basis
                s1 <- s2 <- s3 <- s4 <- numeric(d)
                s <-  .Fortran("sgram", as.double(s1), as.double(s2),
                               as.double(s3), as.double(s4),
                               as.double(knots),
                               as.integer(d))
                Gram <- matrix(0, d, d)
                diag(Gram) <- s[[1]] / cs^2
                Gram[seq(2, d*d, d+1)] <- Gram[seq(d+1, d*d, d+1)] <- s[[2]][1:(d-1)] / (cs[1:(d-1)] * cs[2:d])
                Gram[seq(3, d*(d-1), d+1)] <- Gram[seq(2*d+1, d*d, d+1)] <- s[[3]][1:(d-2)] / (cs[1:(d-2)] * cs[3:d])
                Gram[seq(4, d*(d-2), d+1)] <- Gram[seq(3*d+1, d*d, d+1)] <- s[[4]][1:(d-3)] / (cs[1:(d-3)] * cs[4:d])
                GramSpec <- eigen(Gram[2:(d-1), 2:(d-1)], symmetric = TRUE)
                V <- diag(1, d)
                V0 <- GramSpec$vectors 
                specRatio <- GramSpec$values / GramSpec$values[d-2]
                V[-c(1, 2), -c(1, 2)] <-  t(t(V0) / sqrt(specRatio))
                colnames(V) <- colnames(model@basisEnv$basis[[smoothTerms[i]]])
                attr(model@basisEnv$basis[[smoothTerms[i]]], "V") <- V 
              }
            }            
            model
          }
)

computeKnots <- function(x, y, support, strategy = "log", ...) {
  if (is.numeric(strategy) & length(strategy) > 1) {
    knots <- strategy
    knots <- knots[knots > support[1] & knots < support[2]]
    knots <- c(support[1], unique(sort(knots)), support[2])
  } else {
    ## TODO: Implement this in C!?
    differences <- outer(x, y, '-')
    differences <- differences[differences > support[1] & differences < support[2]]
    differences <- unique(differences)
    ## TODO: Implement different strategies for "thinning". This one is taken from
    ## smooting.spline directly ...
    sknotl <- function(x, nk = "log")
    {
      ## if (!all.knots)
      ## return reasonable sized knot sequence for INcreasing x[]:
      n.kn <- function(n) {
        ## Number of inner knots
        if(n < 50L) n
        else trunc({
          a1 <- log( 50, 2)
          a2 <- log(100, 2)
          a3 <- log(140, 2)
          a4 <- log(200, 2)
          if	(n < 200L) 2^(a1+(a2-a1)*(n-50)/150)
          else if (n < 800L) 2^(a2+(a3-a2)*(n-200)/600)
          else if (n < 3200L)2^(a3+(a4-a3)*(n-800)/2400)
          else  200 + (n-3200)^0.2
        })
      }
      n <- length(x)
      if(isTRUE(nk == "log")) {
        nk <- n.kn(sqrt(n))  ## This is modified from 'smoothing.spline'
      } else if(isTRUE(nk == "all")) {
        nk <- n
      } 
      if(!is.numeric(nk)) 
        stop("'nknots' must be numeric <= n")
      if(nk > n)
        stop("Cannot use more inner knots than unique 'x' values.")
      inner <- x[seq.int(1, n, length.out= nk)]
      inner <- inner[-c(1, length(inner))]
      c(rep(x[1L], 4L), inner, rep(x[n], 4L))
    }
    knots <- sknotl(c(support[1], sort(differences), support[2]), nk = strategy) 
  }
  return(knots)
}

setMethod("computeSandwichKJ", "PointProcessModel",
          function(model, ...) {
            X <- getModelMatrix(model)
            eta <- predict(model)
            phi <- model@family@phi
            Dphi <- model@family@Dphi
            if (model@family@link == 'identity') {
              J <- getCrossProd(model)$G
              w <- Diagonal(x = eta * model@delta)
              K <- crossprod(model@V, crossprod(w %*% X, X)) %*% model@V
            } else {
              wX <- Diagonal(x = Dphi(eta)^2 * model@delta) %*% X
              J <- crossprod(model@V, crossprod(wX, X)) %*% model@V
              K <- crossprod(model@V, crossprod(Diagonal(x = phi(eta)) %*% wX, X)) %*% model@V
            }
            return(list(K = K, J = J))
          }
)

setMethod("computeLinearPredictor", "PointProcessSmooth",
          function(model, coefficients = NULL, ...) {
            if (is.null(coefficients)) 
              coefficients <- coefficients(model)
            
            as.numeric(getModelMatrix(model) %*% (model@V %*% coefficients))
          }
)

setMethod("computeMinusLogLikelihood", "PointProcessSmooth",
          function(model, coefficients = NULL, ...) {
            callNextMethod(model = model, coefficients = coefficients, 
                           fastIdentity = FALSE, ...)
          }
)

setMethod("computeQuadraticContrast", "PointProcessSmooth",
          function(model, coefficients = NULL,  ...) {
            callNextMethod(model = model, coefficients = coefficients, 
                           fastIdentity = FALSE, ...)
          }
)

setMethod("computeDMinusLogLikelihood", "PointProcessSmooth",
          function(model, coefficients = NULL, eta = NULL, ...) {
            dmll <- callNextMethod(model = model, coefficients = coefficients, 
                                   eta = eta, fastIdentity = FALSE, ...)
            as.numeric(dmll %*% model@V)
          }
)

setMethod("computeDDMinusLogLikelihood", "PointProcessSmooth",
          function(model, coefficients = NULL, eta = NULL, ...) {
            ddmll <- callNextMethod(model = model, coefficients = coefficients, 
                                   eta = eta, fastIdentity = FALSE, ...)
            as(crossprod(model@V, ddmll) %*% model@V, "matrix")
          }
)

setMethod("computeFisherInformation", "PointProcessSmooth",
          function(model, coefficients = NULL, eta = NULL, ...){
            J <- callNextMethod()
            as(crossprod(model@V, J) %*% model@V, "matrix")
          }
)

setMethod("getBasis", c(model = "PointProcessSmooth", term = "ANY"),
          function(model, term, rePar = FALSE, ...) {
            if (missing(term)) {
              basis <- model@basisEnv$basis
            } else {
              basis <- model@basisEnv$basis[[term]]
              if (rePar && !is.null(attr(basis, "V"))) 
                basis <- basis %*% attr(basis, "V")
            }
            basis
          }
)

setMethod("getSmoothTerms", "PointProcessSmooth",
          function(model, ...){
            model@smoothTerms
          }
)

## TODO: Implement update method.

setMethod("update", "PointProcessSmooth",
          function(object, ...) {
           message("No 'update' method currently implemented for a 'PointProcessSmooth' object.")
           object
          }          
)

setMethod("updateCrossProd", "PointProcessSmooth",
          function(model, subset = FALSE, ...) {
            test <- 'G' %in% names(model@crossProd)
            if(xor(test, subset))
              return(model)
            
            model <- callNextMethod()
            
            if (!test) {
              G <- model@crossProd$G
              G <- crossprod(model@V, G) %*% model@V
              response <- model@responseMatrix %*% model@V
              model@crossProd <-  list(G = G, response = colSums(response))
            }
            return(model)
          }
)

## TODO: New summary function for an object of class 'PointProcessSmooth'.

setMethod("summary", "PointProcessSmooth",
          function(object,...) {
            callNextMethod()
          }
)