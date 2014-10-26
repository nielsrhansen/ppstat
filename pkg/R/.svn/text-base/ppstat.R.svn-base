###======================================================================
### ppstat - point process statistics
###======================================================================

### Setting up an environment for ppstat to hold global variables, in
### particular registration of parallel backends. Default registration
### is no parallel backend, that is, lapplyParallel is just lapply.

## Environment for globals used by the ppstat package
.ppstatGlobals <- new.env(parent = .GlobalEnv)
lapplyParallel <- function(...) .ppstatGlobals$lapplyParallel(...)
  
## bSpline replaces bs for convenience and provides easier
## computations of B-spline bases. Does not issue a warning when
## evaluated outside of knots. This will in fact be the rule rather
## than the exception for point process usages.

bSpline <- function(x, knots, ..., sym = FALSE, trunc = NULL, levels = NULL) {
  if(is.null(levels)) {
    if(length(knots) <= 4) 
      stop("Needs at least 5 knots.")
    if(sym) {
      design <- list()
      
      if(any(x>=0))
        design[[1]] <- bSpline(x = x[x>=0], knots = knots, ..., trunc = NULL, sym = FALSE)
      
      if(any(x < 0))
        design[[2]] <- bSpline(x = -x[x<0], knots = knots, ..., trunc = NULL, sym = FALSE)
      design <- do.call("rbind", design)
    } else {
      design <- splineDesign(knots, x, ..., outer.ok=TRUE)
    }
    if(!is.null(trunc)) {
      if(length(trunc) == 1) {
        design[x <= trunc, ] <- 0
      } else if(length(trunc) == 2) {
        design[x <= trunc[1] | x > trunc[2], ] <- 0
      } else {
        stop("The truncation argument 'trunc' must be either a single numeric or a vector of length 2.")
      }
    }
    design <- design[ , apply(design, 2, function(s) any(s != 0))]
  } else {
    if(!is.numeric(levels) || levels < 1) 
      stop("For multiscale modeling, 'levels' must be an integer larger than or equal to 1.")
    nrKnots <- 2^levels * 4 + 1
    lower <- knots[1]
    upper <- knots[length(knots)]
    if(length(knots) == 1)
      lower <- 0
    knots <- seq(lower, upper, length.out = nrKnots)
    design <- list()
    kn1 <- knots[seq(1, nrKnots, 2^levels)]
    kn1 <- c(3 * kn1[1] - 2 * kn1[2], 2 * kn1[1] - kn1[2], kn1)
    kn2 <- knots[seq(1 + 2^(levels - 1), nrKnots, 2^levels)]
    kn2 <- c(3 * kn2[1] - 2 * kn2[2], 2 * kn2[1] - kn2[2], kn2)
    design[[1]] <- cbind(bSpline(x, knots = kn1, ...,
                                 trunc = NULL, sym = FALSE),
                         bSpline(x, knots = kn2, ...,
                                     trunc = NULL, sym = FALSE)
                         )
    if(levels > 1) {
      for (k in seq(2, levels)) {
        kn = knots[seq(1 + 2^(levels - k), nrKnots, 2^(levels - k + 1))]
        kn <- c(2 * kn[1] - kn[2], 2 * kn[1] - kn[2], kn)
        design[[k]] <- bSpline(x, knots = kn, ...,
                                   trunc = NULL, sym = FALSE)
      }
    }    
    design <- do.call("cbind", design)
  }
  return(design)
}

## Truncated exponential

tExp <- function(x, tLevel = 1e-4) {
  y <- exp(x)
  y[y < tLevel] <- 0
  return(y)
}

## Safe logarithm

safeLog <- function(x) {
  x[x < 0] <- 0
  log(x)
}

## Constant and affine functions

const <- function(x, y, c = 1){
  if(missing(y)) {
    lesseq <- rep(1, length(x))
  } else {
    lesseq <- (x <= y)
  }
  c * as.numeric(lesseq)
}

affine <- function(x, a = 1, b = 1) {
    matrix(c(b * rep(1, length(x)), a * x), ncol = 2)
}

### TODO: This should be changed to an S4 method and specialized to the
### different point process classes.

print.summary.ppm <-  function (x, digits = max(3, getOption("digits") - 3), 
    signif.stars = getOption("show.signif.stars"), ...) 
{
    cat("\nCall:\n")
    cat(paste(deparse(x$call), sep = "\n", collapse = "\n"), 
        "\n\n", sep = "")
    cat("\nCoefficients:\n")
    coefs <- x$coefficients
    printCoefmat(coefs, digits = digits, signif.stars = signif.stars, 
            na.print = "NA", ...)
    cat("\nMinus-log-likelihood: ", format(x$mll, digits = max(4, digits + 1)), " on ",x$df, " degrees of freedom",
        "\n", sep = "")
    cat("AIC: ", format(x$aic, digits = max(4, digits + 1)), 
        "\n\n", "Number of function evaluations: ", x$iter[1],
        "\n","Number of gradient evaluations: ", x$iter[2],
        "\n", sep = "")
    if(is.na(x$convergence)) {
      cat("\nNote: Model has not been fitted yet.")
    } else if(x$convergence != 0) {
      cat("\nWarning: Algorithm did not converge.\n         'optim' convergence status:", x$convergence,"\n")
    }
    cat("\n")
    invisible(x)
}

## Special functions used to construct model matrices

.__s__ <- function(t) cbind(1, t)
.__k__ <- function(t) t

