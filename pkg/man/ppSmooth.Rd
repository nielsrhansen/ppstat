\name{ppSmooth}
\alias{ppSmooth}

\title{
  Generalized linear point process modeling using smoothing splines. 
}
\description{

  The function \code{ppSmooth} fits a generalized linear point process model
  based on a B-spline basis expansion of smooth terms.  
}
\usage{
ppSmooth(formula, data, family, support = 1, knots = 'log', N = 200, Delta, lambda, 
  coefficients, fit = TRUE, varMethod = 'Fisher', ...)
}
\arguments{
    \item{formula}{an object of class \code{\link{formula}}. A symbolic
    description of the model to be fitted. Smooth terms are treated in 
    a special way and other terms are treated as in \code{\link{pointProcessModel}}.
    See \sQuote{Details}.}

  \item{data}{an object of class \code{\linkS4class{MarkedPointProcess}} containing
    the point process data as well as any continuous process data.}

  \item{family}{an object of class
  \code{\linkS4class{Family}}. Specification of the general model
  family containing the specification of the \eqn{\phi}{phi} function,
  which links the linear predictor process to the predictable intensity process.}
  
  \item{support}{a \code{numeric} vector. Specifies the support of the
    filter functions as the interval from \code{support[1]} to
    \code{support[2]}. If \code{support} is of length 1 the support is the
    interval from 0 to \code{support[1]}. The default value is 1.}

  \item{knots}{a \code{character} or \code{numeric}. The number or value of knots to use.
  The default value, \code{'log'}, means that an algorithm similar to 
  the one in \code{smoothing.spline} is used to adaptively select the number of knots. 
  Other possible values are \code{'all'} (not recommended), an \code{integer} 
  specifying the precise number of adaptive knots that are used, or a numeric vector 
  of length 2 or more specifying the knots in a non-adaptive way. 
  }

  \item{N}{a \code{numeric}. The number of basis function evaluations
    used in the support. Default value 200.}

  \item{Delta}{a \code{numeric}. Basis functions are evaluated at
    \code{Delta}-grid values in the support. If missing, \code{Delta} is
    set to the length of the support divided by \code{N}. If specified,
    overrides the use of \code{N}.}
    
  \item{lambda}{a \code{numeric}. The parameter that controls the
  amount of penalization. }
  
  \item{coefficients}{an optional specification of the initial parameters
    used for the numerical optimization.}

  \item{fit}{a \code{logical}. Specifies if the model is to be
    fitted. Default is \code{TRUE}.}
  
  \item{varMethod}{a \code{character}. Specifies the method used for
    estimation of the variance matrix of the parameter
    estimators. Currently the default value, \code{'Fisher'}, and
    \code{'none'} are implemented. Using \code{'Fisher'} a sandwich estimator 
    based on the Fisher information is used, whereas \code{'none'}
    means that the variance matrix is not estimated.}

  \item{\dots}{additional parameters that are passed on to 
  \code{ppmFit}.}
}
\details{

  \code{ppSmooth} preprocesses the formula and extracts the smooth terms before 
  a \code{PointProcessModel} is created by a call to \code{pointProcessModel}.
  Terms of the form \code{s(.)} in the formula are treated as special terms.
  They are replaced by an automatic basis expansion in terms
  of B-splines, and the corresponding parameters are penalized using 
  the standard integral of the square of the second derivative to ensure smoothness. 
  
  Though the basis expansion is in terms of B-splines, there is an internal 
  reparametrization in terms of orthogonal basis components, which results
  in the smoothness penalty being the ordinary Euclidean norm times \code{lambda}.
  
  The \code{knots} argument determines how many knots are used. The locations of the 
  knots are determined by the quantiles for the distribution of interdistances between 
  points from the response and points from the terms in the formula. 
  Each term uses its own set of knots. 
  
  All the B-spline basis functions have support within the support interval and are
  0 on the boundary. A constant and linear a function are added to the set of B-spline basis 
  functions, whose coefficients are not penalized by default. 
  
  The sandwich estimator depends on the amount of penalization. If the fit is 
  oversmoothed, and thus biased, the resulting confidence intervals on the 
  filter functions are most likely misleading.  

}
\value{
  The function \code{ppSmooth} returns an object of class
  \code{\linkS4class{PointProcessSmooth}}, which is an extension of
  \code{PointProcessModel}. 
}
\note{
  The method does not yet support automatic data adaptive selection of \code{lambda}. 
  An information quantity (which in this case is TIC) can be extracted 
  using \code{getInformation}. This quantity can be minimized over at grid for selection
  of \code{lambda}. 
}
\author{
Niels Richard Hansen \email{Niels.R.Hansen@math.ku.dk}.
}
\seealso{
\code{\linkS4class{PointProcessModel}}, \code{\link{pointProcessModel}}, \code{\link{ppKernel}}.
}

