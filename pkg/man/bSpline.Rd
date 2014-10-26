\name{bSpline}
\alias{bSpline}

\title{B-Splines bases}
\description{
  \code{bSpline} computes the B-spline basis matrix.
}
\usage{
bSpline(x, knots, ..., sym = FALSE, trunc = NULL, levels = NULL)
}

\arguments{
  
  \item{x}{a \code{numeric} vector. The predictor variable.}
  
  \item{knots}{a \code{numeric} vector. The knots or breakpoints that
    define the spline basis. If \code{levels} is not \code{NULL} this argument
    specifies the range of the knots for a multiscale B-spline basis.}

  \item{sym}{a \code{logical}. Should the basis functions be symmetric
  around 0. Default value is \code{FALSE}.}
  
  \item{trunc}{a \code{numeric} of length 1 or 2. The spline basis is
    truncated below \code{trunc} or to the interval from \code{trunc[1]}
    to \code{trunc[2]} if \code{trunc} has length 2. Default value is
    \code{NULL}, which means no truncation.}
    
  \item{levels}{a positive \code{integer} specifying the number 
    of levels in a multiscale model using splines. Default value is \code{NULL}, 
    which means no multiscale modeling.
  }
  
  \item{\dots}{additional arguments passed to \code{splineDesign}, for
    instance, \code{ord} which gives the order of the spline functions,
    see \code{\link{splineDesign}}.}
  
}
\details{
  
  \code{bSpline} is a convenience wrapper function for
  \code{splineDesign} as an alternative to \code{bs}. It has defaults
  and some useful parameters suitable for formula specification of
  linear filter function bases when using
  \code{\link{pointProcessModel}}.
  
  The function also provides a computation of a multiscale 
  B-spline basis using the \code{levels} argument. If \code{levels} is 
  not \code{NULL} a sequence of bases are used with different scales. 
  Only the range of the \code{knots} argument is used. With \eqn{n} levels 
  the total number of equidistant knots is \eqn{2^{n} \times 4 + 2 \times n + 3}{2^n * 4 + 2 * n + 1}.
  The \eqn{k}'th level (for \eqn{k > 1}) uses every \eqn{2^{n-k+1}}{2^(n-k+1)} 
  knot. 
  
  
}
\value{
  
  A matrix of basis function evaluations. Zero-columns returned from
  \code{splineDesign} are discarded.

} \author{Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}}

\seealso{\code{\link{splineDesign}}, \code{\link{bs}}, \code{\link{pointProcessModel}}}
\examples{
bSpline(seq(1,10), 2:8)
bSpline(seq(-10,10), 2:8, sym = TRUE)
bSpline(seq(-10,10), seq(-10,10,2), trunc = c(-4,4))
}
\keyword{math}
