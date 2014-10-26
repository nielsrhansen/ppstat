\name{summary}
\alias{summary,PointProcessModel-method}
\alias{summary,PointProcessSmooth-method}
\alias{summary,PointProcessKernel-method}
\alias{print.summary.glppm}
\title{summary}
\description{
  
  Computes and formats a summary of a fitted point process model constructed with
  e.g. \code{pointProcessModel} or \code{pointProcessSmooth}.

}
\usage{
\S4method{summary}{PointProcessModel}(object, ...)
\S4method{summary}{PointProcessSmooth}(object, ...)
\S4method{summary}{PointProcessKernel}(object, ...)

\S3method{print}{summary.glppm}(x, digits = max(3, getOption("digits") - 3), 
    signif.stars = getOption("show.signif.stars"), ...)
}
\arguments{
  \item{object}{an object of class \code{"PointProcessModel"}}
  \item{x}{a list of class \code{"summary.glppm"}, usually the result of a
    call of \code{summary} on a \code{"PointProcessModel"} object.}
  \item{digits}{the number of significant digits to use when printing.}
  \item{signif.stars}{logical. If \code{TRUE}, 'significance stars' are printed for
    each coefficient.}
  \item{...}{additional arguments to the call.}
}
\value{
  The S4 method \command{summary} returns a list of S3-class
  \code{"summary.glppm"}. It contains the entries
  \item{df}{degrees of freedom or effective degrees of freedom.}
  \item{call}{the call used to generate the \code{PointProcessModel}
    object.}
  \item{mll}{the value of the minus-log-likelihood function in the
    estimated parameters.}
  \item{iter}{the number of function and gradient evaluations used in
    the numerical optimization.}
  \item{aic}{the AIC model selection criteria defined in this case as
    \code{2*(mll + df)}.}
  \item{coefficients}{a px4 matrix with the columns being the
    estimated parameters, estimated standard errors, z statistics and
    corresponding two-sided p-values.}
}
\details{
  
  In the current implementation the degrees of freedom, \code{df}, is simply equal
  to the number of parameters. Future extensions will offer the
  possibility of L2 and L1 regularized fits and a corresponding
  computation of the effective degrees of freedom useful for model
  selection that differs from the actual number of parameters. 
  
}

\seealso{
  \code{\linkS4class{PointProcessModel}}
}

\author{Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}}
\keyword{print}
