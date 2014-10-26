\name{computeBasis}
\alias{computeBasis}
\alias{computeBasis-method}
\alias{computeBasis,PointProcessModel,ANY-method}
\title{
Computes function evaluations
}
\description{

  This function computes function evaluations and stores them in a
  given environment.  
  
}
\usage{
\S4method{computeBasis}{PointProcessModel,ANY}(model, form, ...)
}
\arguments{
  \item{model}{an object of class \code{"PointProcessModel"}.}

  \item{form}{a S3-class \code{terms} object. Usually a term obtained from a
    formula, which gives the function to be evaluated.
    Modified into the formula \code{~ term - 1} before computation.}

  \item{...}{ other arguments.}

}
\details{

  The function is called when building the model matrix for an object of
  class \code{"PointProcessModel"}.

  The function checks if the environment in \code{model} holding the basis
  evaluations is locked, and if it is, if the basis evaluations for the
  \code{form} are present. If so, the function returns \code{TRUE}. It is an
  error if the term is not present (should not happen, an error message
  is given). Otherwise the basis evaluations are computed and stored
  in the appropriate environment in the model. 
  
}
\value{
A \code{"logical"}, which is \code{TRUE} if the basis function
evaluations are computed as they need to be.

This function is called for its side effect of computing and storing the basis
evaluations in the environment. If the environment is locked the
evaluations are not recomputed.

}

\author{
Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}
}

\seealso{\code{\linkS4class{PointProcessModel}}
}

\keyword{math}
