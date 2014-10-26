\name{tExp}
\alias{tExp}
\title{Truncated Exponential}
\description{
	The exponential function truncated to 0 for small values.
}
\usage{
tExp(x, tLevel = 1e-04)
}
\arguments{
  \item{x}{Argument for the exponential function}
  \item{tLevel}{Truncation level}
}
\details{
	Truncation can be useful in combination with basis function
	expansions where there is a need to store a large number of
	evaluations. Truncation of small values to 0 allow for
	efficient storage using e.g. sparse matrices. 
}
\value{
  \eqn{\exp(x)}{exp(x)} if \eqn{\exp(x) \geq \code{tLevel}}{exp(x) >= \code{tLevel}} and 0
  if \eqn{\exp(x) < \code{tLevel}}{exp(x) < \code{tLevel}}.    
}
\author{Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}}

\seealso{\code{\link{exp}}}
\examples{
tExp(-1)
tExp(-10)
}

\keyword{math}
