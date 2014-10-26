\name{ToyData}
\alias{toyData}
\docType{data}
\title{
A small toy data set.
}
\description{

  A small, artificial data set used for illustrations and testing.  

}
\usage{data(toyData)}
\format{ An object of class \code{\linkS4class{MarkedPointProcess}}
 
}
\details{

  The \code{MarkedPointProcess} object holds 10 replications of a bivariate
  point process with points named \code{ALPHA} and \code{BETA}. 
  
}
\source{

  The data are simulated. \code{ALPHA} points are generated as a
  homogeneous Poisson process time-transformed using the square root
  transformation. \code{BETA} points are uniformly distributed in
  intervals of length 1 after a random subsample of the \code{ALPHA}
  points. 
  

}
\seealso{\code{\linkS4class{MarkedPointProcess}}}
\examples{
data(toyData)
\dontrun{
plot(toyData)}
}
\keyword{datasets}
