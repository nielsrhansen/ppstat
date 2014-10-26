\name{integrator}
\alias{integrator}
\alias{integrator,MarkedPointProcess-method}

\title{Integrator of a marked point process}

\description{
  
  Computes the integral with respect to a marked point
  process. Either the integrated process with jump sizes as weights is
  computed, or the integrated process with respect to the counting process
  is computed.  
  
}
\usage{

\S4method{integrator}{MarkedPointProcess}(object, f = 1, jumpVar = '', result = 'JumpProcess', \dots)


}
\arguments{

  \item{object}{a \code{MarkedPointProcess} object.}
  
  \item{f}{ a \code{function} or \code{numeric} vector. The integrand,
    which is integrated with respect to the marked point process. If
    \code{f} is a function it needs to be vectorized in its first
    argument. The default value is \code{1}, meaning a constant
    integrand. }

  \item{jumpVar}{ a \code{character}. If not equal to one of the column
    names of \code{getMarkValue(object)} the counting process for
    \code{mark} is computed. The default value is \code{''}.}
  
  \item{result}{ a \code{character}. By default, the function returns an
    object of class \code{JumpProcess}. If \code{result = 'numeric'} it
    returns instead the integrated process as a numeric vector.}
    
  \item{\dots}{ other arguments passed to \code{f}.}
 
}

\value{

  A \code{JumpProcess} object or a numeric vector. Note that some information is
  lost when returning just a numeric, e.g. the unit identifications.
}

\details{
  The integral computed is with respect to all points in the marked
  point process. If the integral with respect to the process having only
  a subset of the marks is needed, use the \code{subset} function on the object to
  select the appropriate subset of marks.

  It is possible to integrate functions that depend upon marks or mark
  values. This can be done either by precomputing the function
  evaluations, or by specifying \code{f} to take more than one argument,
  so that it works correctly as a vectorized function, and supply the
  additional arguments in the call of \code{integrator}.
  
  
}

\author{
  Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}.
  }

\seealso{
\code{\link{jumpProcess}}, \code{\linkS4class{MarkedPointProcess}}, \code{\linkS4class{JumpProcess}}, \code{\link{subset}}.  

}
\examples{

MP <- data.frame(time = seq(5, 95, 10),
                 markType = rep("foo", 10),
                 jump = rnorm(10))

\dontrun{## The counting process only
plot(integrator(markedPointProcess(MP, 1:100)))

## The integrated pure jump process.
plot(integrator(markedPointProcess(MP, 1:100), jumpVar = 'jump'))

## With an integrand
plot(integrator(markedPointProcess(MP, 1:100), f = cos))
}
}
\keyword{manip}
