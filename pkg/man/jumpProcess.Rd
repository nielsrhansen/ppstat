\name{jumpProcess}
\alias{jumpProcess}
\alias{jumpProcess,MarkedPointProcess-method}
\alias{jumpProcess,data.frame-method}

\title{jumpProcess
}
\description{
Creates an object of class \code{JumpProcess}. 
}
\usage{

\S4method{jumpProcess}{MarkedPointProcess} (object, ...)

\S4method{jumpProcess}{data.frame} (object, continuousData, ...)

}
\arguments{
  \item{object}{ a \code{MarkedPointProcess} object or a \code{data.frame}.}

  \item{continuousData}{ a \code{data.frame}.}

  \item{\dots}{ other arguments.}
 
}

\value{

  An object of class \code{JumpProcess}
  
}

\author{
  Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}
  }

\seealso{
\code{\linkS4class{MarkedPointProcess}}  
}
\examples{

## A pure jump process.
MP <- data.frame(time = seq(5, 95, 10),
                 markType = rep("foo", 10),
                 jump = rnorm(10))
JP <- integrator(markedPointProcess(MP, 1:100))

\dontrun{
plot(JP)}

## A 'jump target' process (target = 0).
CP <- data.frame(
 time = 1:100,
 foo = unlist(tapply(rnorm(100), rep(0:10, times = c(4, rep(10,9), 6)), cumsum))
)

MP$jump <-  CP$foo[seq(5, 95, 10)]
JP <- jumpProcess(MP, CP)

\dontrun{
plot(JP)}


## Another combined process.
MP$jump <- rnorm(10, sd = 2)
CP$foo <- CP$foo + integrator(markedPointProcess(MP, 1:100), result = 'numeric')
JP <- jumpProcess(MP, CP)

\dontrun{
plot(JP)}

}
\keyword{manip}
