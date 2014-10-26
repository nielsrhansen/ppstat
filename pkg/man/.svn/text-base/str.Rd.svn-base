\name{str}
\alias{str,ProcessData-method}
\alias{str,ContinuousProcess-method}
\alias{str,MarkedPointProcess-method}
\title{
  Compact display of the structure of a process data object.
}
\description{
  This method extends the \code{str} function from the \pkg{utils}
  package to display the complete structure of a process data object.
  Data are stored in environments for those objects, and this method
  lists the content of the environments in addition to other slots in
  the object. 
}
\usage{
\S4method{str}{ProcessData}(object, ...)
\S4method{str}{ContinuousProcess}(object, ...)
\S4method{str}{MarkedPointProcess}(object, ...)
}
\arguments{
  \item{object}{ a process object.}
  \item{\dots}{ additional arguments.}
}
\details{
  The method was implemented to provide more meaningful structure
  information for process data objects. The default
  method reports the environment reference for environment slots only,
  but not the the content of the environments.

  To get the classical structure information for process data objects
  call \code{utils:::str.default} instead, see the examples.  

}
\value{
  \code{str} does not return anything, but prints out the structure
  information for the object. 
}

\author{
  Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}.
}

\seealso{
\code{\link{str}}
}
\examples{
data(example)
str(contExam)
str(pointExam)
utils:::str.default(pointExam)
}

\keyword{utilities}
