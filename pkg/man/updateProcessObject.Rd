\name{updateProcessObject}
\alias{updateProcessObject}
\alias{updateProcessObject,ContinuousProcess-method}
\title{
  Functions to update old objects with new slots.
}

\description{
  
This function updates process object from the \pkg{processdata} package
to have all required slots.

}

\usage{

updateProcessObject(object, ...)

}

\arguments{

  \item{object}{ an object of class \code{ContinuousProcess} or \code{MarkedPointProcess}.}

  \item{\dots}{ other arguments.}

}

\details{

  This is a convenience function for updating objects created with an
  older version of \pkg{processdata} to have all required slots. An
  alternative, and recommend, approach is to create the objects from
  scratch. This function simply fills in any missing slots with default
  values. 

  }

  \value{
An object of class  \code{ContinuousProcess} or \code{MarkedPointProcess}.
}

\author{
  Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}
}

\seealso{
\code{\linkS4class{MarkedPointProcess}},  
\code{\linkS4class{ContinuousProcess}}, 
\code{\link{continuousProcess}},
\code{\link{markedPointProcess}}.    
}

\keyword{ manip }
