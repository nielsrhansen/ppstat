\name{markedPointProcess}
\alias{markedPointProcess}
\alias{markedPointProcess,data.frame,ContinuousProcess-method}
\alias{markedPointProcess,data.frame,data.frame-method}
\alias{markedPointProcess,data.frame,factor.frame-method}
\alias{markedPointProcess,data.frame,vector-method}
\alias{markedPointProcess,data.frame,missing-method}
\alias{markedPointProcess,data.frame,ANY-method}
\alias{markedPointProcess,vector,ANY-method}
\alias{markedPointProcess,vector,vector-method}
\alias{markedPointProcess,vector,data.frame-method}
\alias{markedPointProcess,data.frame-method}
\alias{markedPointProcess,MarkedPointProcess-method}
\alias{markedPointProcess,MarkedPointProcess,missing-method}

\title{Marked Point Process}
\description{
  Creates a \code{MarkedPointProcess} object from 
  data frame(s).
}
\usage{
\S4method{markedPointProcess}{data.frame,ContinuousProcess}(pointData,
continuousData, markVar = 'markType', coarsen = NULL, ...)

\S4method{markedPointProcess}{data.frame,data.frame}(pointData, continuousData, ...,
markVar = 'markType', coarsen = NULL)

\S4method{markedPointProcess}{data.frame,vector}(pointData, continuousData,
positionVar = 'time', idVar = 'id', markVar = 'markType', ...)

\S4method{markedPointProcess}{vector,data.frame}(pointData, continuousData,
positionVar = 'time', idVar = 'id', markVar = 'markType', ...)

\S4method{markedPointProcess}{vector,vector}(pointData, continuousData, ...)


\S4method{markedPointProcess}{data.frame}(pointData, positionVar = 'time',
idVar = 'id', markVar = 'markType', ...)

\S4method{markedPointProcess}{MarkedPointProcess}(pointData, ...)
}
\arguments{

  \item{pointData}{ a \code{data.frame} containing the point process data. It
  must have a \code{numeric} column named \code{positionVar} and
  optional \code{factor} columns named \code{markVar} and
  \code{idVar}. Additional columns are allowed and stored as further
  mark values for each point.}
  
  \item{continuousData}{ a \code{ContinuousProces} object or a data
    frame containing the continuous process data. Alternatively, a
    vector giving an observation window and grid for each unit.}

  \item{positionVar}{ a \code{character} specifying the name of the
    variable holding the x-coordinates for the observations. Default
    value \code{'time'}.}

  \item{idVar}{ a \code{character} specifying the name of the
    identification variable for the units. Default value \code{'id'}.}

  \item{markVar}{ a \code{character} specifying the name of the
    mark variable. Default value \code{'markType'}.}

  \item{coarsen}{ a \code{character} or \code{NULL}. Either
  \code{'left'} or \code{'right'} for coarsing the event points to the
  observation grid, or \code{NULL} (the default) for no coarsing. }
  
  \item{...}{ further arguments passed to \code{continuousProcess}.} 
}

\value{ An object of class \code{MarkedPointProcess}.  }

\details{

  If the data frame \code{pointData} lacks either of the \code{markVar}
  or \code{idVar} columns they are automatically generated. If
  \code{markVar} is missing it is assumed that there is only one type of marks. If
  \code{idVar} is missing it is assumed that there is only one unit.

  If \code{coarsen} is not \code{NULL} the event points are coarsed to
  the observation grid of the continuous process. Otherwise the
  observation grid will be enlarged to include the event points and
  continuous process data will be interpolated. 

  The constructor with signature ("data.frame", "data.frame") is a wrapper for
  
  \code{markedPointProcess(pointData, continuousProcess(continuousData, ...),
    markVar = 'markType', ...)}. 
}

\author{
  Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}
  }

\seealso{
\code{\linkS4class{MarkedPointProcess}},  
\code{\linkS4class{ContinuousProcess}}, 
\code{\link{continuousProcess}}.  
}
\examples{

MPP <- data.frame(time = cumsum(rexp(18, 2)))
markedPointProcess(MPP)

CP <- data.frame(id = rep("A",10), time = seq(0.1, 10, 0.1), value = rnorm(100))
markedPointProcess(MPP, CP)

}
\keyword{manip}
