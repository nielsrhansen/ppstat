\name{continuousProcess}
\alias{continuousProcess}
\alias{continuousProcess,list-method}
\alias{continuousProcess,data.frame-method}
\alias{continuousProcess,factor.frame-method}
\alias{continuousProcess,numeric-method}
\alias{continuousProcess,matrix-method}
\alias{continuousProcess,ContinuousProcess-method}
\title{Continuous Process}
\description{
  Creates a \code{ContinuousProcess} object from 
  data frame(s).
}
\usage{
\S4method{continuousProcess}{list}(continuousData, unitData = data.frame(),
  metaData = list(), positionVar = 'time', idVar = 'id', equiDistance = 0, ...)

\S4method{continuousProcess}{data.frame}(continuousData, unitData = data.frame(),
  metaData = list(), positionVar = 'time', idVar = 'id', equiDistance = 0, ...)

\S4method{continuousProcess}{factor.frame}(continuousData, unitData = data.frame(),
  metaData = list(), positionVar = 'time', idVar = 'id', valueVar = 'value',
  variableVar = 'variable', equiDistance = 0, ...)

\S4method{continuousProcess}{numeric}(continuousData, unitData = data.frame(),
  metaData = list(), positionVar = 'time', idVar = 'id', equiDistance = 0, ...)

\S4method{continuousProcess}{matrix}(continuousData, unitData = data.frame(),
  metaData = list(), positionVar = 'time', idVar = 'id', equiDistance = 0, ...)

\S4method{continuousProcess}{ContinuousProcess}(continuousData, ...)
}
\arguments{
  
  \item{continuousData}{ a data frame containing the process data. An
    optional column with name \code{positionVar} (default \code{'time'})
    specifies the x-coordinates where we have observations, and an
    optional column with name \code{idVar} (default \code{'id'}) specifies the
    identification variable for the division of the data into units.
    Additional columns must be numeric and are taken as the coordinates
    of observations of the process.}

  \item{unitData}{an optional data frame containing further unit
    specific variables.}

  \item{metaData}{an unstructured \code{list} containing additional
    information on the data in the resulting \code{ContinuousProcess}
    object.}

  \item{positionVar}{a \code{character} specifying the name of the
    variable holding the x-coordinates for the observations. Default
    value \code{'time'}.}

  \item{idVar}{a \code{character} specifying the name of the
    identification variable for each unit. Default value \code{'id'}.}

  \item{valueVar}{a \code{character}. Default value \code{'value'}.}

  \item{variableVar}{a \code{character}. Default value \code{'variable'}.}
  
  \item{equiDistance}{a positive \code{numeric} specifying the distance
    between the observations if the observations are
    equidistant. Alternatively, setting \code{equiDistance = 'auto'}
    implies that the constructor determines if the observations are
    equidistant and computes the value of \code{equiDistance}. The default value of 0
    implies that the observations are treated as non-equidistant.}

  \item{...}{ other arguments.}

}

\value{ An object of class \code{ContinuousProcess}.  }

\details{

  If the data frame \code{continuousData} lacks either of the
  \code{positionVar} or \code{idVar} columns they are
  automatically generated. If \code{positionVar} is missing the
  observations are assumed equidistant at time points 1, 2 etc within
  each unit. If \code{idVar} is missing it is assumed that there is only
  one unit. 
  
}

\author{
  Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}
  }

\seealso{
\code{\linkS4class{ContinuousProcess}}  
}
\examples{

CP <- data.frame(time = 1:10, value = rnorm(10))
continuousProcess(CP)

CP <- data.frame(id = c(rep("A",5), rep("B", 5)), value = rnorm(10))
continuousProcess(CP)

}
\keyword{manip}
