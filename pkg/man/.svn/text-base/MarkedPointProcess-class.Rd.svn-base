\name{MarkedPointProcess-class}
\docType{class}
\alias{MarkedPointProcess-class}
\alias{dim,MarkedPointProcess-method}
\alias{colNames}
\alias{colNames<-}
\alias{colNames,MarkedPointProcess,missing-method}
\alias{colNames,MarkedPointProcess,character-method}
\alias{getPointId,MarkedPointProcess-method}
\alias{getPointId}
\alias{getMarkType,MarkedPointProcess-method}
\alias{getMarkType}
\alias{getMarkValue,MarkedPointProcess-method}
\alias{getMarkValue}
\alias{getPointPointer}
\alias{getPointPointer,MarkedPointProcess,missing-method}
\alias{getPointPointer,MarkedPointProcess,character-method}
\alias{getPointPosition,MarkedPointProcess-method}
\alias{getPointPosition}
\alias{getPointTime,MarkedPointProcess-method}
\alias{getPointTime}

\title{Class "MarkedPointProcess"}
\description{	

An object of class \code{MarkedPointProcess} is a container for data
obtained as observations of a marked point process or combinations of a
continuous time process and a marked point process. 

}
\section{Objects from the Class}{
Objects can be created by calls of the function \code{markedPointProcess}.

The object contains time points or positions in one-dimensional space of
either a marked point process, that is, discrete events and their
associated mark information, or a combination of point process data and
continuous process data.  

The class extends \code{ContinuousProcess} and is extended by the class
  \code{JumpProcess}. These classes form an infrastructure for storing and handling general stochastic
  process data. 

}
\section{Slots}{
  \describe{

    \item{\code{markColNames}:}{a \code{character}.}

    \item{\code{markValueColNames}:}{a \code{character}.}
    
    \item{\code{iPointSubset}}{ an \code{integer} vector.}

    \item{\code{jPointSubset}}{ an \code{integer} vector.}

    \item{\code{pointPointer}}{ an \code{integer} vector.}

    \item{\code{pointProcessEnv}}{ an \code{environment}.}
    
  }
}
\section{Methods}{
  \describe{

    \item{colNames}{\code{signature(object = "MarkedPointProcess", type
       = "character")}: Returns the column names if \code{type} is
       missing. If \code{type} is not missing, it must be either
       \code{'unit'}, \code{'factor'}, \code{'numeric'}, \code{'mark'}
       or \code{'markValue'} resulting in the column names for the
       specific column type only.}
   
     \item{dim}{\code{signature(x = "MarkedPointProcess")}: Returns the
       dimensions.}

    \item{getPointId}{\code{signature(object = "MarkedPointProcess")}:
      Returns the values of the unit identification variable.}
  
    \item{getMarkType}{\code{signature(object = "MarkedPointProcess")}:
      Returns the discrete marks.}
    
    \item{getMarkValue}{\code{signature(object = "MarkedPointProcess")}:
      Returns additional mark values.}
    
    \item{getPointPosition}{\code{signature(object =
	"MarkedPointProcess")}: Returns the time/position,
	\code{position}, of the observed discrete events.}
      
    \item{getPointTime}{\code{signature(object =
	"MarkedPointProcess")}: Same as \code{getPointPosition}.}
    
    \item{getPointPointer}{\code{signature(object =
	"MarkedPointProcess", mark = "character")}: Returns the pointer
	to the point positions in the continuous data.}
  }
  Other methods are documented in their own help pages.
}
              
\author{Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}}

\seealso{
  
  \code{\link{markedPointProcess}}, 
  \code{\linkS4class{ProcessData}},
  \code{\linkS4class{ContinuousProcess}},
  \code{\linkS4class{JumpProcess}}
}

\examples{
showClass("MarkedPointProcess")
showMethods(classes = "MarkedPointProcess")
}
\keyword{classes}
