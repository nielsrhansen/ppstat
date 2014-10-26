\name{ContinuousProcess-class}
\docType{class}
\alias{ContinuousProcess-class}
\alias{dim,ContinuousProcess-method}
\alias{colNames,ContinuousProcess,missing-method}
\alias{colNames,ContinuousProcess,character-method}
\alias{getId,ContinuousProcess-method}
\alias{getId}
\alias{getEquiDistance}
\alias{getEquiDistance,ContinuousProcess-method}
\alias{getPosition}
\alias{getPosition,ContinuousProcess-method}
\alias{getTime}
\alias{getTime,ContinuousProcess-method}
\alias{getValue,ContinuousProcess-method}
\alias{getValue}
\alias{getFactors,ContinuousProcess-method}
\alias{getFactors}
\alias{getNumerics,ContinuousProcess-method}
\alias{getNumerics}
\alias{getUnitData,ContinuousProcess-method}
\alias{getUnitData}
\alias{show,ContinuousProcess-method}
\alias{summary,ContinuousProcess-method}

\title{Class "ContinuousProcess"}
\description{	
  
An object of class \code{ContinuousProcess} is a container for data
obtained as discrete observations of univariate or multivariate continuous 
time processes. 

}
\section{Objects from the Class}{
  Objects can be created by calls of the function \code{continuousProcess}.
  
  The object contains time points or positions in one-dimensional space 
  and corresponding observations of a univariate or multivariate
  process. The data structure can hold data for several units, or
  individuals.  

  The class extends \code{ProcessData} and is extended by the classes
  \code{MarkedPointProcess} and \code{JumpProcess}. These classes form
  an infrastructure for storing and handling general stochastic
  process data. 

}
\section{Slots}{
  \describe{

    \item{\code{equiDistance}:}{a \code{numeric}. The interdistance
      between equidistant sampled observations or 0 indicating
      non-equidistant observations.}
    
    \item{\code{factorColNames}:}{a \code{character}.}

    \item{\code{iSubset}:}{ an \code{integer} vector. References for subsetting.}

    \item{\code{jSubset}:}{ an \code{integer} vector. References for subsetting.}

    \item{\code{numericColNames}:}{a \code{character}.}

    \item{\code{positionVar}:}{ a \code{character}.}
  }
}
\section{Methods}{
  \describe{

     \item{colNames}{\code{signature(object = "ContinuousProcess", type
       = "character")}: Returns the column names if \code{type} is
       missing. If \code{type} is not missing, it must be either
       \code{'unit'}, \code{'factor'}, \code{'numeric'}, resulting in
       the column names for the specific column type only.}
     
     \item{dim}{\code{signature(x = "ContinuousProcess")}: Returns the
       dimensions.}
    
    \item{getId}{\code{signature(object = "ContinuousProcess")}: Returns
      the identification variable with name in slot \code{idVar}. }

    \item{getEquiDistance}{\code{signature(object = "ContinuousProcess")}: Returns
      the value of the \code{equiDistance} slot. A value of 0 means that
      the observations are not equidistant.}
    
    \item{getPosition}{\code{signature(object = "ContinuousProcess")}: Returns the time/position of the observations. }

    \item{getTime}{\code{signature(object = "ContinuousProcess")}: Same
    as \code{getPosition}.}
    
    \item{getNumerics}{\code{signature(object = "ContinuousProcess")}:
      Returns the numeric columns as a \code{"matrix"}.}
    
    \item{getFactors}{\code{signature(object = "ContinuousProcess")}:
      Returns the factor columns as a \code{"list"}.}
    
    \item{getValue}{\code{signature(object = "ContinuousProcess")}:
       Same as \code{getNumerics}.}
     
     \item{getUnitData}{\code{signature(object = "ContinuousProcess")}:
      Returns the unit data as a \code{"data.frame"} with row names
      the unit identifications.}

    \item{summary}{\code{signature(object = "ContinuousProcess")}:
    Computes and prints a summary of the information in the object.}
    
  }

    Other methods are documented in their respective help pages.
}

\author{Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}}

\seealso{
  \code{\linkS4class{ProcessData}},  \code{\link{continuousProcess}}. 

}

\examples{
showClass("ContinuousProcess")
CP <- continuousProcess(data.frame(id = rep(c(1,2), each = 10),
                                   time = rep(seq(0.1,1,0.1), 2),
                                   value = rnorm(20)))  
subset(CP, id == 1)
subset(CP, time >= 0.5)
}
\keyword{classes}
