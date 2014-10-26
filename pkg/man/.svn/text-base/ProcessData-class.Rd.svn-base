\name{ProcessData-class}
\docType{class}
\alias{ProcessData-class}
\alias{getUnitData,ProcessData-method}
\alias{setUnitData,ProcessData-method}
\alias{getId,ProcessData-method}
\alias{setUnitData}
\alias{colNames,ProcessData,missing-method}
\alias{colNames,ProcessData,character-method}
\alias{process}
\alias{process,data.frame-method}
\alias{dim,ProcessData-method}

\title{Class "ProcessData"}
\description{	 

  A base class for holding unit specific informations and meta data
  for more complex classes for process data.
  
}
\section{Objects from the Class}{
  Objects of this class are usually not created. It is essentially just
  a data frame wrapped up in an S4-object. It can be created using the
  constructor \code{process}. 

  The class serves the purpose of being a building block for the classes 
  \code{ContinuousProcess} and \code{MarkedPointProcess}.
}
\section{Slots}{
  \describe{
    
    \item{\code{metaData}:}{a \code{list}.}

    \item{\code{unitColNames}:}{a \code{character}.}

    \item{\code{valueEnv}:}{an \code{environment}.}

    \item{\code{idVar}:}{a \code{character}.}

    \item{\code{iUnitSubset}:}{an \code{integer} vector. References for subsetting.}

    \item{\code{jUnitSubset}:}{an \code{integer} vector. References for subsetting.}
      
  }
}
\section{Methods}{
  \describe{

    \item{colNames}{\code{signature(object = "ProcessData", type = "character")}: Returns
      the column names whether \code{type} is missing or equal to
      \code{'unit'}. Other values of \code{type} are not allowed.}
     
    \item{dim}{\code{signature(x = "ProcessData")}: Returns the
      dimensions.}

    \item{getId}{\code{signature(object = "ProcessData")}: Returns
      the identification variable with name in slot \code{idVar}. }

    \item{getUnitData}{\code{signature(object = "ProcessData")}: Returns
      the \code{data.frame} of unit data from slot \code{unitData}.}

    \item{process}{\code{signature(unitData = "data.frame")}(unitData,
    idVar = "id", ...): Creates a \code{ProcessData} object from a data frame.}
    
    \item{setUnitData}{\code{signature(object = "ProcessData", value =
      "data.frame")}: Sets the unit data in slot \code{unitData} equal
      to \code{value}.} 

  }
}
       
\author{Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}}

\seealso{
	 \code{\linkS4class{ContinuousProcess}} and  \code{\linkS4class{MarkedPointProcess}}. 
}
\examples{
showClass("ProcessData")
}
\keyword{classes}
