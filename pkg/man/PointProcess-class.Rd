\name{PointProcess-class}
\docType{class}
\alias{PointProcess-class}
\alias{processData}
\alias{processData,PointProcess-method}
\alias{processData<-}
\alias{processData<-,PointProcess-method}
\alias{computeMinusLogLikelihood,PointProcess-method}
\alias{computeMinusLogLikelihood}
\alias{family,PointProcess-method}
\alias{formula,PointProcess-method}
\alias{formula<-,PointProcess,formula-method}
\alias{response}
\alias{response,PointProcess-method}


\title{Class "PointProcess"}
\description{
  
  This class is a virtual class. It implements general structures
  for generalized linear point process models. 
  
}
\section{Slots}{
  \describe{
    
    \item{\code{call}:}{a \code{"call"}. The call that
	created the point process model.}
      
    \item{\code{Delta}:}{a \code{"numeric"}. The
      equidistant spacings between basis function evaluations - within the
      support.}
    
    \item{\code{delta}:}{a \code{"numeric"}. The
      interdistances between the grid (time) points in the data set.}
      
    \item{\code{df}:}{a \code{"numeric"}. The effective degrees of freedom.}
    
    \item{\code{family}:}{an object of class \code{"Family"}. The family
      object specifying the specific interpretation of the formula
      specification.}
     
    \item{\code{formula}:}{a \code{"formula"}. The model
      formula specifying the model.}
      
    \item{\code{loss}:}{a \code{"character"} specifying the type of loss function 
    used for model fitting. }

     \item{\code{response}:}{a \code{"character"}. The vector of 
     response marks. Extracted from the \code{formula}.}

    \item{\code{optimResult}:}{a \code{"list"} containing the results
      from a call to \code{optim}.}
    
    \item{\code{processData}:}{a \code{"MarkedPointProcess"} - the model data.}
    
    \item{\code{support}:}{a \code{"numeric"}. The
      support, [a,b], of the linear filter functions as a vector of
      length 2, \code{support = c(a,b)}.}
  }
}
\section{Methods}{
  \describe{

    \item{computeMinusLogLikelihood}{\code{signature(model =
	"PointProcessModel")}: Computes the minus-log-likelihood
      function.}

    \item{family}{\code{signature(object = "PointProcess")}: Returns the
    \code{Family} object from the model.} 
    
    \item{formula}{\code{signature(model = "PointProcess")}: Returns
      the model formula.}

    \item{formula<-}{\code{signature(model = "PointProcess",
	value = "formula")}: Sets the model formula and the response.}

    \item{response}{\code{signature(model = "PointProcess")}: Returns
      the response mark(s).}
    
    \item{processData}{\code{signature(model = "PointProcess")}: Returns
      the process data.}

    \item{processData<-}{\code{signature(model = "PointProcess",
	value = "ProcessData")}: Sets the process data.}
  
	 }
}

\author{Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}}

\seealso{\code{\link{pointProcessModel}}, \code{\linkS4class{PointProcessModel}}, \code{\linkS4class{ProcessData}}, \code{\link{formula}}.
}
\examples{
showClass("PointProcess")
}

\keyword{classes}
