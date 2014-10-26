\name{MultivariatePointProcess-class}
\Rdversion{1.1}
\docType{class}
\alias{MultivariatePointProcess-class}
\alias{formula,MultivariatePointProcess-method}
\alias{getLinearFilter,MultivariatePointProcess-method}
\alias{simulate,MultivariatePointProcess-method}
\alias{summary,MultivariatePointProcess-method}
\alias{termPlot,MultivariatePointProcess-method}

\title{Class \code{"MultivariatePointProcess"}}
\description{

  An object of class \code{"MultivariatePointProcess"} is returned from
  the constructor \code{pointProcessModel}. It contains the fitted
  generalized linear point process model. The class is essentially a
  list of univariate point process objects of class
  \code{\linkS4class{PointProcessModel}} with some additional infrastructure.

}
\section{Objects from the Class}{
Objects can be created by calls of \code{\link{pointProcessModel}}.
}
\section{Slots}{
  \describe{
    \item{\code{models}:}{Object of class \code{"list"}.}
    \item{\code{adjMat}:}{Object of class \code{"matrix"}.}
  }
}
\section{Methods}{
  \describe{
    
    \item{formula}{\code{signature(x = "MultivariatePointProcess")}:
      Returns a list formulas. }
    
    \item{summary}{\code{signature(object =
	"MultivariatePointProcess")}: 
      Prints a summary for each univariate point process in the object. }

    \item{termPlot}{\code{signature(model = "MultivariatePointProcess")}:  Plots the (estimated) linear filter
       functions.}

  }
}

\author{
Niels Richard Hansen \email{Niels.R.Hansen@math.ku.dk}.

Maintainer: \email{Niels.R.Hansen@math.ku.dk}
}
\references{

  Didelez, V. (2008) \emph{Graphical models for marked point processes
    based on local independence}, Journal of the Royal Statistical
  Society, Series B, 70(1), 1467-9868.

}

\seealso{
  \code{\linkS4class{PointProcessModel}}, \code{\link{pointProcessModel}}.
}
\examples{
showClass("MultivariatePointProcess")
}
\keyword{classes}
