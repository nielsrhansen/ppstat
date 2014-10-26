\name{PointProcessSmooth-class}
\Rdversion{1.1}
\docType{class}
\alias{PointProcessSmooth-class}
\alias{computeDDMinusLogLikelihood,PointProcessSmooth-method}
\alias{computeDMinusLogLikelihood,PointProcessSmooth-method}
\alias{computeMinusLogLikelihood,PointProcessSmooth-method}
\alias{update,PointProcessSmooth-method}

\title{Class \code{"PointProcessSmooth"}}
\description{

A point process model class that extends \code{"PointProcessModel"} to handle smooth filter terms
non-parametrically using B-spline expansions. 

}
\section{Objects from the Class}{
Objects are created by calls of \code{ppSmooth)}.
}
\section{Slots}{

New slots:

  \describe{
    \item{\code{smoothTerms}:}{a \code{"numeric"} specifying the smooth terms in the formula.}
    \item{\code{V}:}{a \code{"Matrix"} specifying a reparametrization.} 
  }
}
\section{Extends}{
Class \code{"\linkS4class{PointProcessModel}"}, directly.
Class \code{"\linkS4class{PointProcess}"}, by class "PointProcessModel", distance 2.
}
\section{Methods}{
  Available methods as for \code{"PointProcessModel"}.
}
\author{Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}}

\seealso{\code{\linkS4class{PointProcessModel}}, \code{\linkS4class{PointProcessKernel}},
\code{\linkS4class{PointProcess}}, \code{\link{ppSmooth}}, \code{\link{ppKernel}}, \code{\linkS4class{ProcessData}}
}


\keyword{classes}
