\name{PointProcessKernel-class}
\Rdversion{1.1}
\docType{class}
\alias{PointProcessKernel-class}
\alias{coefficients<-,PointProcessKernel,numeric-method}
\alias{computeDDMinusLogLikelihood,PointProcessKernel-method}
\alias{computeDMinusLogLikelihood,PointProcessKernel-method}
\alias{computeMinusLogLikelihood,PointProcessKernel-method}
\alias{update,PointProcessKernel-method}

\title{Class \code{"PointProcessKernel"}}
\description{

A point process model class that extends \code{"PointProcessModel"} to handle smooth filter terms
non-parametrically using reproducing kernel Hilbert spaces.  

}
\section{Objects from the Class}{

Objects are created by calls of \code{ppKernel)}.

}
\section{Slots}{
New slots: 

  \describe{
    \item{\code{g}:}{a \code{"numeric"} containing the kernel term evaluations.}
    \item{\code{kernelTerms}:}{a \code{"numeric"} specifying the terms in the formul
    that are filter terms.}
    \item{\code{kernelMatrixCol}:}{a \code{numeric} vector.}
    \item{\code{kernelMatrixEnv}:}{an environment that contains an
      object of class \code{"Matrix"} - a sort of model matrix corresponding to 
      the kernel terms.}
    \item{\code{responseKernelMatrix}:}{a \code{"Matrix"}.}
    \item{\code{U}:}{a \code{"Matrix"} specifying a reparametrization.}
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

\seealso{\code{\linkS4class{PointProcessModel}}, \code{\linkS4class{PointProcessSmooth}},
\code{\linkS4class{PointProcess}}, \code{\link{ppKernel}}, \code{\link{ppSmooth}}, \code{\linkS4class{ProcessData}}
}

\keyword{classes}
