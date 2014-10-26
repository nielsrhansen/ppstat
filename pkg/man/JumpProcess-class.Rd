\name{JumpProcess-class}
\docType{class}
\alias{JumpProcess-class}

\title{Class "JumpProcess"}
\description{	

An object of class \code{JumpProcess} is a container for data
obtained as observations of continuous time processes with jumps.
The jump times and jump sizes are represented as a marked point
process. 

}
\section{Objects from the Class}{
Objects can be created by calls of the function \code{jumpProcess}.

The object contains time points or positions in one-dimensional space of
continuous process data together with jump times and jump sizes.

The class extends \code{MarkedPointProcess} and contains no additional
slots. 

}

\section{Methods}{
  The only method implemented is \code{plot} documented in its own help
  page. 
  
}
              
\author{Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}}

\seealso{
  
  \code{\link{jumpProcess}}, 
  \code{\linkS4class{ProcessData}},
  \code{\linkS4class{ContinuousProcess}},
  \code{\linkS4class{JumpProcess}}
}

\examples{
showClass("JumpProcess")
}
\keyword{classes}
