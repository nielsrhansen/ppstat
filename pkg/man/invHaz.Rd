\name{invHaz}
\alias{invHaz}
\title{
Inverse hazard algorithm
}
\description{

Simulation of multivariate (marked) point processes using 
the inverse hazard algorithm.

}
\usage{
invHaz(n = 1, LambdaInv, seed = NULL, tLim = Inf, ...)
}
\arguments{
  \item{n}{an \code{integer}. The total number of points to be simulated. Default value is 1.
}
  \item{LambdaInv}{a \code{function} with the arguments \code{s, T, N, t} with \code{s} 
  a numeric, \code{T} a list of points and \code{N} is a list of indices in \code{T} specifying the points 
  preceeding \code{t}. The function must return
  a vector of intensities, one for each coordinate. It is usually given by a call to another function 
  that returns the appropriate intensity function for a given set of parameters etc. An example is the 
  \code{hawkesRate} function.
  }
  \item{seed}{an \code{integer}. Default \code{NULL}.}
  \item{tLim}{an \code{numeric}. Upper time limit. Default is no limit.}
  \item{\dots}{other arguments passed to \code{LambdaInv}.}
}
\details{

The inverse hazard algorithm generates \code{n} points or points 
in the interval from 0 to \code{tlim} if the latter is finite. It is based 
on a function \code{LambdaInv} that computes the (combined) inverse cumulative hazard 
of a given point \code{s} given the history of points before \code{t}, and for 
multivariate point processes the conditional distribution of the different coordinates. 
For an example of specifying a \code{LambdaInv}-function 
see \code{\link{weibullInv}}. 

}
\value{
Depending on \code{LambdaInv} either a \code{data.frame} if the process is multivariate or a 
\code{numeric} if the process us univariate. 
}

\author{
Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}
}
\seealso{
\code{\link{weibullInv}}
}
