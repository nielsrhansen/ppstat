\name{Ogata}
\alias{Ogata}
\title{
Multivariate Ogata thinning algorithm
}
\description{

Simulation of multivariate (marked) point processes using 
the Ogata thinning algorithm.

}
\usage{
Ogata(n = 1, lambda, seed = NULL, tLim = Inf, adapK = FALSE, ...)
}
\arguments{
  \item{n}{an \code{integer}. The total number of points to be simulated. Default value is 1.
}
  \item{lambda}{a \code{function} with the arguments \code{t, T, N, bound} with \code{t} 
  a numeric, \code{T} a list of points and \code{N} is a list of indices in \code{T} specifying the points 
  preceeding \code{t}. The argument \code{bound} is a logical specifying if the function returns the
  intensity (\code{bound = FALSE}) or and upper bound. The function must return
  a vector of intensities, one for each coordinate. It is usually given by a call to another function 
  that returns the appropriate intensity function for a given set of parameters etc. An example is the 
  \code{hawkesRate} function.
  }
  \item{seed}{an \code{integer}. Default \code{NULL}.}
  \item{tLim}{an \code{numeric}. Upper time limit. Default is no limit.}
  \item{adapK}{a \code{logical}. Specifies if the upper bound on the intensity is recomputed 
  adaptively for all suggested points or only for accepted points. Default is \code{FALSE}, so 
  the bound is only recomputed for accepted points.} 
  \item{\dots}{other arguments passed to \code{lambda}.}
}
\details{

This implementation of the Ogata thinning algorithm generates \code{n} points or points 
in the interval from 0 to \code{tlim} if the latter is finite. It is based 
on a vector valued function \code{lambda} that returns the intensity 
given the history of points. For an example of specifying a \code{lambda}-function 
see \code{\link{hawkesRate}}. 

}
\value{
Depending on \code{LambdaInv} either a \code{data.frame} with the two columns \code{time} and \code{markType}
if the process is multivariate or a \code{numeric} if the process us univariate. 
}

\author{
Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}
}
\seealso{
\code{\link{hawkesRate}}
}
\examples{
h <- list( 
  list(function(t)
        - 0.1 * exp(- (2*(t-1))^2), 
       function(t)
         exp(- (2*(t-2))^2)
  ),
  list(function(t)
        exp(- (2*(t-1))^2),
       NULL    
  )
)

## Evaluations of the filter functions.

M <- length(h)
Delta <- 0.001
A <- 5
h1 <- vector("list", M)

for (m in seq_len(M)) {
  h1[[m]] <- vector("list", M)
  for (k in seq_len(M)) {
    if (!is.null(h[[m]][[k]])) {
      h1[[m]][[k]] <- h[[m]][[k]](seq(0, A, Delta))
    }
  }
}

## Simulation using the 'hawkesRate' intensity function.

T <- Ogata(100, 
           lambda = hawkesRate(h = h1,  
                               A = A,
                               Delta = Delta,
                               beta0 = c(0.1, 0.1)
                               )
          )
}
