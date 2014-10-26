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
Ogata(n = 1, lambda, h, A = Inf, seed = NULL, tLim = Inf, ...)
}
\arguments{
  \item{n}{an \code{integer}. The total number of points to be simulated. Default value is 1.
}
  \item{lambda}{a \code{function} with arguments \code{t, T, h, predict} with \code{t} 
  a numeric, \code{T} a list of history points preceeding \code{t} and \code{h} a list of 
  filter functions. The argument \code{predict} is a logical specifying if the history 
  must preceed \code{t} strictly (\code{predict = TRUE}) or not. The function must return
  a vector of intensities, one for each coordinate, and 
}
  \item{h}{a \code{list} of \code{lists} of filter function evaluations.
}
  \item{A}{a \code{numeric} bounding the support of the filter functions. Default \code{Inf} 
  means no bound.   
  }
  \item{seed}{an \code{integer}. Default \code{NULL}.}
  \item{tLim}{an \code{numeric}. Upper time limit. Default is no limit.}
  \item{\dots}{other arguments passed to \code{lambda}.}
}
\details{

This implementation of the Ogata thinning algorithm generates \code{n} points based 
on a vector valued function \code{lambda} that returns the intensity 
given the history of points. For an example of a \code{lambda}-function 
see \code{\link{hawkesRate}}. 

The algorithm requires the specification of filter functions \code{h} 
(a lists of lists of function evaluations) and a bound, \code{A}, on how much history is needed. 
The list \code{h} is a list of lists with \code{h[[m]][[k]]} a vector containing the effect of
the k'th process on the m'th process. 

}
\value{
A \code{data.frame} with the two columns \code{time} and \code{markType} and \code{n} rows. 
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
           lambda = hawkesRate, 
           h = h1,
           A = A,
           Delta = Delta,
           beta0 = c(0.1, 0.1)
)
}
