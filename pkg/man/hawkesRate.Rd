\name{hawkesRate}
\alias{hawkesRate}
\title{
Intensity for the Hawkes process.
}
\description{
Computes the intensity, or rate, for the non-linear multivariate Hawkes process 
at a given time point given a history of points. 
}
\usage{
hawkesRate(h, Delta = 1, beta0 = rep(1, M), 
phi = function(x) pmax(x, 0), A = Inf, warn = TRUE, ...)
}
\arguments{ 
  \item{h}{a \code{list} of \code{lists} of filter function evaluations.
}
 \item{Delta}{a \code{numeric}. The distance between the equidistant evaluations of
 the filter functions. Default value is 1.
}
 \item{beta0}{a \code{numeric} vector. Default value is a vector of ones.
}
  \item{phi}{a \code{function}. Transforms the linear predictor obtained by 
  the filters to the intensity. Default is to take the positive part.
}
  \item{A}{a \code{numeric} bounding the support of the filter functions. Default value \code{Inf} means no bound.}
  \item{warn}{a \code{logical} specifying if warnings should be given. Default is \code{TRUE}.
}
  \item{\dots}{additional arguments}
}
\details{
The \code{hawkesRate} function returns a function taking the arguments \code{(t, T, N, bound, ...)}. 
This function computes 
\deqn{\varphi(\beta^0_k + \sum_k \int_0^{t-} h^{mk}(t-s) dN_s^k)}{phi(beta0_k + sum_k int_0^{t-} h^{mk}(t-s) dN_s^k)}
if \code{bound = FALSE}. It is an error to allow for event times in the history that 
doesn't strictly preceed \code{t} when \code{bound = FALSE}. If \code{bound = TRUE} 
equality is allowed. The latter option is used to compute bounds on the intensity until the next 
event occurs, which in turn is used in Ogatas thinning algorithm. 

The algorithm requires the specification of filter functions \code{h} 
(a lists of lists of function evaluations) and a bound, \code{A}, on how much history is needed. 
The list \code{h} is a list of lists with \code{h[[m]][[k]]} a vector containing the effect of
the k'th process on the m'th process. If \code{h} is a \code{numeric} the result is a simulation of a univariate point process.

}
\value{
A function that returns a \code{numeric} vector. 
}
\author{
Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}
}

\seealso{
\code{\link{Ogata}}
}