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
hawkesRate(t, T = list(), h, predict = TRUE, 
Delta = 1, beta0 = rep(1, M), phi = function(x) pmax(x, 0), warn = TRUE, ...)
}
\arguments{
  \item{t}{a \code{numeric}. The time point at which the intensity is computed.
}
  \item{T}{a \code{list}. The history of the process as a list of vectors, one for 
  each mark, containing the event times preceeding \code{t}.
}
  \item{h}{a \code{list} of \code{lists} of filter function evaluations.
}
  \item{predict}{a \code{logical}. Specifies whether the events in the 
  history \code{T} have to strictly preceed \code{t} (\code{predict = TRUE}, the default), 
  or whether equality is allowed.  
}
 \item{Delta}{a \code{numeric}. The distance between the equidistant evaluations of
 the filter functions. Default value is 1.
}
 \item{beta0}{a \code{numeric} vector. Default value is a vector of ones.
}
  \item{phi}{a \code{function}. Transforms the linear predictor obtained by 
  the filters to the intensity. Default is to take the positive part.
}
  \item{warn}{a \code{logical} specifying if warnings should be given. Default is \code{TRUE}.
}
  \item{\dots}{additional arguments}
}
\details{
The \code{hawkesRate} function computes 
\deqn{\varphi(\beta^0_k + \sum_k \int_0^{t-} h^{mk}(t-s) dN_s^k)}{phi(beta0_k + sum_k int_0^{t-} h^{mk}(t-s) dN_s^k)}
if \code{predict = TRUE}. It is an error to allow for event times in the history that 
don't strictly preceed \code{t} when \code{predict = TRUE}. If \code{predict = FALSE} 
equality is allowed. The latter option is used to compute bounds on the intensity until the next 
event occurs, which in turn is used in Ogatas thinning algorithm. 

}
\value{
A \code{numeric} vector. 
}
\author{
Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}
}

\seealso{
\code{\link{Ogata}}
}