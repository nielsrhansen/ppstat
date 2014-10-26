\name{expFilterScale}
\alias{expFilterScale}
\title{
Exponential backward moving average
}
\description{

Instantiates a scale function that computes scales for simulation by
an exponentially decaying backward moving average.

}
\usage{
expFilterScale(alpha0 = 0, beta = 1, alpha = 1, ...)
}
\arguments{
  \item{alpha0}{A \code{numeric}.
}
  \item{beta}{A \code{numeric}.
}
  \item{alpha}{A \code{numeric}.
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
This function can be used in combination with \code{invHaz} and \code{weibullInv} to compute 
history dependent scale parameters. 
}
\value{
A function that takes a point process history given as \code{(T = list, N = list, t = numeric)} 
and computes backward moving averages from \code{t}. 
}
\author{
Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}
}
\seealso{
\code{\link{invHaz}}, \code{\link{weibullInv}}
}