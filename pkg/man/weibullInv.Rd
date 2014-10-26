\name{weibullInv}
\alias{weibullInv}
\title{

Inverse cumulative Weibull hazard function

}
\description{

Instantiates an inverse cumulative Weibull function to be used for simulation.

}
\usage{
weibullInv(scale, gamma = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{scale}{
A \code{function} that takes the arguments \code{(list, list, numeric, ...)} and returns a numeric vector.}
  \item{gamma}{A positive \code{numeric}. The shape parameter for the Weibull distribution.
}
  \item{\dots}{
}
}
\details{

The purpose of this function is to specify an inverse cumulative hazard function that can
be used for simulating point processes. This is done by specifying a \code{scale} function 
that is used for computing a scale parameter depending on the point process history and the 
\code{gamma} shape parameter of the Weibull distribution. 

}
\value{
A function that computes the inverse cumulative Weibull function.   
}

\author{
Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}
}
\seealso{
\code{\link{invHaz}}, \code{\link{expFilterScale}}
}
