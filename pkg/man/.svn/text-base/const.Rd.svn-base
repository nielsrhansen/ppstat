\name{const}
\Rdversion{1.1}
\alias{const}

\title{
Indicator function multiplied by a constant.
}
\description{

  This function is a simple wrapper intended to be used in a
  formula specification for generalized linear point process models.

}
\usage{
const(x, y, c = 1)
}

\arguments{
  \item{x}{
  a numeric.  
  }
  \item{y}{
  an optional numeric.
}
  \item{c}{
  an optional numeric.  
  }
}

\value{
  The function returns a numeric of the same length as \code{x}. The
  result is constantly equal to \code{c} if \code{y} is missing. If
  \code{y} is not missing the result is the vector \code{c *
    as.numeric(x <= y)}. 
  
}

\author{
  Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}
}


\examples{

const(5)
const(10, 5)
const(5, 10)
const(1:10, rep(5,10), 2)

}
\keyword{ math }
