\name{sobolevKernel}
\alias{sobolevKernel}
\alias{gaussianKernel}

\title{
Reproducing kernels
}
\description{
Computation reproducing kernel evaluations.
}
\usage{
sobolevKernel(s, r, t = 1, sub = NULL, constraint = 'boundary')
gaussianKernel(s, r, t = 1, c = 1)
}
\arguments{
  \item{s}{a \code{numeric}.
}
  \item{r}{a \code{numeric}.
}
  \item{t}{a \code{numeric}. Arguments are rescaled by \code{t}. Default
  value 1. 
}
  \item{c}{a \code{numeric}. The squared inverse scale parameter for the 
  Gaussian kernel. 
  }
  \item{sub}{a \code{numeric}. Determines if the kernel should be projected to a subspace. 
  Default value \code{NULL} means no subspace. If 0, the kernel is projected to the 2-dimensional 
  subspace spanned by the constant and linear functions, and if anything else, the kernel 
  is projected to the orthogonal complement. 
  }
  \item{constraint}{a \code{character}. The Sobolev space is a direct sum of a 2-dimensional
  space and a constraint space. This can either be the space of functions with boundary values 0,
  or the space of functions with initial value and derivative 0. The corresponding direct sum 
  kernels differ. The default is 'boundary'. 
  }
}
\details{The Sobolev kernel is the reproducing kernel for the (2,2) Sobolev space
on \eqn{[0, t]}. The arguments \code{s} and \code{r} must be in the interval. If \code{sub = 1}
the kernel is projected to the subspace of functions being 0 in 0 and \eqn{t}

The Gaussian kernel is \eqn{\exp{-c(s-r)^2/t^2}}{exp(-c(s-r)^2/t^2)}. 
Both arguments \code{t} and \code{c} are needed when used with \code{ppKernel},
where \code{t} is always the upper bound on the support.  

}
\value{
A \code{numeric}. The value of the kernel in \eqn{(s,r)}.
}

\author{
Niels Richard Hansen \email{Niels.R.Hansen@math.ku.dk}.
}

\seealso{
\code{\link{ppKernel}}
}
\examples{
sobolevKernel(1:5, 1:5)
sobolevKernel(1:5, 1:5, t = 5)

gaussianKernel(1:5, 1:5)
gaussianKernel(1:5, 1:5, t = 5)
gaussianKernel(1:5, 1:5, t = 5, c = 2)
}
