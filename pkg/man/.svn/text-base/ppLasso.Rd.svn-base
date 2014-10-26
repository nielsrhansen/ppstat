\name{ppLasso}
\alias{ppLasso}

\title{
  Generalized linear point process modeling using lasso penalization. 
}
\description{

The function \code{ppLasso} fits a generalized linear point process model
with a lasso penalization on all parameters.

}
\usage{
ppLasso(formula, data, family, support = 1, N = 200, Delta, ...)
}
\arguments{
 \item{formula}{an object of class \code{\link{formula}}. A symbolic
    description of the model to be fitted. Kernel terms are treated in 
    a special way and other terms are treated as in \code{\link{pointProcessModel}}.
    See \sQuote{Details}.}

  \item{data}{an object of class \code{\linkS4class{MarkedPointProcess}} containing
    the point process data as well as any continuous process data.}

  \item{family}{an object of class
  \code{\linkS4class{Family}}. Specification of the general model
  family containing the specification of the \eqn{\phi}{phi} function,
  which links the linear predictor process to the predictable intensity process.
  The default value is \code{Hawkes("identity")}, but also \code{Hawkes("log")} is
  allowed.}
  
  \item{support}{a \code{numeric} vector. Specifies the support of the
    filter functions as the interval from \code{support[1]} to
    \code{support[2]}. If \code{support} is of length 1 the support is the
    interval from 0 to \code{support[1]}. The default value is 1.}

  \item{N}{a \code{numeric}. The number of basis function evaluations
    used in the support. Default value 200.}

  \item{Delta}{a \code{numeric}. Basis functions are evaluated at
    \code{Delta}-grid values in the support. If missing, \code{Delta} is
    set to the length of the support divided by \code{N}. If specified,
    overrides the use of \code{N}.}
 
 \item{\dots}{additional parameters that are passed on to 
  \code{pointProcessModel}.
  }
}

\details{

The function provides an interface for using the \code{glmnet} function from 
the suggested package \code{glmnet} to fit a model with the lasso penalization on the 
parameter vector. 

With \code{family = Hawkes("identity")} (the default) \code{ppLasso} uses a quadratic contrast
function for estimation, as considered in Hansen, Reynaud-Bouret and Rivoirard, and
the different parameters are weigthed according to the criteria derived in that
paper. With \code{family = Hawkes("log")} the likelihood is used. A one-dimensional tuning 
parameter remains to be selected, which is currently done by an ad hoc degrees-of-freedom 
computation. 

}
\value{
A model of class \code{PointProcessModel}.
}
\references{
N. R. Hansen, P. Reynaud-Bouret and V. Rivoirard. \emph{Lasso and probabilistic 
inequalities for multivariate point processes.} arXiv:1208.0570 
  }
\author{
Niels Richard Hansen \email{Niels.R.Hansen@math.ku.dk}.
}
\note{With \code{family = Hawkes("identity")} the model returned may 
have filter functions taking negative values. No approximate standard errors 
are currently computed.}
\seealso{
\code{\link{pointProcessModel}}
}
