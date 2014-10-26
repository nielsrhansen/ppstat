\name{ppKernel}
\alias{ppKernel}

\title{
  Generalized linear point process modeling using reproducing kernels. 
}
\description{
  The function \code{ppKernel} fits a generalized linear point process model
  based on expansions of smooth terms in a reproducing kernel Hilbert space. 
}
\usage{
ppKernel(formula, data, family, support = 1, N = 200, Delta, lambda, 
coefficients, modelMatrix = TRUE, fit = modelMatrix, varMethod = 'Fisher', 
kernel = sobolevKernel, specThres = 1e-8, ...)
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
  which links the linear predictor process to the predictable intensity process.}
  
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
  
  \item{lambda}{a \code{numeric}. The parameter that controls the
  amount of penalization.}
  
  \item{coefficients}{an optional specification of the parameters.}
  
  \item{modelMatrix}{a \code{logical}. Specifies if the model and kernel matrices are
    to be computed. Default is \code{TRUE}.}

  \item{fit}{a \code{logical}. Specifies if the model is to be
    fitted. Default value is that the model is only fitted if the model
    matrix is computed. If the \code{formula} does not have a response
    variable the model is not fitted - disregarding the value of \code{fit}.}
  
  \item{varMethod}{a \code{character}. Specifies the method used for
    estimation of the variance matrix of the parameter
    estimators. Currently the default value, \code{'Fisher'}, and
    \code{'none'} are implemented. Using \code{'Fisher'} a sandwich estimator 
    based on the Fisher information is used, whereas \code{'none'}
    means that the variance matrix is not estimated.}
    
  \item{kernel}{a \code{function}. The reproducing kernel. Default value is 
  the \code{sobolevKernel}, which is the reproducing kernel for the (2, 2) Sobolev
  space. Any vectorized function that computes a positive definite matrix on a grid 
  (and take one additional parameter, see details) is allowed. 
  }
  
  \item{specThres}{a \code{numeric}. Determines the incompleteness of the factorization 
  of the Gram matrix. A value of 0 means using the complete factorization. An incomplete 
  factorization is obtained for positive values. Default value is \code{1e-8}.
  }

  \item{\dots}{additional parameters that are passed on to 
  \code{ppmFit}.}
}
\details{

  \code{ppKernel} preprocesses the formula, and all terms of the form \code{k(.)} are 
  treated specially as kernel terms when a \code{PointProcessKernel} is created. 
  Other terms are treated as in a direct call of \code{pointProcessModel}. The kernel 
  terms are penalized by the norm in the reproducing kernel Hilbert space given by the 
  kernel.   
  
  Using the default kernel, the \code{sobolevKernel}, the resulting fit is almost 
  identical to that obtained by \code{ppSmooth}. The \code{gaussianKernel} is 
  available as an alternative. All kernels are called with an additional argument 
  \code{t = support[2]}, which for the two kernels mentioned rescales the support to
  \eqn{[0,1]}. Hence a kernel should allow for such an argument even if it is 
  ignored. 
  
  The kernel enters only through a Gram matrix computation on an \code{N} by \code{N} 
  grid, which is then factorized. The current factorization is via the spectral 
  decomposition, and for computational efficiency an incomplete factorization is 
  used. The \code{specThres} argument is the threshold for the smallest eigenvalue 
  used relative to the largest eigenvalue.  
 
  The sandwich estimator depends on the amount of penalization. If the fit is 
  oversmoothed, and thus biased, the resulting confidence intervals on the 
  filter functions are most likely misleading. 
  
  The \code{ppKernel} generally requires less memory than \code{ppSmooth} but 
  gradient computations and thus the estimation can be slower. 

}
\value{
An object of class \code{PointProcessKernel}, which is an extension of the 
\code{PointProcessModel} class. 
}

\author{
Niels Richard Hansen \email{Niels.R.Hansen@math.ku.dk}.
}
\note{
  The method does not yet support automatic data adaptive selection of \code{lambda}. 
  An information quantity (which in this case is TIC) can be extracted 
  using \code{getInformation}. This quantity can be minimized over at grid for selection
  of \code{lambda}. 
  
  The current implementation is entirely in R. Certain precomputations in \code{ppSmooth} 
  are implemented much more efficiently in compiled code. In future versions the
  corresponding precomputations for \code{ppKernel} will be transferred to compiled 
  code. 
}
\seealso{
\code{\linkS4class{PointProcessKernel}}, \code{\link{pointProcessModel}}, \code{\link{ppSmooth}}.
}

