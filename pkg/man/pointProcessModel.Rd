\name{pointProcessModel}
\alias{pointProcessModel}
\alias{ppmFit}
\alias{ppmFit,PointProcessModel-method}
\alias{ppmFit,MultivariatePointProcess-method}

\title{
  Construction and estimation of a generalized linear point process model.
}
\description{
  
  The function \code{pointProcessModel} constructs objects of class 
  \code{PointProcessModel} or \code{MultivariatePointProcess}. These 
  models are generalized linear point processes with the intensity function
  given in terms of a linear predictor process, that is, in terms of a linear 
  combination of linear filters. The linear filters are specified through a symbolic
  description of the linear predictor process. The function
  \code{ppmFit} fits the model to the data. 
  
}
\usage{

pointProcessModel(formula, data, family, support = 1, N = 200, Delta,
  lambda, coefficients, modelMatrix = TRUE, fit = modelMatrix, 
  varMethod = 'Fisher', basisEnv, selfStart = TRUE, ...)

\S4method{ppmFit}{PointProcessModel}(model, control = list(), optim = 'optim', selfStart = TRUE, ...)

\S4method{ppmFit}{MultivariatePointProcess}(model, control = list(), ...)

}
\arguments{

  \item{formula}{an object of class \code{\link{formula}}. A symbolic
    description of the model to be fitted. See \sQuote{Details}.}

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
    
  \item{lambda}{an optional vector of penalization weights 
  in a quadratic penalization of the parameter vector. If missing, no 
  penalization is added to the negative log-likelihood when estimates are computed.}
  
  \item{coefficients}{an optional specification of the initial parameters
    used for the numerical optimization.}
  
  \item{modelMatrix}{a \code{logical}. Specifies if the model matrix is
    to be computed. Default is \code{TRUE}.}

  \item{fit}{a \code{logical}. Specifies if the model is to be
    fitted. Default value is that the model is only fitted if the model
    matrix is computed. If the \code{formula} does not have a response
    variable the model is not fitted - disregarding the value of \code{fit}.}
  
  \item{varMethod}{a \code{character}. Specifies the method used for
    estimation of the variance matrix of the parameter
    estimators. Currently the default value, \code{'Fisher'}, and
    \code{'none'} are implemented, with \code{'Fisher'} meaning that the
    inverse of the observed Fisher information is used and \code{'none'}
    meaning that the variance matrix is not estimated.}

  \item{basisEnv}{an optional \code{environment} holding precomputed
    basis function evaluations.}

  \item{model}{a point process model as an object of class
    \code{\linkS4class{PointProcessModel}}.}

  \item{control}{a \code{list} of control parameters for the numerical
  optimization algorithm, e.g. \code{optim}.}

  \item{optim}{a \code{character} specifying the actual optimization
  method. The default is \code{'optim'}. Other possibilities are
  \code{'poisson'}, \code{'glmnet'}, \code{'iwls'} and \code{'ls'}. See
  details below for a description.}

  \item{selfStart}{a \code{logical} specifying if the fitting procedure
  should compute a preliminary estimate before the actual
  optimization. If it is \code{FALSE}, the current model parameters
  are used as starting values. Default value is \code{TRUE}.}

  \item{\dots}{additional parameters that are passed on to \code{ppmFit}
    or the optimization algorithm, e.g. \code{optim}.}
  
}
\details{
 \code{pointProcessModel} is the main function for estimation of a generalized
  linear point process model. The function sets up the model 
  specified in terms of a formula, and, if required, computes the  
  model matrix and fits the model to the data.

  When calling \code{pointProcessModel} the three arguments
  \code{formula}, \code{data} and \code{family} must be
  specified. In addition, a specification of the argument \code{support} and
  optionally \code{N} or \code{Delta} should be considered.

  The \code{formula} specifies together with the \code{family} the
  conditional intensity for a point process in terms of the history of
  the point process itself as well as additional processes and
  variables. The \code{formula} alone specifies a \dfn{linear predictor
  process}, which consists of a linear combination of stochastic
  processes determined by the terms in the formula. The parameters to be
  estimated in the model are the coefficients in this linear
  combination. The stochastic processes are computed and stored in the
  model matrix. The \code{family} specifies a \eqn{\phi}{phi} function
  that transforms the linear predictor process into the conditional
  intensity. 

  The possibilities for specifying the linear predictor process in terms of the formula are
  quite extensive. For instance, any variable corresponding to
  continuous process observations, unit specific variables, the unit
  identification variable or the position/time variable can enter in the
  formula specification by name just as if we did an ordinary regression
  on these variables.  The variables enter directly, as specified by the
  formula, into the intensity through the inverse link function of the
  linear predictor process.  Interactions can be specified, e.g. if
  there is a column named \code{V} and the unit identification variable
  is \code{id}, the inclusion of \code{id*V} in the formula
  specification will result in unit specific parameters for the
  effect of \code{V} on the intensity. Standard rules for formula
  specification applies, e.g. \code{id*V} expands to \code{id + V +
  id:V}, and \code{id:V} by itself results in a different
  parametrization. In addition, variables can be transformed using either
  a single function or basis expansions. A model formula specification
  of the intensity that only relies on the variables listed above
  results in a (conditional) inhomogeneous Poisson process model.

  Other possible terms in the formula specification are terms involving
  the mark variables, that is, the point process data. The response (the left hand side of the formula)
  must always be one of the mark variables. If the response is
  \emph{not} specified, the model is set up, and the model matrix is
  computed - if requested - but the model is not fitted. 

  How a point process, as given by a term in the formula involving
  a mark variable, enters in the model is partly determined by the family.
  For the \code{Hawkes}-family, point processes enter in the model
  through linear filters specified by filter functions. That is, if points for
  the mark variable \code{M} are observed at \eqn{s_1, \ldots s_n}{s_1, ...,
    s_n}, and \code{f(M)} is a term in the formula, the corresponding
  linear filter 

  \deqn{\sum_{i : t-s_i \in \mathrm{support}} f(t-s_i)}{sum_{i: t-s_i in support} f(t-s_i)}

  then enters as a term in the linear predictor process.  Expansions of
  a filter function in terms of basis functions is possible, using
  e.g. \code{bSpline} to generate an expansion in terms of
  B-splines. See also \code{\linkS4class{Family}} for details on the
  family and \eqn{\phi}{phi} function specification. The term \code{M}
  results in a linear filter function on the support, whereas the term
  \code{const(M)} results in a constant function on the support. Thus
  an affine filter function can be obtained by including \code{I(const(M)
  + M)} in the formula.

  As described above, there is a fundamental difference in the ways that 
  a point process and a continuous process enter in the model
  specification. A continuous process enters \emph{as is}
  whereas a point process enters through a linear filter. This is
  considered to be natural in most cases - the point history needs to be
  aggregated through a linear filter before it enters in the
  specification of the conditional intensity, whereas it is the
  instantaneous value of the continuous process that determines the
  conditional intensity. However, it may be desirable to apply a linear
  filter to a continuous process as well. This is possible by including
  terms involving a continuous process variable name concatenated with a
  ".d". That is, if \code{V} is a continuous process variable and
  \code{f(V.d)} is a term in the formula, the corresponding
  linear filter 

  \deqn{\int_{\mathrm{support}} f(s) V_{(t-s)-} \mathrm{d}s
  }{int_{support} f(s) V((t-s)-)ds}

  then enters as a term in the linear predictor process. As for point
  processes, the filter function can be expanded using any set of basis
  functions. Currently, the implementation only supports causal
  filters. Note that the specified filter functions are 
  normalized to integrate, numerically, to 1 and that the process is
  padded with its initial value to the left. 

  The left hand side of the formula can be a point process variable or a sum of
  point process variables, in which case a univariate point process model is
  fitted. It can also be a vector of point process variables in which
  case a multivariate point process model is fitted. 

  \code{ppmFit} is called for the estimation of the model. If the model
  is set up with \code{fit = FALSE} initially, the model parameters can
  subsequently be estimated by calling \code{ppmFit}. It is usually not
  called directly by the user. Depending on the \code{optim} argument
  it calls the appropriate internal function for the actual
  optimization. The current default does numerical minimization of
  the (time-discretized) minus-log-likelihood function using \code{optim} - in general
  using the BFGS-algorithm. However, if the identity \eqn{\phi}{phi}
  function is used (choose \code{link = "identity"} in the family
  specification) the L-BFGS-B-algorithm is used instead, with the
  default lower bound \code{sqrt(Machine$double.eps)} on the intercept
  parameter and 0 on other parameters. The alternatives are described
  below. 
  
  Control parameters for the numerical optimization may be passed to the
  \code{optim} function, or the other optimization functions, via the
  \code{control} argument for \code{ppmFit}. The maximal number of
  iterations for the algorithm is set to 1000 when used in \code{ppmFit}
  in contrast to the default for \code{optim}, which is 100. See
  \code{\link{optim}} for details. A \code{control} argument in a call
  of \code{pointProcessModel} is automatically passed on to the call of
  \code{ppmFit}.

  The \code{optim} method can always be specified via a named
  \code{method} argument, which is passed to \code{optim}. The L-BFGS-B
  algorithm allows for box-constraints on the parameters. This may be
  required, e.g. if \eqn{\phi}{phi} is the identity, as mentioned
  above. The lower and upper bounds are specified in the call by setting
  the additional arguments \code{lower} and \code{upper} that are passed
  to the \code{optim} function.  See \code{\link{optim}} for details.
  Note that for estimates computed using box-constraints with one or
  more parameter value(s) on the boundary of the parameter space, the
  estimated standard errors based on the inverse Fisher information are
  unreliable. This holds true for the default method with the identity
  \eqn{\phi}{phi} function as well.

  Alternatives to \code{optim = 'optim'}, all experimental, include
  \code{optim = 'poisson'} based on \code{glm.fit} for the Poisson
  family. The implementation is based on the same time-discretized
  approximation to the minus-log-likelihood as with the \code{optim}
  function and gives comparable results. For large sparse model matrices
  the \code{optim} function is usually faster because function and
  gradient evaluations exploit sparse matrix operations. With
  \code{optim = 'poisson'} the model matrix is coerced to a dense
  matrix. With \code{optim = 'iwls'} an R implementation exploiting
  sparse matrix operations of the iterative weighted least squares
  algorithm is used as with \code{optim = 'poisson'}. The result should
  be comparable to that of the two methods described above. With
  \code{optim = 'ls'} a least squares criterion is used, which only
  works with \eqn{\phi}{phi} the identity. In this case the resulting
  quadratic optimization problem is faster to solve than minimizing the
  minus-log-likelihood. Technically, for the resulting model to be a
  well specified point process, the solution has to fulfill a positivity
  contraint, which is not enforced. Finally, with \code{optim
  = 'glmnet'} the parameters are penalized with an \eqn{l_1}{l1}-norm
  and the solution is computed using the \code{glmnet} function. This
  is implemented with \eqn{\phi}{phi} the identity, in which case a least squares
  criterion is used, and with \eqn{\phi}{phi} the exponential (that is,
  with a log-link function), in which case the minus-log-likelihood is
  used.
  
}

\value{ An object of class \code{\linkS4class{PointProcessModel}} or
  \code{\linkS4class{MultivariatePointProcess}} 
  containing the estimated parameters.
  
  Use \code{summary} to get a standard summary output from the
  fitted model.

  Use \code{termPlot} on the resulting model to plot  the estimated
  filter function(s).
}
\note{

  The current implementation supports linear filters of continuous
  processes through the \code{.d} notation in the formula. However, the
  data structure allows for non-equidistant sampling, and the filter
  computations are done using numerical integration and not
  FFT. Consequently, the computation of the model matrix for large data
  sets can be prohibitively slow.

}


\references{
  Hansen, N. R. \emph{Penalized maximum likelihood estimation for
  generalized linear point processes}. 	arXiv:1003.0848v1. \url{http://arxiv.org/abs/1003.0848} 
}
\author{
Niels Richard Hansen \email{Niels.R.Hansen@math.ku.dk}.

Maintainer: \email{Niels.R.Hansen@math.ku.dk}
}

\seealso{
  \code{\linkS4class{PointProcessModel}},  \code{\link{update}},
  \code{\link{optim}}, \code{\link{bSpline}}, \code{\link{termPlot}},
  \code{\link{registerParBackend}}, \code{\link{glm.fit}}.
}
\examples{
data(toyData)
toyPPM <- pointProcessModel(BETA ~ const(ALPHA),
                            data = toyData,
                            family = Hawkes(),
                            support = 2)
summary(toyPPM)
\dontrun{## Plot of the filter function
termPlot(toyPPM)}

toyPPM <- pointProcessModel(BETA ~ cut(ALPHA, c(0,1,2),
                                       include.lowest = TRUE),
                            data = toyData,
                            family = Hawkes(),
                            support = 2)
summary(toyPPM)
\dontrun{## Plot of the filter function
termPlot(toyPPM)}

## Removing the baseline intensity.
toyPPM <- update(toyPPM, .~. -1)
summary(toyPPM)

## Different baselines for each unit. This 'update' adds a term
## not originally in the model, thus the model matrix will be
## recomputed. 
toyPPM <- update(toyPPM, .~. + id)
summary(toyPPM)


}
\keyword{models}
