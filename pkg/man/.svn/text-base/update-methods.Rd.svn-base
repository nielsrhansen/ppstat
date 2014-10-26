\name{update-methods}
\alias{update-methods}
\alias{update,PointProcessModel-method}
\alias{update,MultivariatePointProcess-method}
\title{update}
\description{
  
  The function \command{update} will update and refit a
  \code{PointProcessModel} and a \code{MultivariatePointProcess}.
  
}
\usage{

\S4method{update}{PointProcessModel}(object, formula = NULL, warmStart = TRUE, fit = TRUE, lambda = NULL, \dots)

\S4method{update}{MultivariatePointProcess}(object, formula = NULL, \dots)

}
\arguments{
  \item{object}{the \code{PointProcessModel} object to be updated.}

  \item{formula}{Changes to the formula - see \code{\link{update.formula}} for
    details. The default value \code{NULL} results in a refit of the
    model.}

  \item{warmStart}{a \code{logical}. Specifies if the optimization
    should use the previously
    estimated values of the parameters from the original
    model, if possible, as initial values. Default is
    \code{TRUE}.}

  \item{fit}{a \code{logical}. Should the model be refitted. Default
  value is \code{TRUE}.}
  
  \item{lambda}{a \code{numeric}. Either a vector or a single numeric
  specifying the penalization weight on the different parameters. Default
  value is \code{NULL}, which means no penalization.}

  \item{\dots}{additional arguments to the call. Passed on to \code{ppmFit}.}
}


\value{
  
  An object of class \code{\linkS4class{PointProcessModel}} or
  \code{\linkS4class{MultivariatePointProcess}} containing the estimated
  parameters after the update.
  
}

\details{

  This function is used to update and refit a model of class
  \code{\linkS4class{PointProcessModel}} or
  \code{\linkS4class{MultivariatePointProcess}} as an alternative to
  constructing a new model object by calling the function
  \command{pointProcessModel} directly.

  A call of \code{update} without specifying the \code{formula} will
  refit the model without recomputing the model matrix. Combined with
  setting the coefficients of the \code{model} object this can be used 
  to try different initial values for the numerical optimization
  algorithm. Also, additional (alternative) arguments can be given,
  which are passed on to \code{ppmFit} and ultimately to \code{optim}. 

  If possible, the updated model will reuse (parts of) the model matrix
  and thus not recompute the entire model matrix as would happen by a
  direct call to \command{pointProcessModel}. Reuse will happen if the
  updated model is a submodel of the original model and will reduce
  computation time as well as memory usage. For large data sets and/or
  many linear predictors the model matrix becomes very large, and the
  time to compute the matrix can be considerable. Most notably, by using
  \code{update} the model matrix is copied by reference from the
  original model to the submodel, and thus does not take up memory
  twice. When investigating several models that are all submodels of a
  single supermodel it is therefore recommended to fit the supermodel
  first and use \code{update} to fit the different submodels.

  The current implementation of \code{update} works by using the
  assignment of columns in the model matrix to terms in the
  formula. This assignment is given by the attribute \code{assign} to
  the model matrix. A new model is identified as a submodel if all terms
  in the new model are terms in the original model. The definition used
  is that the updated model matrix consists of the columns that are
  assigned to a term in the new model. A consequence of this is that the
  updated model may not be parametrized as expected nor as if the model
  was specified using \command{pointProcessModel}. This is in particular
  the case when interactions are included, in which case a warning is
  currently given when using update. There are also other cases, in
  particular if one removes the intercept, where the updated model may
  not be as expected. At a deeper level these issues are due to the
  contrasts used when including factors in the orginal formula
  specification. Nothing is currently done in the code to deal with
  this. To avoid obvious pitfalls, don't remove a term using update if
  there is a higher order term involving the same variable(s). In
  particular, don't remove the intercept if there are factors in the
  model and don't remove a factor if it is included in an interaction. 

  For any submodel the model matrix is given by reference
  to the full model matrix of the supermodel. Therefore,
  a call to \code{getModelMatrix} for a submodel will return a subset of
  columns from the full model matrix. Consequently, the references in the
  attribute \code{assign} are to the terms in the full model formula and
  not the terms in the submodel formula. The full model formula is stored in
  the attribute \code{formula} of the model matrix. The attributes are
  intended for internal use in \code{update} primarily. It is recommended that
  they are not relied upon for other usages.  
  
}


\author{Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}}

\seealso{

\code{\link{update}}, \code{\link{update.formula}}, \code{\link{pointProcessModel}}, \code{\linkS4class{PointProcessModel}}

}

\keyword{models}
