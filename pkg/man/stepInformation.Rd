\name{stepInformation}
\alias{stepInformation}
\alias{stepInformation,PointProcessModel-method}
\alias{stepInformation,MultivariatePointProcess-method}
\alias{getInformation}
\alias{getInformation,PointProcessModel-method}

\title{
  Stepwise point process model selection
}
\description{
  Performs a stepwise, greedy model selection using an information criteria.
}
\usage{
\S4method{stepInformation}{PointProcessModel}(model, scope = ~ 0, direction = "both", trace = 1,
steps = 1000, warmStart = TRUE, k = 2, \dots)

\S4method{stepInformation}{MultivariatePointProcess}(model, scope = ~ 0, direction = "both", trace = 1,
steps = 1000, warmStart = TRUE, k = 2, \dots)

\S4method{getInformation}{PointProcessModel}(model, k = 2, \dots)

}
\arguments{

  \item{model}{ a \code{"PointProcessModel"} object. This model is used
    as the initial model in the stepwise search.  }
  
  \item{scope}{ a formula. Only the right hand side matters. It
  specifies an upper bound of models to consider. The default, \code{~
  0}, results in the upper bound being the formula of \code{model}. If
  specified, the upper bound is the model whose formula has a right hand
  side, which is the sum of terms from \code{scope} and the formula of
  \code{model}.}

%  \item{scale}{ a \code{numeric}. Usage currently not implemented.  }

   \item{direction}{ a \code{character}, one of
    \code{"both"} or \code{"backward"}, with a
    default of \code{"both"}. Determines the direction of the search. }

   \item{trace}{ a \code{numeric}. If set to 1 (the default) the
   algorithm prints tabular information while running. A value of 2
   gives a different format for the information and the selected model in
   each step is printed. Other values result in no printed information.}

%  \item{keep}{Usage currently not implemented.}
  
  \item{steps}{ a \code{numeric}. The maximum number of steps. Default
  value is 1000.
  }
  
  \item{warmStart}{ a \code{logical}. Determines if any updated fit
    should be computed with initial parameter values taken from the
    previous fit. The default is \code{TRUE}.
  }
  
  \item{k}{ a \code{numeric}. The multiple of the number of degrees of
    freedom used for the penalty. The default is \code{k = 2}, which
    gives genuine AIC. 
  }
  
  \item{\dots}{ other arguments}
  

}
\value{

  The stepwise selection results in a model for which the information
  criteria can not be reduced further by additional steps (a local
  minimizer). This resulting model, an object of class
  \code{"PointProcessModel"}, is returned (invisibly) from
  \code{stepInformation}.

  For \code{getInformation} a numeric is returned. 
}
\details{

  N.B. The function \code{stepAIC} from \pkg{MASS} was used as a
  template for this function. Several of the functionalities of \code{stepAIC}
  are currently not implemented in this method. 

}


\seealso{
\code{\link{stepAIC}}, \code{\link{pointProcessModel}}.
}

\keyword{models}
