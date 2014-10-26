\name{termPlot-methods}
\alias{termPlot}
\alias{termPlot,PointProcessModel-method}

\title{termPlot}
\description{
  Plots the linear filter functions for a \code{PointProcessModel}.
}
\usage{
\S4method{termPlot}{PointProcessModel}(model, alpha = 0.05, layer =
geom_line(), trans = NULL, confArg = list(fill = "blue", alpha = 0.2), ...)
}
\arguments{
  \item{model}{an object of class \code{PointProcessModel}.}
  \item{alpha}{a \code{numeric}. One minus the level of the pointwise
      confidence intervals. Default value \code{0.05}. A value of 1
      means no confidence intervals.}
  \item{layer}{a \code{proto} object specifying a ggplot2 layer. Default
    layer \code{geom_line()}.}
  \item{trans}{a function or character string. The function used to transform the linear filter functions
    before plotting.}
    \item{confArg}{a \code{list}. Determines the color of confidence bands.}
  \item{...}{other arguments.}

}
\value{

  A \code{\link{ggplot}} object.
  
}
\details{
  
  The (estimated) linear filter functions in the
  \code{PointProcessModel} are extracted and plotted. Approximate pointwise level \code{1-alpha} confidence intervals
  are also plotted.

  Setting \code{trans} to \code{exp}, the linear filters are
  exponentiated before plotting. This particular transformation is relevant
  for the \code{log}-link function, as this will then provide the
  multiplicative effect of the filter on the intensity as an alternative
  to the \code{log}-additive effect. 
}

\seealso{
  
  \code{\linkS4class{PointProcessModel}}, \code{\link{pointProcessModel}}, \code{\link{ggplot}}
  
}

\author{Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}}
\keyword{aplot}


