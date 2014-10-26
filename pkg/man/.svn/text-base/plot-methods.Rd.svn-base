\name{plot-methods}
\alias{plot,ContinuousProcess-method}
\alias{plot,ContinuousProcess,missing-method}
\alias{plot,ContinuousProcess,character-method}
\alias{plot,ContinuousProcess,numeric-method}
\alias{plot,JumpProcess-method}
\alias{plot,JumpProcess,missing-method}
\alias{plot,MarkedPointProcess-method}
\alias{plot,MarkedPointProcess,missing-method}
\alias{plot,MarkedPointProcess,character-method}
\alias{plot,ProcessPlotData,missing-method}
\alias{plot,ProcessPlotData-method}
\alias{getPlotData,ContinuousProcess-method}
\alias{getPlotData,JumpProcess-method}
\alias{getPlotData,MarkedPointProcess-method}
\alias{getPlotData,MarkedPointProcess,character-method}
\alias{getPlotData}
\title{Methods for 'plot' in Package 'processdata'}
\description{
  Methods for plotting process data contained in objects of class
  \code{ContinuousProcess},
  \code{MarkedPointProcess} or
  \code{JumpProcess}.
}
\usage{
\S4method{plot}{ContinuousProcess}(x, nPoints = 200, ...)

\S4method{plot}{MarkedPointProcess,character}(x, y, nPoints = 200, ...)

\S4method{plot}{JumpProcess}(x, nPoints = 200, ...)

\S4method{plot}{ProcessPlotData}(x, ...)

\S4method{getPlotData}{ContinuousProcess}(object, y = '@bottom', nPoints = 200,
allUnitData = FALSE, selectPoints = NULL, dropLevels = 1, ...)

\S4method{getPlotData}{MarkedPointProcess,character}(object, y = '@mark', nPoints = 200, allUnitData = FALSE,
allMarkValueData = isTRUE(y \%in\% names(getMarkValue(object))), ...)

\S4method{getPlotData}{JumpProcess}(object, nPoints = 200, ...)
}
\arguments{

  \item{x}{ process object to be plotted.  }

  \item{y}{ a \code{character}. Specifies how the points are plotted for
    a \code{MarkedPointProcess} object. Possible values are \code{'@mark'},
    \code{'@top'}, \code{'@bottom'}, a name of a continuous process variable, a name of a mark
    value variable, the unit identification variable name or a numeric. See Details
    for further explanations. If missing, the value is taken as \code{'@mark'}.}
  
  \item{object}{ process object for which to compute a data set for
    plotting.  }

  \item{nPoints}{ a \code{numeric}. Specifies the number of grid
    points for each process to include in the data set that is
    plotted. Default value is 200.  }
  
  \item{allUnitData}{ a \code{logical}. Specifies if non-process data
    should be included in the data set for plotting. Default value
    depends upon whether \code{y} is a mark value or not. }

  \item{allMarkValueData}{ a \code{logical}. Specifies if mark value data
    should be included in the data set for plotting. Default value is
    \code{FALSE}. If }
  
  \item{selectPoints}{ a \code{numeric} vector or \code{NULL}. Grid
    points to include in the plot in addition to the \code{nPoints}.
  }

  \item{dropLevels}{ a \code{numeric} vector or \code{list} of
    vectors. Specifies one or more levels to be dropped when plotting
    factor columns. Default value is 1, which means that the first
    level is dropped from the plot.  }

  \item{...}{ other arguments.}
}

\value{
  \code{plot} returns a \code{\link{ggplot}} object.

  \code{getPlotData} returns an object of class \code{ProcessPlotData},
  which contains the data that is actually plotted. 
  
}

\details{

  The underlying plotting engine is \code{ggplot} from the \pkg{ggplot2}
  package. The result of a call to \code{plot} is thus a \code{ggplot}
  object, which can be altered and plotted like any other \code{ggplot}
  object.
  
  The default plot is a plot of the (multivariate) process data held in
  the process object \code{x} divided according to the observation units
  -- the individuals. This is most suitable with relatively few units.

  The \code{plot} methods rely on the data structure to determine
  sensible aesthetic mappings of the data for visualization. Hence,
  numeric columns in a \code{ContinuousProcess} object are plotted as
  continuous sample paths of processes, factor columns are plotted as
  bars at different levels and event columns in a
  \code{MarkedPointProcess} object are plotted as points. We refer to
  the former as a \sQuote{Continuous} type data and the latter two as
  \sQuote{Track} data. The \code{plot} methods attempt to compute a
  sensible y-axis to plot either one of the types of data or the
  combination of \sQuote{Continuous} and \sQuote{Track} data.

  Modifications to the resulting plot can be done by a combination of
  arguments to the \code{plot} methods or subsequent manipulations of
  the \code{ggplot2} objects. For instance, with many units and \code{p}
  the value of the \code{plot} call, the plot

  \code{p + facet_null()}

  can be a useful alternative to the default. For \code{ContinuousProcess} objects
  with many units and few variables, one can also consider 

  \code{p + facet_grid(variable ~ .)}

  while for a \code{MarkedPointProcess} a possible alternative is
  
  \code{plot(object, y = "id") + facet_grid(variable ~ ., scale = "free_y")}
  
  For \code{JumpProcess} objects one can in principle proceed as for a
  \code{ContinuousProcess} objects. However, when plotting the process
  with discontinuities at the jump times the curve for each unit becomes
  disconnected. A better alternative then, assuming that the unit
  identification variable is \code{id}, is

  \code{p + facet_grid(variable ~ .) + aes(color = id)}
 
   See \code{demo(examples, package = "ppstat")} for examples. Some
   aspects of the \code{ggplot} objects are not that easily modified after
   the object has been created. See below for how to deal with some examples. 

   The \code{y} argument controls the y-values used for plotting the
   point process part of the data set when plotting a
   \code{MarkedPointProcess} object. Values \code{'@top'} and
   \code{'@bottom'} place the points above or below, respectively, the
   plot of the \code{ContinuousProcess} part of the object. A numeric
   value of \code{y}, e.g. 0, results in a plot with all points at this
   particular y-value. A name of a mark value variable places the points
   at their corresponding mark value and a name of a continuous process
   variable places the points on the continous process.  The default,
   \code{'@mark'}, places the points at different levels on the y-axis
   corresponding to the different marks, and the unit identification
   variable name places the points at different levels on the y-axis
   corresponding to the different units.

   As descriped above, the factor columns of a process data object are
   plotted with bars at the different levels of the factor. The
   \code{dropLevels} argument works like the specification of contrasts
   in a linear model parametrization. That is, the default value of 1
   means that the first level is dropped in the plot and bars are
   plotted for the other levels.

   Technically, a call to \code{plot} for either of the S4 process data
   classes above is a two step procedure. First a call to
   \code{getPlotData} that returns the \code{ProcessPlotData} object,
   and then a call of the \code{plot} method for that object, which
   constructs the resulting \code{ggplot2} object. The
   \code{ProcessPlotData} class is, however, currently not exported, nor
   is the \code{getPlotData} methods. The user only needs to
   understand that additional arguments to \code{plot} should be named
   and are passed on to the \code{getPlotData} data extractors
   and the \code{plot} method for the \code{ProcessPlotData} object.

   The \sQuote{Track} types of data are plotted as layers on the plot
   with their own data sets. In general, any argument for
   \code{geom_point}, e.g. \code{shape}, used to create the event points
   layer can be given as an argument of the form \code{point_shape}
   appending \sQuote{point_} to the argument. Any argument for
   \code{geom_line}, e.g. \code{size}, used to create the factor bars
   layer can be given as an argument of the form \code{factor_size}
   appending \sQuote{factor_} to the argument. See the examples below.
   }

\seealso{

   \code{\linkS4class{ProcessData}}, \code{\linkS4class{ContinuousProcess}},
   \code{\linkS4class{MarkedPointProcess}},
   \code{\linkS4class{JumpProcess}}, 
   \code{\linkS4class{ProcessPlotData}}, 
   \code{\link{ggplot}}
 }
 
 \author{Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}}

 \examples{
## Examples of plot usages are collected in a demo.
\dontrun{
demo(examples, package = "processdata")}
## Certain parameters used in the layers for the tracks can be specified
## when calling the plot method. 
\dontrun{
data(example, package = "processdata")
plot(pointExam, point_size = 4, point_shape = 15)
plot(factExam, factor_size = 1, factor_alpha = 0.5)}
}
 
\keyword{methods}
\keyword{aplot}