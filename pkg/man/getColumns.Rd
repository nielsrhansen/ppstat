\name{getColumns}
\alias{getColumns}
\alias{getColumns,ProcessData,numeric-method}
\alias{getColumns,ProcessData,character-method}
\alias{getColumns,ProcessData,logical-method}
\alias{getColumns,ContinuousProcess,numeric-method}
\alias{getColumns,ContinuousProcess,character-method}
\alias{getColumns,MarkedPointProcess,numeric-method}
\alias{getColumns,MarkedPointProcess,character-method}
\alias{$,ProcessData-method}
\title{
  Extraction of column(s) from a process data object.
}
\description{
  The function \code{getColumns} is the fundamental function for extraction
  of column data from a process data object. Single columns can be
  extracted as vectors or multiple columns as lists of vectors. 
}
\usage{
\S4method{getColumns}{ProcessData,numeric}(object, j, drop = TRUE)
\S4method{getColumns}{ProcessData,character}(object, j, drop = TRUE)
\S4method{getColumns}{ProcessData,logical}(object, j, drop = TRUE)

\S4method{$}{ProcessData}(x, name)
}
\arguments{
  \item{object}{
    a process object extending class \code{ProcessData}. 
  }
   \item{x}{
    a process object extending class \code{ProcessData}. 
  }
  \item{j}{
    The column(s) to extract.
  }
  \item{drop}{
    a \code{logical}. Should a single column be returned
    as a vector instead of a list of length 1. Default value is \code{TRUE}.
  }
  \item{name}{
    a variable name.
  }
}
\details{
  It is possible to extract single column data by name using the
  \code{$} operator or one or multiple columns by index using the
  \code{getColumns} function.

  The drop argument is primarily provided for programming to ensure that
  the function always returns a list.

  Single columns can, in addition, be extracted using the bracket \code{[} with
  the \code{drop} argument equal to \code{TRUE}, cf. the examples
  below. 

}
\value{
 A list of the column values or a single column as a vector. 
}

\author{
  Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}.
}

\seealso{
  \code{\linkS4class{MarkedPointProcess}},
  \code{\linkS4class{ContinuousProcess}},
  \code{\link{subset,ProcessData-method}}
}
\examples{
data(example)
identical(getColumns(pointExam, "foo"), pointExam$foo)
identical(getColumns(pointExam, 2), pointExam[ , 2, drop = TRUE])
pointExam$ALPHA
getColumns(pointExam, c(4,5))
}
\keyword{manip}
