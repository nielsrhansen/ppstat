\name{subset}
\alias{unsubset}
\alias{unsubset,ProcessData-method}
\alias{unsubset,ContinuousProcess-method}
\alias{unsubset,MarkedPointProcess-method}
\alias{subset,ProcessData-method}
\alias{[,ProcessData-method}
\alias{[,ProcessData,ANY,ANY,ANY-method}
\alias{[,ProcessData,missing,missing,missing-method}
\alias{[,ProcessData,integer,missing,ANY-method}
\alias{[,ProcessData,numeric,missing,ANY-method}
\alias{[,ProcessData,logical,missing,ANY-method}
\alias{[,ProcessData,character,missing,ANY-method}
\alias{[,ProcessData,missing,integer,ANY-method}
\alias{[,ProcessData,missing,logical,ANY-method}
\alias{[,ProcessData,missing,character,ANY-method}
\alias{[,ProcessData,missing,numeric,ANY-method}
\alias{subset,ContinuousProcess-method}
\alias{[,ContinuousProcess-method}
\alias{[,ContinuousProcess,ANY,ANY,ANY-method}
\alias{[,ContinuousProcess,missing,missing,missing-method}
\alias{[,ContinuousProcess,integer,missing,ANY-method}
\alias{[,ContinuousProcess,numeric,missing,ANY-method}
\alias{[,ContinuousProcess,logical,missing,ANY-method}
\alias{[,ContinuousProcess,character,missing,ANY-method}
\alias{[,ContinuousProcess,missing,integer,ANY-method}
\alias{[,ContinuousProcess,missing,logical,ANY-method}
\alias{[,ContinuousProcess,missing,character,ANY-method}
\alias{[,ContinuousProcess,missing,numeric,ANY-method}
\alias{subset,MarkedPointProcess-method}
\alias{[,MarkedPointProcess-method}
\alias{[,MarkedPointProcess,ANY,ANY,ANY-method}
\alias{[,MarkedPointProcess,missing,missing,missing-method}
\alias{[,MarkedPointProcess,integer,missing,ANY-method}
\alias{[,MarkedPointProcess,numeric,missing,ANY-method}
\alias{[,MarkedPointProcess,logical,missing,ANY-method}
\alias{[,MarkedPointProcess,character,missing,ANY-method}
\alias{[,MarkedPointProcess,missing,integer,ANY-method}
\alias{[,MarkedPointProcess,missing,logical,ANY-method}
\alias{[,MarkedPointProcess,missing,character,ANY-method}
\alias{[,MarkedPointProcess,missing,numeric,ANY-method}
\alias{subset,JumpProcess-method}
\alias{[,JumpProcess-method}
\alias{[,JumpProcess,ANY,ANY,ANY-method}

\title{Subsetting}

\description{
  Methods for subsetting using \code{[,]} and \code{subset} for process data objects.
}

\usage{
\S4method{[}{ProcessData}(x, i, j, ..., drop = FALSE)

\S4method{[}{ContinuousProcess}(x, i, j, ..., drop = FALSE)

\S4method{[}{MarkedPointProcess}(x, i, j, ..., drop = FALSE)

\S4method{[}{JumpProcess}(x, i, j, ..., drop = FALSE)

\S4method{subset}{ProcessData}(x, subset, select, ...)

\S4method{subset}{ContinuousProcess}(x, subset, select, ...)

\S4method{subset}{MarkedPointProcess}(x, ..., pointSubset)

\S4method{subset}{JumpProcess}(x, subset, select, ...)

\S4method{unsubset}{ProcessData}(x, ...)

\S4method{unsubset}{ContinuousProcess}(x, ...)

\S4method{unsubset}{MarkedPointProcess}(x, ...)

}

\arguments{

  \item{x}{ a process object.}
 
  \item{i}{ a \code{numeric}, \code{logical} or
  \code{character}.}

  \item{j}{ a \code{numeric}, \code{logical} or
  \code{character}.}

  \item{drop}{ a \code{logical}. Detault value \code{FALSE}.}

  \item{subset}{ an expression given in terms of the variables in the
     data set that evaluates to \code{logical}. See details for further
     explanations.}

   \item{select}{ an expression that evaluates to \code{logical} in a
    frame consisting of the column names.}

\item{pointSubset}{ an expression given in terms of the mark value variables in
the data set that evaluates to \code{logical}. The argument must be
named.}

    \item{...}{other arguments. In particular, the \code{subset} method
for \code{MarkedPointProcess} takes the two arguments \code{subset} and
\code{select} and passes them to the \code{ContinuousProcess}
method. } 
}

\details{

Bracket subsetting works basically as if the process data was a big
data frame with variable names as the column names. All subsetting is,
however, done by reference meaning that the data is not copied but
pointers to the subset are computed. 

The \code{subset} method works in the same ways as the \code{subset} function 
for data frames, but relying on the reference based bracket subsetting. This
allows for an \code{unsubset} method that brings the full data set back
from a subset.  

}

\value{

  A process object of the same class as \code{x} unless \code{drop =
  TRUE}. If \code{drop = TRUE} the object is dropped to a class further down
  the class hierarchy if appropriate. For instance, subsetting to column entries
  for a \code{MarkedPointProcess} such that all mark columns are lost
  results in an object of class \code{ContinuousProcess} if  \code{drop =
    TRUE}.


 
}

\author{
  Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}
  }

\seealso{
\code{\linkS4class{ContinuousProcess}},
\code{\linkS4class{MarkedPointProcess}},
\code{\linkS4class{JumpProcess}},
\code{\link{subset}},
\code{\link{[}}

}

\examples{
data(example)
contExam[1:1500, ]
subset(pointExam, time > 4 & gender == "M")

contExam[ ,2:3]

colNames(pointExam)
pointExam[ , c("gender", "foo", "ALPHA")]
pointExam[ , c("gender", "foo"), drop = TRUE]
subset(pointExam, select = gender:BETA)




}
\keyword{manip}
