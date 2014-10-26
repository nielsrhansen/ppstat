\name{object.size}
\alias{object.size,ProcessData-method}
\alias{object.size,ContinuousProcess-method}
\alias{object.size,MarkedPointProcess-method}
\title{
 The memory used to store a process data R object.
}
\description{
  This method builds on the function of the same name from the
  \pkg{utils} package, and provides an estimate of the total memory
  consumption attributable to a process data object. 

}
\usage{
\S4method{object.size}{ProcessData}(x)
\S4method{object.size}{ContinuousProcess}(x)
\S4method{object.size}{MarkedPointProcess}(x)
}
\arguments{
  \item{x}{
    a process object.  
  }
  }
\details{
  Several process data objects may share memory as the data are stored by
  reference, for instance if they are all subsets of the same process
  data object. The object size reported by this function is the object's
  memory consumption from the single object's point of view.

  To get the classical memory usage estimate for process data objects
  call \code{utils:::object.size} instead, see the examples. This
  ignores all the data in the environments. 
  
}
\value{
  An object of class \code{"object_size"} with a length-one double value,
  an estimate of the total memory allocation attributable to the object in
  bytes.
}
\author{
  Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}.
}

\seealso{
  \code{\link{object.size}}
}

\examples{
data(example)
object.size(pointExam)
utils:::object.size(pointExam) 

## Subsetting _increases_ the memory estimate for the single object,
## however, the subsetted object does not contain a copy of the data,
## only a reference and the pointers to the subset.  

object.size(pointExam[1:2500,1:4])

## A "copy-through" construction where the data is copied too is
## obtained by calling the constructor. This reduces the memory
## consumption for the single object, but creates a copy of the data. 

object.size(markedPointProcess(pointExam[1:2500,1:4]))

}

\keyword{utilities}
