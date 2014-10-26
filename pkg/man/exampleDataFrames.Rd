\name{ExampleDataFrames}
\alias{exampleDataFrames}
\alias{unitData}
\alias{continuousData}
\alias{pointData}

\docType{data}
\title{
Two small example data sets.
}
\description{

  The data sets are small, simulated data sets used to illustrate the
  functionality of the \code{processdata} data structure, subsetting and
  plotting methods.

}
\usage{exampleDataFrames}

\format{ The three data frames
  \code{unitData}, \code{continuousData} and \code{pointData} contain
  the data for the \code{contExam} and \code{pointExam} from the
  \code{example} data set, and can be used
  for the contruction of these.
 
}

\seealso{\code{\link{example}}}
\examples{
data(exampleDataFrames)
continuousProcess(continuousData, unitData)
markedPointProcess(pointData, continuousData, unitData = unitData)

}
\keyword{datasets}