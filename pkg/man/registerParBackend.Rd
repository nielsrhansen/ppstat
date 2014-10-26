\name{registerParBackend}
\alias{registerParBackend}
\alias{lapplyParallel}
\alias{getRegisteredParBackend}
\title{
Parallel backend for the \pkg{ppstat} package.
}
\description{

  The function \code{registerParBackend} sets up a parallel backend to be
  used by several functions in the \pkg{ppstat} package. 
  
}
\usage{
registerParBackend(backend = 'mc', cores = NULL)

lapplyParallel(\dots)

getRegisteredParBackend()
}
\arguments{
  \item{backend}{ a \code{character}. The default value,
     \code{'mc'}, relies on the \pkg{parallel} package. Use
    \code{'sequential'} to reset the registration for sequential
    execution. }
  \item{cores}{ a \code{numeric}. The number of cores to use. If
    \code{NULL}, the default, the number of cores is determined
    automatically.}
  \item{\dots}{ arguments that are to be passed on to the actual
    \code{lapply} function used in the parallel execution of the \code{lapply}
    function. }
}
\details{

  The effect of calling \code{registerParBackend()} is to substitute the
  call to the \code{lapply} function at various places in the code by a
  call to a function that can execute the \code{lapply} call in
  parallel. Currently the only supported backend is based on the
  \pkg{parallel} package and uses the \code{mclapply} function from
  that package. The registration function checks if the R session is
  appropriate for using \pkg{parallel}, i.e. that R does not run in a
  GUI. See the \pkg{parallel} and \code{mclapply} documentation for details.

  When the package \pkg{ppstat} loads it checks if a global option
  \code{ppstatParallel} is found and is equal to \code{TRUE}. If it is,
  \code{registerParBackend()} is called at load time. Otherwise it
  has to be called in the session before \pkg{ppstat} can take advantage
  of multiple cores. The global option can be set using
  \code{options(ppstatParallel = TRUE)} e.g. in the \command{.Rprofile}
  file for automatic registration of the parallel backend at package
  load time.

  The function \code{lapplyParallel} is a wrapper responsible for
  calling the appropriate, registered \code{lapply} function, which does
  the actual parallel execution of the \code{lapply} call. The function
  is not exported and intended for internal use only. 
  
}
\value{
  
  \code{registerParBackend} returns \code{NULL} invisibly. The function
  is invoked for its side effects.

  \code{lapplyParallel} returns a \code{list}.

  \code{getRegisteredParBackend} returns a character specifying the
  current registered parallel backend. Currently either \code{'sequential'} or
  \code{'mc'}. 
}

\section{Warning}{

  The parallel execution of computations in \pkg{ppstat} are
  experimental and the \code{mclapply} function from the \pkg{parallel}
  package does not work on Windows.

  Some unstable behaviour has also been encountered on Mac OS X whenever
  the parallel computations are run in a session where the 
  \code{quartz} device for plotting has been used previously. Though non
  of the parallel parts uses on screen devices, the unstable
  behaviour has only been encountered when the \code{quartz} device
  has been used in the session, and a note is printed when the parallel
  backend is registered and the \code{quartz} device is the current
  device. 
  
  }

\author{
  Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}
}

\seealso{
  \pkg{\link{parallel}}, \code{\link{mclapply}}, \code{\link{options}}.
}
\keyword{utilities}
