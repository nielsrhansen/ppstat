\name{NEWS}
\title{ppstat News}
\section{Changes in ppstat version 0.9.5}{
  \subsection{New features}{
    \itemize{
      \item The code from the \code{processData} R package has been included in the 
      this package. The \code{processData} package is no longer needed.
      \item The \code{Ogata} function and the \code{simulate} method for the 
      \code{MultivariatePointProcess} class are introduced to simulate multivariate 
      point processes using the Ogata thinning algorithm.
      \item Support for automatic model selection and multivariate modeling 
      using \code{ppKernel} and \code{ppSmooth}.
    }
  }
}

\section{Changes in ppstat version 0.9}{
  \subsection{New features}{
    \itemize{  
      \item Function \code{ppKernel} added, which implements non-parametric 
      estimation of filter functions in reproducing kernel Hilbert spaces.
      \item Function \code{ppSmooth} replaces \code{pointProcessSmooth} (with 
      a few changes in the argument list, but quite a few changes in the internals).
      It implements non-parametric estimation of smooth filter functions using 
      B-spline basis expansions. 
      \item Function \code{ppLasso} added. It relies on \code{glmnet}.
  }
  }
  \subsection{Other Changes}{
    \itemize{
      \item Argument \code{fixedCoefficients} for \code{pointProcessModel} has been 
      removed. In future versions the formula interface for model specification 
      will support the use of \code{offset}, but for the current version there is 
      no longer support for fixed components in the linear predictor. 
      \item Argument \code{Omega} for \code{pointProcessModel} has been replaced 
      by \code{lambda}. The function no longer supports general quadratic penalty 
      matrices but only diagonal penalization. 
    }
  }
}

\section{Changes in ppstat version 0.8}{
  \subsection{New features}{
    \itemize{
      \item The class \code{MultivariatePointProcess} has been
      introduced, which contains a list of
      \code{PointProcessModel}s. Each element in the list is a
      model of one coordinate given one or more of the other
      coordinates, and in total the list comprises a multivariate model
      of point processes. 
      \item Methods such as \code{summary}, \code{termPlot},
      \code{stepInformation} and \code{ppmFit} have been implemented for
      the \code{MultivariatePointProcess} class.
      \item The \code{pointProcessModel} function now interprets a
      vector of variables on the left hand side of a formula as the
      specification of a multivariate point process model in which case
      the function returns an object of class
      \code{MultivariatePointProcess}.
    }
  }
   \subsection{Other Changes}{
     \itemize{
       \item The implementation of the linear filters has been modified
       slightly concerning the treatment of the boundary.
       }
}
}


