\name{Family-class}
\docType{class}
\alias{Family}
\alias{Family-class}
\alias{family,Family-method}
\alias{makeLink}

\title{Class "Family"}
\description{	 

An object of class \code{Family} specifies the family and 
corresponding \eqn{\phi}{phi} function for a generalized linear point process
model. 

}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Family", name,
  link, phi, Dphi, D2phi)}.

Alternatively, the constructors \code{\link{Hawkes}} and
\code{\link{Gibbs}} can be used. 

}
\section{Slots}{
	 \describe{
    \item{\code{name}:}{Object of class \code{character}.}
    \item{\code{link}:}{Object of class \code{character}.}
    \item{\code{phi}:}{Object of class \code{function}.}
    \item{\code{Dphi}:}{Object of class \code{function}.}
    \item{\code{D2phi}:}{Object of class \code{function}.}
  }
}		
\section{Methods}{
  \describe{
    \item{family}{\code{signature(object = "Family")}: Returns the
      family name as a \code{character}.}
  }   
}
    
\section{Details}{
  The family object is part of the model specification for a generalized
  linear point process model as implemented in the class
  \code{PointProcessModel}. 
  
  Two implemented families are the \code{Hawkes} and the \code{Gibbs}
  family. The link is the "inverse" of the \eqn{\phi}{phi} function
  specified as a text string to facilitate the use of standard
  conventions, e.g. the log-link function corresponds to
  \eqn{\phi=\exp}{phi=exp}.
  
  Implemented standard link functions are, \code{'log'}, \code{'logit'},
  \code{'cloglog'}, \code{'identity'}, \code{'root'},
  \code{'logaffine'}.  The latter two require the specification of the
  additional parameter \code{c}, which by default is 0. For
  \code{'root'} the resulting \eqn{\phi}{phi}-function is

  \deqn{\phi(x) = x^{c+1}1(x >0)}{phi(x) = x^{c+1}1(x > 0)}

  and for \code{'logaffine'}

  \deqn{\phi(x) = \exp(x)1(x \leq c) + \exp(c)(x - c + 1)1(x > c).}{
  phi(x) = exp(x)1(x <= c) + exp(c)(x - c + 1)1(x > c).}

  Using the identity link-function requires a constrained parameter
  space to ensure that the intensity for the point process model is
  positive. For this reason \eqn{\phi(x)=\max\{x,0\}}{\phi(x)=max(x,0)}
  is often a better choice, which is obtained using the \code{'root'} link with
  \code{c=0} (the default).

  Note that in order to prevent taking the logarithm of 0 in the
  computation of the minus-log-likelihood function, and thus preventing
  the minus-log-likelihood from taking the value \code{Inf}, the
  implementation modifies the standard link functions to be bounded
  below by \code{.Machine$double.eps}.

}
    
\author{Niels Richard Hansen, \email{Niels.R.Hansen@math.ku.dk}}

\seealso{
	 \code{\linkS4class{PointProcessModel}}, \code{\link{Hawkes}} and  \code{\link{pointProcessModel}}. 
}
\examples{
showClass("Family")

## The Hawkes family with the log-link.
new("Family", 'Hawkes', link = 'log')
## Or using the constructor.
Hawkes(link = 'log')

## The default for the constructor is the root link.
Hawkes()

## The Hawkes family with the logaffine-link.
Hawkes(link = 'logaffine', c = 1) 
}
\keyword{classes}
