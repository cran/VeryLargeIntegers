% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pi.R
\name{21. Pi function}
\alias{21. Pi function}
\alias{pi}
\alias{pi.default}
\alias{pi.numeric}
\alias{pi.vli}
\title{Pi Function Approximation for vli Objects}
\usage{
pi(x)

\method{pi}{default}(x)

\method{pi}{numeric}(x)

\method{pi}{vli}(x)
}
\arguments{
\item{x}{positive integer; vli class object or 32 bits integer}
}
\value{
number of primes up to \code{x}; object of class vli
}
\description{
Pi function approximation for vli (Very Large Integers) objects. It is also called "Prime-counting function".
Given a positive integer \code{x}, the Pi function returns the number of primes up to \code{x}.
}
\details{
The implemented algorithm is based in the fact that \code{x/log(x)} is asymptotically equal to \code{pi(x)}, also known as "Prime Number Theorem".

Closer approximations could be implemented by using the Logarithmic Integral Function. The function \code{countprimes} of the present package is another way to get a better approximation (in return for a less efficient computation) of \code{pi(x)}. Alhought the algorithm is not deterministic, it is based in the Miller-Rabin Probabilistic Primality Test, therefore the error can be arbitrarily reduced.
}
\examples{
x <- as.vli("89235489145293876129784691")
pi(x)
}
\author{
Javier Leiva Cuadrado
}