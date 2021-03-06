% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/primescount.R
\name{22. Counting the number of primes}
\alias{22. Counting the number of primes}
\alias{primescount}
\alias{primescount.default}
\alias{primescount.numeric}
\alias{primescount.vli}
\title{Counting the Number of Primes Up to a Given Bound}
\usage{
primescount(n, iter = 10, bar = TRUE)

\method{primescount}{default}(n, iter = 10, bar = TRUE)

\method{primescount}{numeric}(n, iter = 10, bar = TRUE)

\method{primescount}{vli}(n, iter = 10, bar = TRUE)
}
\arguments{
\item{n}{upper bound of the interval in which we want to count the number of primes; object of class vli or 32 bits integer}

\item{iter}{number of iterations for each number being tested; numeric}

\item{bar}{to choose if display or not a progress bar; boolean}
}
\description{
The function \code{primescount} returns the number of primes found up to a given bound. The implemented algorithm uses the Miller-Rabin Primality Test to determine whether a number is prime or not. The number of iterations is configurable, to set the desired accuracy.
}
\examples{
\dontrun{
## Counting primes up to 200
primescount(n = 200, iter = 10, bar = TRUE)

## Computing the approximation of pi(x)
pi(200)

## Showing the numbers by using the Solovay-Strassen test
primes(n = 200, iter = 10, test = "SS", bar = TRUE)
}
}
\author{
Javier Leiva Cuadrado
}
