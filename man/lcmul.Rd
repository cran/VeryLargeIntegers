% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lcmul.R
\name{10. Least common multiple}
\alias{10. Least common multiple}
\alias{lcmul}
\alias{lcmul.default}
\alias{lcmul.numeric}
\alias{lcmul.vli}
\title{Least Common Multiple for vli Objects}
\usage{
lcmul(x, y)

\method{lcmul}{default}(x, y)

\method{lcmul}{numeric}(x, y)

\method{lcmul}{vli}(x, y)
}
\arguments{
\item{x}{object of class vli or 32 bits integer}

\item{y}{object of class vli or 32 bits integer}
}
\value{
object of class vli
}
\description{
Computation of the least common multiple of two vli (Very Large Integers) objects.
}
\examples{
x <- as.vli("125634750214756")
y <- as.vli("761048412524216246")
lcmul(x, y)
}
\author{
Javier Leiva Cuadrado
}
