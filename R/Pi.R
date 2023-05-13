

# Pi function approximation  (number of primes up x)

#' @title Pi Function Approximation for vli Objects
#' @author Javier Leiva Cuadrado
#' @param x positive integer; vli class object or 32 bits integer
#' @return number of primes up to \code{x}; object of class vli
#' @description Pi function approximation for vli (Very Large Integers) objects. It is also called "Prime-counting function".
#' Given a positive integer \code{x}, the Pi function returns the number of primes up to \code{x}.
#' @details The implemented algorithm is based in the fact that \code{x/log(x)} is asymptotically equal to \code{Pi(x)}, also known as "Prime Number Theorem".
#'
#' Closer approximations could be implemented by using the Logarithmic Integral Function. The function \code{countprimes} of the present package is another way to get a better approximation (in return for a less efficient computation) of \code{Pi(x)}. Alhought the algorithm is not deterministic, it is based in the Miller-Rabin Probabilistic Primality Test, therefore the error can be arbitrarily reduced.
#' @examples x <- as.vli("89235489145293876129784691")
#' Pi(x)
#' @name 21. Pi function
#' @rdname Pi
#' @export Pi
#'
Pi <- function(x) UseMethod("Pi")


#' @rdname Pi
#' @method Pi default
#' @export Pi default
#'
Pi.default <- function(x) stop("The object passed as argument is neither a vli object nor a 32 bits integer")


#' @rdname Pi
#' @method Pi numeric
#' @export Pi numeric
#'
Pi.numeric <- function(x){
  if ( abs(x) < 2147483648 ){
    if (x > 0){
      x = vliC(toString(as.integer(x)))
    }
    else stop("x has to be a positive integer")
  }
  else stop("The object passed as argument is neither a vli object nor a 32 bits integer")
  divbaseC(x, loge(x))[[1]]
}


#' @rdname Pi
#' @method Pi vli
#' @export Pi vli
#'
Pi.vli <- function(x){
  if ( ltC(x, .pkgenv$one) ) stop("x has to be a positive integer")
  divbaseC(x, loge(x))[[1]]
}
