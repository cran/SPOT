#' @title diff0
#'
#' @family
#' spotTools
#'
#' @description
#' Calculate differences
#' @details
#' Input vector length = output vector length
#'
#' @param x input vector
#'
#' @return vector of differences
#'
#' @examples
#' x <- 1:10
#' diff0(x)
#' @export
diff0 <- function(x) {
  diff(c(0, x))
}


#' @title Transform coded values to natural values
#'
#' @description Input values from the interval from zero to one, i.e., normalized
#' values,  are mapped to the interval from a to b.
#'
#' @param x \code{matrix} of m n-dimensional input values
#' from the interval \code{[0;1]}, i.e, dim(x) = m x n
#' @param a \code{vector} of n-dimensional lower bound, i.e., length(a) = n
#' @param b \code{vector} of n-dimensional upper bound, i.e., length(b) = n
#'
#' @examples
#' x <- matrix(runif(10),2)
#' a <- c(-1,1,2,3,4)
#' b <- c(1,2,3,4,5)
#' R <- code2nat(x,a,b)
#'
#' @export
code2nat <- function(x, a, b) {
  a <- matrix(a, 1)
  b <- matrix(b, 1)
  n <- dim(x)[1]
  A <- matrix((rep(a, n)),
              ncol = length(a),
              nrow = n,
              byrow = TRUE)
  A + x %*% diag(as.vector(b - a))
}

#' @title Get natural parameter values from coded +-1 representation
#'
#' @description For given lower and upper bounds, a and b, respectively,
#' coded input values are mapped to their natural values
#'
#' @param x (n,m)-dim \code{matrix} of coded values, i.e., lower values
#' are coded as -1, upper values as +1.
#' @param a m-dim \code{vector} of lower bounds (natural values)
#' @param b m-dim \code{vector} of upper bounds (natural values)
#'
#' @examples
#' # Note: donttest is used, because platform x86_64-w64-mingw32 (64-bit)
#' # does not provide the package babsim.hospital.
#' \donttest{
#' require(babsim.hospital)
#' x <- matrix(rep(-1,29),1,)
#' bounds <- getBounds()
#' lower <- bounds$lower
#' upper <- bounds$upper
#' getNatDesignFromCoded(x, a = lower, b=upper)
#' }
#' @export
getNatDesignFromCoded <- function(x, a, b) {
  a <- matrix(a, 1)
  b <- matrix(b, 1)
  matrix(a + b + x * (b - a) , 1,)
}



#' @title Interface SANN to SPOT
#'
#' @description Provide an interface for tuning SANN.
#' The interface function receives a \code{matrix} where each row is  proposed parameter setting (`temp`, `tmax`),
#' and each column specifies the parameters.
#' It generates a $(n,1)$-matrix as output, where $n$ is the number of (`temp`, `tmax`) parameter settings.
#'
#' @param algpar \code{matrix} algorithm parameters.
#' @param par Initial values for the parameters to be optimized over.
#' @param fn A function to be minimized (or maximized),
#' with first argument the vector of parameters over which minimization is to take place. It should return a scalar result.
#' @param maxit Total number of function evaluations: there is no other stopping criterion. Defaults to 10000.
#' @param ... further arguments for \code{optim}
#'
#' @return \code{matrix} of results (performance values)
#'
#' @examples
#' sphere <- function(x){sum(x^2)}
#' algpar <- matrix(c(1:10, 1:10), 10,2)
#' sann2spot(algpar, fn = sphere)
#'
#' @export
sann2spot <- function(algpar,
                      par = c(10, 10),
                      fn,
                      maxit = 100,
                      ...) {
  performance <- NULL
  for (i in 1:nrow(algpar)) {
    resultList <- optim(
      par = par,
      fn = fn,
      method = "SANN",
      control = list(
        maxit = maxit,
        temp = algpar[i, 1],
        tmax = algpar[i, 2]
      )
    )
    performance <- c(performance, resultList$value)
  }
  return(matrix(performance, ncol = 1))
}


#' @title Transform input
#'
#' @description Transform input variables
#'
#' @param xNat \code{matrix} with natural variables. Default: \code{NA}.
#' @param fn vector of transformation functions names (\code{char}). Default: Empty
#' vector (\code{vector()}).
#'
#' @return \code{matrix} of transformed parameters
#'
#' @examples
#' f2 <- function(x){2^x}
#' fn <- c("identity", "exp", "f2")
#' xNat <- diag(3)
#' transformX(xNat, fn)
#'
#' fn <- append(fn, c("sin", "cos", "tan"))
#' xNat <- cbind(xNat, xNat)
#' transformX(xNat, fn)
#' @export
transformX <- function(xNat = NA,
                       fn = vector()) {
  if (length(fn) != ncol(xNat)) {
    print(fn)
    print(length(fn))
    print(xNat)
    stop(
      "transformX() in spotTools.R:
         Number of functions in fn and number of columns in xNat do not match."
    )
  }
  res <- matrix(data = NA,
                nrow = nrow(xNat),
                ncol = ncol(xNat))
  res <- tryCatch(
    expr = {
      for (ii in 1:ncol(xNat)) {
        res[, ii] <- eval(parse(text = fn[ii]))(xNat[, ii])
      }
      return(res)
    },
    error = function(e) {
      message("transformX() in spotTools.R: Caught an error!")
      print(e)
    }
  )
  return(res)
}


#' @title Get Multi Start Points
#'
#' @description Determine multi start points for optimization on the surrogate.
#' Combines the current best with additional random starting points for
#' optimization on the surrogate.
#'
#' @param x matrix of design points
#' @param y matrix of function values (f(x))
#' @param control Control list for \code{\link{spot}} and \code{\link{spotLoop}}.
#' Generated with \code{\link{spotControl}}.
#'
#' @return x0 matrix of restart points
#' @export
getMultiStartPoints <- function(x, y, control) {
  ## problem dimension
  n <- ncol(x)
  ## 1. Generate additional random points
  x0 <- matrix(NA, nrow = control$multiStart, n)
  if (control$multiStart > 1) {
    ## copy global control list and adapt size (number of restarts)
    designMultiStart <- control$designControl
    designMultiStart$size <- control$multiStart
    ## replicates should not be used here
    designMultiStart$replicates <- 1
    
    x0 <- control$design(
      x = NULL,
      lower = control$lower,
      upper = control$upper,
      control = designMultiStart
    )
  }
  
  ## 2. Determine current xbest
  indexBest <- which.min(y[, 1, drop = FALSE])
  xbest <- x[indexBest, , drop = FALSE]
  ## determine whether xbest can be used as a starting point x0
  if (checkFeasibilityNlopGnIngres(x = xbest, control = control)) {
    if(control$verbosity>0){
    print("No constraints active OR (constraints ative AND xbest feasible)")}
      if (control$multiStart > 1) {
      x0[1, ] <- xbest
    } else{
      x0 <- xbest
    }
    }
  else{
    if(control$verbosity>0){
    print("xbest not feasible")
    }
    if (control$multiStart == 1) {
      x0 <- NULL
    }
  }
  ## Rounding values produced by the design function to integers, etc.
  x0 <- repairNonNumeric(x0, control$types)
  return(x0)
}

#' @title Check feasibility for NLOPT_GN_ISRES
#' @description Returns \code{TRUE} if x does satisfy ineq constraint OR
#' no constraint function is used
#' @param x (1 x n)-matrix to be tested
#'
#' @param control Control list for \code{\link{spot}} and \code{\link{spotLoop}}.
#'
#' Generated with \code{\link{spotControl}}.
#' @return logical (\code{TRUE} if feasible)
#'
#' @export
checkFeasibilityNlopGnIngres <- function(x,
                                         control) {
      if(is.null(control$optimizerControl$eval_g_ineq)){
      return(TRUE)
    }else{
      return(control$optimizerControl$opts$algorithm == "NLOPT_GN_ISRES"  &
          control$optimizerControl$eval_g_ineq(x) < 0)
    }
}
