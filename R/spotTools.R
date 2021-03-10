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
diff0 <- function(x){
  diff( c(0,x))
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
code2nat <- function(x, a, b){
  a <- matrix(a,1)
  b <- matrix(b,1)
  n <- dim(x)[1]
  A <- matrix( (rep(a, n)), ncol= length(a), nrow = n, byrow=TRUE)
  A + x %*% diag(as.vector(b-a))
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
#' require(babsim.hospital)
#' x <- matrix(rep(-1,29),1,)
#' bounds <- getBounds()
#' lower <- bounds$lower
#' upper <- bounds$upper
#' getNatDesignFromCoded(x, a = lower, b=upper)
#' 
#' @export
getNatDesignFromCoded <- function(x, a, b){
  a <- matrix(a,1)
  b <- matrix(b,1)
  matrix( a+b+ x* (b-a) ,1,)
}


#' @title Return effects for each subgroup
#'   
#' @description  subgroups: returns the table the effects per groups.
#' Code based on the \code{sbgroups} function written by Gilles Pujol for
#' the function \code{\link[sensitivity]{sb}} in the \code{sensitivity} package.
#'   
#' @param x data
#' 
#' @return data frame with groupnames and effects
#'
#' @examples 
#' \donttest{
#' set.seed(2)
#' # Interesting for larger n:
#' n <- 2
#' lower <- c(-0.1, rep(-10,n))
#' upper <- c(0.1, rep(10,n))
#' 
#' # Model-based optimization
#' res <- spot(,funSphere,
#'              lower, upper, 
#'              control=list(funEvals=30,
#'                           optimizer = optimNLOPTR)) 
#'                           
#' # Use the surrogate model for prediction                                                     
#' predictFunKriging <- function(x){
#'       predict(object = res$modelFit, x)
#'       }
#'
#' # Determine sensitivity 
#' sens <- sequentialBifurcation(predictFunKriging,
#'                               lower, upper,
#'                               k=n+1, interaction = TRUE, verbosity = 0)
#' 
#' # Extract group information (variable effects) from sensitivity analysis                                                           
#' ps <- subgroups(sens)
#' colors <- RColorBrewer::brewer.pal(12, "Set3") 
#' barplot(ps$effect, names.arg=ps$group, col= colors) 
#' }
#'         
#' @export
#' 
subgroups <- function(x) {
  ## sort the data on i
  y <- x$y
  y[rank(x$i)] <- y
  if (! is.null(x$ym)) {
    ym <- x$ym
    ym[rank(x$i)] <- ym
  }
  i <- sort(x$i)
  
  ## groups
  n <- length(i)
  lower <- i[1 : (n - 1)] + 1
  upper <- i[2 : n]
  
  ## effects
  if (is.null(x$ym)) {
    effect = diff(y)
  } else {
    effect = (diff(y) - diff(ym)) / 2
  }
  T <- data.frame(lower = lower, upper = upper, effect = effect)
  groupnames <- paste(T[, 1], T[, 2], sep = "-")
  groupnames[T[, 1] == T[, 2]] <- paste(T[T[, 1] == T[, 2], 1])
  return(data.frame(group = groupnames, effect = T[, 3]))
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
                      par = c(10,10),
                      fn,
                      maxit = 100,
                      ...){
  performance <- NULL
  for (i in 1:nrow(algpar)){
    resultList <- optim(par = par,
                        fn = fn,
                        method = "SANN",
                        control = list(maxit = maxit,
                                       temp = algpar[i,1],
                                       tmax = algpar[i,2]))
    performance <- c(performance,resultList$value)
  }
  return(matrix(performance,ncol=1))
}


