#' @title Interface to minimization based on Gramacy's lagp package
#' 
#' @description  Implements Gramacy's plgp package based optimization using expected improvement.
#' Example from chapter 7 in the surrogate book.
#'
#' @param x optional matrix of points to be included in the evaluation
#' @param fun objective function, which receives a matrix x and returns observations y
#' @param lower boundary of the search space
#' @param upper boundary of the search space
#' @param control list of control parameters
#' \describe{
#'   \item{\code{funEvals}}{Budget, number of function evaluations allowed. Default: 100.}
#'   \item{\code{retries}}{Number of retries for design generation, used by \code{\link{designLHD}}. Default: 100.}
#' }
#' @param ... passed to \code{fun}
#'
#' @return list, with elements
#' \describe{
#'   \item{\code{x}}{archive of evaluated solutions}
#'   \item{\code{y}}{archive of observations}
#'   \item{\code{xbest}}{best solution}
#'   \item{\code{ybest}}{best observation}
#'   \item{\code{count}}{number of evaluations of \code{fun}}
#'   \item{\code{message}}{success message}
#' }
#'
#' @examples
#' res <- optimLHD(,fun = funSphere,lower = c(-10,-20),upper=c(20,8))
#' res$ybest
#' @export
optimLagp <-function(x=NULL,fun,lower,upper,control=list(),...){
	#if (length(par)==0) stop("dimension of par is null")
	con<-list(funEvals=100,retries=100,types= rep("numeric",length(lower)))
	con[names(control)] <- control
	control<-con
	  
  if(is.null(x)){
    k=0
  }else{
    k=nrow(x)
    if(k>=control$funEvals){
      stop("Design size in optimLHD is zero or negative, due to large number of rows of user-specified x matrix.")
    }
  }
  
  x <- designLHD(x, lower, upper, control=list(size=control$funEvals-k,retries=control$retries,types=control$types)) 
  y <- matrix(fun(x,...),,1)
  indexBest <- which.min(y)
  list(x=x,y=y,xbest=x[indexBest,,drop=FALSE],ybest=y[indexBest,,drop=FALSE],count=control$funEvals,msg="success")
}


#' @title Expected improvement (Gramacy)
#' @param gpi Gaussian process C-side object 
#' @param x matrix of points to calculate EI
#' @param fmin best function value (y) so far
#' @param pred prediction model. Default: \code{predGPsep}
#' 
#' @return ei expected improvement
#' 
#' @examples 
#' 
#' library(laGP)
#' library(plgp)
#' 
#' ninit <- 12
#' dim <- 2
#' X <- designLHD(,rep(0,dim), rep(1,dim), control=list(size=ninit))
#' y <- funGoldsteinPrice(X)
#' m <- which.min(y)
#' ymin <- y[m]
#' start <- matrix(X[m,], nrow =1)
#' 
#' ## 1. Build SPOT BO Model
#' m1 <- buildBO(x = X, y = y, control = list(target="ei"))
#' yy <- predict(object = m1, newdata = start)
#' ei1 <- matrix(yy$ei, ncol = 1)
#' ## Show mue and s
#' mue <- matrix(yy$y, ncol = 1)
#' s2 <- matrix(yy$s, ncol = 1)
#' 
#' ## 2. Build laGP model
#' gpi <- newGPsep(X, y, d=0.1, g=1e-8, dK=TRUE)
#' da <- darg(list(mle=TRUE, max=0.5), designLHD(,rep(0,dim), rep(1,dim), control=list(size=1000)))
#' mleGPsep(gpi, param="d", tmin=da$min, tmax=da$max, ab=da$ab)
#' ei2 <- plgpEI(gpi=gpi, x=start, fmin=ymin)
#' deleteGPsep(gpi)
#' 
#' @export
plgpEI <- function(gpi, x, fmin, pred=predGPsep)
{
  if(is.null(nrow(x))) x <- matrix(x, nrow=1)
  p <- pred(gpi, x, lite=TRUE)
  d <- fmin - p$mean
  sigma <- sqrt(p$s2)
  dn <- d/sigma
  ei <- d*pnorm(dn) + sigma*dnorm(dn)
  return(ei)
}

#' @title Wrapper for Expected improvement (Gramacy)
#' @param x matrix of points to calculate EI
#' @param fmin best function value (y) so far
#' @param gpi Gaussian process C-side object 
#' @param pred prediction model. Default: \code{predGPsep}
#' @seealso \code{\link{plgpEI}}.
#' 
#' @return negative expected improvement
#' 
#' @examples 
#' 
#' library(laGP)
#' library(plgp)
#' 
#' ninit <- 12
#' dim <- 2
#' X <- designLHD(,rep(0,dim), rep(1,dim), control=list(size=ninit))
#' y <- funGoldsteinPrice(X)
#' m <- which.min(y)
#' ymin <- y[m]
#' start <- matrix(X[m,], nrow =1)
#' 
#' ## Build laGP model
#' gpi <- newGPsep(X, y, d=0.1, g=1e-8, dK=TRUE)
#' da <- darg(list(mle=TRUE, max=0.5), designLHD(,rep(0,dim), rep(1,dim), control=list(size=1000)))
#' mleGPsep(gpi, param="d", tmin=da$min, tmax=da$max, ab=da$ab)
#' res <- optim(start[1,], obj.plgpEI, method="L-BFGS-B", lower=0, upper=1, 
#' gpi=gpi, pred=predGPsep, fmin=ymin)
#' xnew <- c(res$par, -res$value)
#' print(xnew)
#' deleteGPsep(gpi)
#' 
#' @export
obj.plgpEI <- function(x, fmin, gpi, pred=predGPsep){
  - plgpEI(gpi, x, fmin, pred)
}
