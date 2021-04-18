#' Bayesian Optimization Model Interface
#'
#' @param x matrix of input parameters. Rows for each point, columns for each parameter. 
#' @param y one column matrix of observations to be modeled.
#' @param control list of control parameters
#' 
#' @importFrom stats optim
#' @importFrom plgp covar.sep
#' 
#' @return an object of class \code{"spotBOModel"}, 
#' with a \code{predict} method and a \code{print} method.
#'
#' @export
#'
buildBO <- function(x, y, control=list()){
  
  ## Control settings
  con = list()
  con[names(control)] <- control
  control<-con
	control$x <- x
	control$y <- y
	## Extract parameter names (inputs and output)
	control$pNames <- colnames(x)
	control$yName <- "y"
	
	fit <- optim(c(rep(0.1, ncol(x)), 0.1*var(y)), thetaNugget, thetaNuggetGradient, 
	              method="L-BFGS-B", lower=sqrt(.Machine$double.eps) , upper=c(rep(10, ncol(x)), var(y)), X=x, Y=y) 
	
	K <- covar.sep(x, d=fit$par[1:ncol(x)], g=fit$par[ncol(x)+1])
	Ki <- solve(K)
	tau2hat <- drop(t(y) %*% Ki %*% y / nrow(x))

	## Prepare return
  control$fit <- fit  
  control$K <- K
  control$Ki <- Ki
  control$tau2hat <- tau2hat
  class(control) <- "spotBOModel"
  
	## Return
  return(control)
}

#' thetaNugget
#' 
#' get theta (distance, lengthscale)
#' and nugget (noise) parameters gradient
#' 
#' @param par parameter vector. First dim(x) entries are theta values, last entry is nugget parameter.
#' @param X x coordinates
#' @param Y y values at x
#' 
#' @importFrom plgp covar.sep
#' @importFrom laGP distance
#' 
#' @return negLogLikelihood
#' 
#' @export
thetaNugget <- function(par, X, Y)
{
  theta <- par[1:ncol(X)]  
  g <- par[ncol(X)+1]
  n <- length(Y)
  K <- covar.sep(X, d=theta, g=g)
  Ki <- solve(K)
  ldetK <- determinant(K, logarithm=TRUE)$modulus
  ll <- - (n/2)*log(t(Y) %*% Ki %*% Y) - (1/2)*ldetK
  return(-ll)
}


#' thetaNuggetGradient
#' 
#' get theta (distance, lengthscale)
#' and nugget (noise) parameters gradient
#' 
#' @param par parameter vector. First dim(x) entries are theta values, last entry is nugget parameter.
#' @param X x coordinates
#' @param Y y values at x
#' 
#' @importFrom plgp covar.sep
#' @importFrom laGP distance
#' 
#' @export
thetaNuggetGradient <- function(par, X, Y)
{
  theta <- par[1:ncol(X)]
  g <- par[ncol(X)+1]
  n <- length(Y)
  K <- covar.sep(X, d=theta, g=g) 
  Ki <- solve(K)
  KiY <- Ki %*% Y
  
  ## loop over theta components
  dlltheta <- rep(NA, length(theta))
  for(k in 1:length(dlltheta)) {
    dotK <- K * distance(X[,k])/(theta[k]^2)
    dlltheta[k] <- (n/2) * t(KiY) %*% dotK %*% KiY / (t(Y) %*% KiY) - 
      (1/2)*sum(diag(Ki %*% dotK))
  }
  
  ## for g   
  dllg <- (n/2) * t(KiY) %*% KiY / (t(Y) %*% KiY) - (1/2)*sum(diag(Ki))
  
  return(-c(dlltheta, dllg))
}


#' Prediction method for bayesian optimization model
#'
#' Wrapper for \code{predict.spotBOModel}.
#'
#' @param object fit of the model, an object of class \code{"spotBOModel"}, produced by \code{\link{buildBO}}.
#' @param newdata matrix of new data.
#' @param ... not used
#' 
#' @importFrom plgp covar.sep
#' 
#' @export
#' @keywords internal
predict.spotBOModel<-function(object, newdata, ...){
  X <- object$x
  y <- object$y
  Ki <- object$Ki
  XX <- newdata
  theta <- object$fit$par[1:ncol(X)]
  g <- object$fit$par[ncol(X)+1]
  KXX <- covar.sep(XX, d=theta, g=g) 
  KX <- covar.sep(XX, X, d=theta, g=0)
  res <- KX %*% Ki %*% y
  # Sigmap2 <- object$tau2hat*(KXX - KX %*% Ki %*% t(KX))
  list(y=res)
}

#' Print method for BO model
#' 
#' Wrapper for \code{print.spotBOModel}.
#'
#' @param object fit of the model, an object of class \code{"spotBOModel"}, produced by \code{\link{buildBO}}.
#' @param ... not used
#' 
#' @export
#' @keywords internal
print.spotBOModel <- function(x,...){
  str(x$param)
}