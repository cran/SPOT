#' Bayesian Optimization Model Interface
#'
#' @param x matrix of input parameters. Rows for each point, columns for each parameter.
#' @param y one column matrix of observations to be modeled.
#' @param control list of control parameters:
#'       \describe{
#'       \item{\code{thetaLower}}{ lower boundary for theta, default is \code{1e-4}}
#'       \item{\code{thetaUpper}}{ upper boundary for theta, default is \code{1e2}}
#'       \item{\code{algTheta}}{  algorithm used to find theta, default is \code{L-BFGS-B}}
#'       \item{\code{budgetAlgTheta}}{ budget for the above mentioned algorithm, default is \code{200}. The value will be multiplied with the length of the model parameter vector to be optimized.}
#'       \item{\code{optimizeP}}{ boolean that specifies whether the exponents (\code{p}) should be optimized. Else they will be set to two. Default is \code{FALSE}}
#'       \item{\code{useLambda}}{ whether or not to use the regularization constant lambda (nugget effect). Default is \code{TRUE}}
#'       \item{\code{lambdaLower}}{ lower boundary for log10{lambda}, default is \code{-6}}
#'       \item{\code{lambdaUpper}}{ upper boundary for log10{lambda}, default is \code{0}}
#'       \item{\code{startTheta}}{ optional start value for theta optimization, default is \code{NULL}}
#'       \item{\code{reinterpolate}}{ whether (\code{TRUE},default) or not (\code{FALSE}) reinterpolation should be performed}
#'       \item{\code{target}}{ target values of the prediction, a vector of strings. Each string specifies a value to be predicted, e.g., "y" for mean, "s" for standard deviation, "ei" for expected improvement. See also \code{\link{predict.kriging}}}
#'       }
#'
#' @importFrom stats optim
#' @importFrom plgp covar.sep
#'
#' @return an object of class \code{"spotBOModel"},
#' with a \code{predict} method and a \code{print} method.
#' Basically a list, with the options and found parameters for the model which has to be passed to the predictor function:
#'       \describe{
#'       \item{\code{x}}{ sample locations }
#'       \item{\code{y}}{ observations at sample locations (see parameters)}
#'       \item{\code{min}}{ min y val}
#'       \item{\code{thetaLower}}{ lower boundary for theta (see parameters)}
#'       \item{\code{thetaUpper}}{ upper boundary for theta (see parameters)}
#'       \item{\code{algTheta}}{ algorithm to find theta (see parameters)}
#'       \item{\code{budgetAlgTheta}}{ budget for the above mentioned algorithm (see parameters)}
#'       \item{\code{lambdaLower}}{ lower boundary for log10{lambda}, default is \code{-6}}
#'       \item{\code{lambdaUpper}}{ upper boundary for log10{lambda}, default is \code{0}}
#'       \item{\code{dmodeltheta}}{ vector of activity parameters}
#'       \item{\code{dmodellambda}}{ regularization constant (nugget)}
#'       \item{\code{mu}}{ mean mu}
#'       \item{\code{ssq}}{ sigma square}
#'       \item{\code{Psi}}{ matrix large Psi}
#'       \item{\code{Psinv}}{ inverse of Psi}
#'       \item{\code{nevals}}{ number of Likelihood evaluations during MLE}
#'       }
#'       
#' @examples 
#' ## Reproduction of Gramacy's classic EI illustration with data from Jones et al.
#' ## Generates Fig. 7.6 from the Gramacy book "Surrogates".
#' x <- c(1, 2, 3, 4, 12)
#' y <- c(0, -1.75, -2, -0.5, 5)
#' ## Build BO Model
#' m1 <- buildBO(x = matrix(x, ncol = 1), 
#'  y = matrix(y, ncol=1),
#'  control = list(target="ei"))
#' xx <- seq(0, 13, length=1000)
#' yy <- predict(object = m1, newdata = xx)
#' m <- which.min(y)
#' fmin <- y[m]
#' mue <- matrix(yy$y, ncol = 1)
#' s2 <- matrix(yy$s, ncol = 1)
#' ei <- matrix(yy$ei, ncol = 1)
#' ## Plotting the Results (similar to Fig. 7.6 in Gramacy's Surrogate book)
#' par(mfrow=c(1,2))
#' plot(x, y, pch=19, xlim=c(0,13), ylim=c(-4,9), main="predictive surface")
#' lines(xx, mue)
#' lines(xx, mue + 2*sqrt(s2), col=2, lty=2)
#' lines(xx, mue - 2*sqrt(s2), col=2, lty=2)
#' abline(h=fmin, col=3, lty=3)
#' legend("topleft", c("mean", "95% PI", "fmin"), lty=1:3,   col=1:3, bty="n")
#' plot(xx, ei, type="l", col="blue", main="EI", xlab="x", ylim=c(0,max(ei)))
#'       
#' @export
#' @references Forrester, Alexander I.J.; Sobester, Andras; Keane, Andy J. (2008). Engineering Design via Surrogate Modelling - A Practical Guide. John Wiley & Sons.
#' @references Gramacy, R. B. Surrogates. CRC press, 2020.
#' @references Jones, D. R., Schonlau, M., and Welch, W. J. Efficient global optimization of expensive black-box functions. Journal of Global Optimization 13, 4 (1998), 455–492.
#' @seealso \code{\link{predict.spotBOModel}}
#' 
#'
buildBO <- function(x, y, control = list()) {
  ## Control settings
  con = list()
  con[names(control)] <- control
  control <- con
  control$x <- x
  control$y <- y
  ## Extract parameter names (inputs and output)
  control$pNames <- colnames(x)
  control$yName <- "y"
  ## Problem dimension
  m <- ncol(x)
  
  fit <- tryCatch(
    optim(
    c(rep(0.1, m), 0.1 * var(y)),
    thetaNugget,
    thetaNuggetGradient,
    method = "L-BFGS-B",
    lower = sqrt(.Machine$double.eps),
    upper = c(rep(10, m), var(y)),
    X = x,
    Y = y
  ), error=function(cond) {
    message(paste("optim reports an error."))
    print(list(m=m, lower=sqrt(.Machine$double.eps), upper=c(rep(10, m), var(y)), x=x, y=y))
    message("Here's the original error message:")
    message(cond)
    # Choose a return value in case of error
    return(NA)
  })
    
  K <- plgp::covar.sep(x,
                       d = fit$par[1:m],
                       g = fit$par[m + 1])
  
  Ki <- solve(K)
  tau2hat <- drop(t(y) %*% Ki %*% y / nrow(x))
  
  ## Prepare return
  ## Necessary?
  control$fit <- fit
  
  control$like = fit$value
  control$d = fit$par[1:m]
  control$g = fit$par[m + 1]
  control$tau2hat = fit$tau2hat
  ## Necessary?
  control$K <- K
  
  control$Ki <- Ki
  control$tau2hat <- tau2hat
  
  ## calculate observed minimum
  ## To be considered for repeats:
  xlist <- split(x, 1:nrow(x))
  uniquex <- unique(xlist)
  ymean <- NULL
  for (xi in uniquex) {
    ind <- xlist %in% list(xi)
    ymean <- c(ymean, mean(y[ind]))
  }
  control$min <- min(ymean)
  
  class(control) <- "spotBOModel"
  
  ## Return
  return(control)
}

#' thetaNugget
#'
#' get theta (distance, lengthscale)
#' and nugget (noise) parameters gradient
#'
#' @param par parameter vector. First dim(x) entries are theta values, 
#' last entry is nugget parameter.
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
  n <- length(Y)
  k <- ncol(X)
  theta <- par[-length(par)]
  if (length(theta) == 1) {
    theta <- rep(theta, k)
  }
  g <- par[k + 1]
  Psi <- plgp::covar.sep(X, d = 1/theta, g = g)
  ## cholesky decomposition
  cholPsi <- try(chol(Psi), TRUE)
  # calculate natural log of the determinant of Psi 
  # (numerically more reliable and also faster than using det or determinant)
  LnDetPsi <- 2 * sum(log(abs(diag(cholPsi))))
  
  #inverse with cholesky decomposed Psi
  Psinv <- try(chol2inv(cholPsi), TRUE)
  psisum <- sum(Psinv) 
  mu <- sum(Psinv %*% Y) / psisum
  yonemu <- Y - mu
  SigmaSqr <- (t(yonemu) %*% Psinv %*% yonemu) / n
  NegLnLike <- n * log(SigmaSqr) + LnDetPsi
  return(NegLnLike)
  # Ki <- tryCatch(solve(K),
  #                error=function(cond) {
  #                  message(paste("Matrix K cannt be inverted:", K))
  #                  message("Here's the original error message:")
  #                  message(cond)
  #                  # Choose a return value in case of error
  #                  return(K)
  #                })
  # ldetK <- determinant(K, logarithm = TRUE)$modulus
  # ll <- -(n / 2) * log(t(Y) %*% Ki %*% Y) - (1 / 2) * ldetK
  # return(-ll)
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
  n <- length(Y)
  k <- ncol(X)
  theta <- par[-length(par)]
  if (length(theta) == 1) {
    theta <- rep(theta, k)
  }
  g <- par[k + 1]
 
  Psi <- plgp::covar.sep(X, d = 1/theta, g = g)
  ## cholesky decomposition
  cholPsi <- try(chol(Psi), TRUE)
  # calculate natural log of the determinant of Psi 
  # (numerically more reliable and also faster than using det or determinant)
  LnDetPsi <- 2 * sum(log(abs(diag(cholPsi))))
  
  #inverse with cholesky decomposed Psi
  Psinv <- try(chol2inv(cholPsi), TRUE)
  psisum <- sum(Psinv) 
  mu <- sum(Psinv %*% Y) / psisum
  yonemu <- Y - mu
  
  # Ki <- tryCatch(solve(K),
  #                error=function(cond) {
  #                  message(paste("Matrix K cannt be inverted:", K))
  #                  message("Here's the original error message:")
  #                  message(cond)
  #                  # Choose a return value in case of error
  #                  return(K)
  #                })
  #  KiY <- Ki %*% Y
  PsinvY <- Psinv %*% yonemu 
  
  ## loop over theta components
  dlltheta <- rep(NA, length(theta))
  for (i in 1:length(dlltheta)) {
    dotK <- Psi * laGP::distance(X[, i]) / (theta[i] ^ 2)
    dlltheta[i] <-
      (n / 2) * t(PsinvY) %*% dotK %*% PsinvY / (t(yonemu) %*% PsinvY) -
      (1 / 2) * sum(diag(Psinv %*% dotK))
  }
  
  ## for g
  dllg <-
    (n / 2) * t(PsinvY) %*% PsinvY / (t(yonemu) %*% PsinvY) - (1 / 2) * sum(diag(Psinv))
  
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
#' @return list with predicted mean \code{y}, uncertainty / standard deviation \code{s} (optional)
#' and expected improvement \code{ei} (optional).
#' Whether \code{s} and \code{ei} are returned is specified by the vector of
#' strings \code{object$target}, which then contains \code{"s"} and \code{"ei"}.
#'
#' @importFrom plgp covar.sep
#'
#' @export
predict.spotBOModel <- function(object, newdata, ...) {
  # printf <- function (...) 
  # {
  #   writeLines(sprintf(...))
  # }
  d <- object$d
  g <- object$g
  tau2hat <- object$tau2hat
  X <- object$x
  y <- object$y
  Ki <- object$Ki
  XX <- as.matrix(newdata)
  KXX <- plgp::covar.sep(XX, d = d, g = g)
  KX <- plgp::covar.sep(XX, X, d = d, g = 0)
  mup <- KX %*% Ki %*% y
  res <- list(y = mup)
  if (any(object$target %in% c("s", "ei", "negLog10ei"))) {
    Sigmap <- tau2hat * (KXX - KX %*% Ki %*% t(KX))
    #print("in predict")
    s <- sqrt(abs(diag(Sigmap)))
    #printf("s: %f", s)
    res$s <- s
    if (any(object$target %in% c("ei", "negLog10ei"))) {
      #printf("objectmin: %f", object$min)
      #printf("mup: %f", mup)
      ydiff <- object$min - mup
      #printf("ydiff: %f", ydiff)
      dn <- ydiff / s
      ei <- (ydiff * pnorm(dn) + s * dnorm(dn))
      res$ei <- ei
      #printf("ei: %f", ei)
      if (any(object$target == "negLog10ei")){
       res$negLog10ei <- -log(ei +(.Machine$double.xmin))
      }
      }
  }
  return(res)
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
print.spotBOModel <- function(x, ...) {
  str(x$param)
}