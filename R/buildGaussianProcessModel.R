#' @title Gaussian Process Model Interface
#'
#' @param x matrix of input parameters. Rows for each point, columns for each parameter. 
#' @param y one column matrix of observations to be modeled.
#' @param control list of control parameters. \code{n} subset size.
#' 
#' @return an object of class \code{"spotGaussianProcessModel"}, 
#' with a \code{predict} method and a \code{print} method.
#' @importFrom laGP darg
#' @importFrom laGP garg
#' @importFrom laGP newGPsep
#' @importFrom laGP jmleGPsep
#' @importFrom laGP updateGPsep
#'
#' @export
#'
#' @examples
#' N <- 200
#' x <- matrix( seq(from=-1, to = 1, length.out = N), ncol = 1)
#' y <- funSphere(x)  + rnorm(N, 0, 0.1)
#' fit <- buildGaussianProcess(x,y)
#' ## Print model parameters
#' print(fit)
#' ## Predict at new location
#' xNew <- matrix( c(-0.1, 0.1), ncol = 1)
#' predict(fit, xNew)
#' ## True value at location
#' t(funSphere(xNew))
buildGaussianProcess <- function(x,
                                 y,
                                 control=list()){
  d <- g <- NULL
  con<-list(samp.size = 100,
            modelControl = list(modelInitialized = FALSE))
  con[names(control)] <- control
  control<-con
	
  ## Case 1: model is not initialized:
  if (control$modelControl$modelInitialized == FALSE){
  control$x <- x
	control$y <- y
	d <- laGP::darg(NULL, x, samp.size = control$samp.size)
	g <- laGP::garg(list(mle = TRUE), y)
	fit <- laGP::newGPsep(x, y, d = d$start, 
	                 g = g$start, dK = TRUE)
	laGP::jmleGPsep(fit, 
	              drange = c(d$min, d$max), 
	              grange = c(g$min, g$max), 
	              dab = d$ab, 
	              gab = g$ab)
	control$fit <- fit  
	control$d <- d
	control$g <- g
	control$pNames <- colnames(x)
	control$yName <- "y"
	class(control) <- "spotGaussianProcessModel"
  } else{
  ## Case 2: model is already initialized:
      n <- nrow(x)
      indices <- (n - control$xNewActualSize +1):n
      xnew <- x[indices, , drop = FALSE]
      ynew <- y[indices, , drop = FALSE]
      laGP::updateGPsep(control$fit, xnew, ynew)
      laGP::jmleGPsep(control$fit, 
                      drange = c(control$d$min, control$d$max), 
                      grange = c(control$g$min, control$g$max), 
                      dab = control$d$ab, 
                      gab = control$g$ab)
      }
  return(control)
}



#' Prediction method for Gaussian Process Model
#'
#'
#' @param object fit of the model, an object of class \code{"spotGaussianProcessModel"}, produced by \code{\link{buildGaussianProcess}}.
#' @param newdata matrix of new data.
#' @param ... not used
#' 
#' @importFrom laGP predGPsep
#' 
#' @export
#' @keywords internal
predict.spotGaussianProcessModel<-function(object, newdata, ...){
	res <- laGP::predGPsep(object$fit, XX = newdata, lite = TRUE)
  list(y = res$mean)
}

#' Print method for Gaussian Process Model
#' 
#'
#' @param object fit of the model, an object of class \code{"spotGaussianProcessModel"}, produced by \code{\link{buildGaussianProcess}}.
#' @param ... not used
#' 
#' @export
#' @keywords internal
print.spotGaussianProcessModel <- function(x,...){
  print(summary(x$fit))
}