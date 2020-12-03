#' @title Lasso Model Interface
#'
#' @description The purpose of this function is to provide an interface as 
#' required by \code{\link{spot}}, 
#' to enable modeling and model-based optimization with Lasso models. 
#'
#' @param x matrix of input parameters. Rows for each point, columns for each parameter. 
#' @param y one column matrix of observations to be modeled.
#' @param control list of control parameters, currently only with 
#' parameter \code{formula}. 
#' The \code{useStep} boolean specifies whether the \code{step} function is used.
#' The \code{formula} is passed to the lm function.
#' Without a formula, a second order model will be built.
#'  
#' @importFrom stats as.formula
#' @importFrom glmnet glmnet
#' @importFrom glmnet cv.glmnet
#' @importFrom stats step
#' @importFrom stats model.matrix
#' 
#' @return an object of class \code{"spotLassoModel"}, 
#' with a \code{predict} method and a \code{print} method.
#'
#' @export
#'
#' @examples
#' ## Test-function:
#' braninFunction <- function (x) {	
#' 	(x[2]  - 5.1/(4 * pi^2) * (x[1] ^2) + 5/pi * x[1]  - 6)^2 + 
#'	10 * (1 - 1/(8 * pi)) * cos(x[1] ) + 10
#' }
#' ## Create design points
#' set.seed(1)
#' x <- cbind(runif(20)*15-5,runif(20)*15)
#' ## Compute observations at design points (for Branin function)
#' y <- as.matrix(apply(x,1,braninFunction))
#' ## Create model
#' fit <- buildLasso(x,y,control = list(algTheta=optimLHD))
#' ## Print model parameters
#' print(fit)
#' ## Predict at new location
#' predict(fit,cbind(1,2))
#' ## True value at location
#' braninFunction(c(1,2))
buildLasso <-function(x,y,control=list()){
  
  ## Control settings
  con<-list(useStep=FALSE)
  con[names(control)] <- control
  control<-con
	
	control$x <- x
	control$y <- y
	
	## Convert inputs to combined data frame
	x <- as.data.frame(x)
	y <- as.data.frame(y)
	colnames(y) <- "y"
	df <- cbind(y,x)
	
	## Extract parameter names (inputs and output)
  pNames<-colnames(x)
  yName<-"y"
	 
  ## build formula
  if(is.null(control$formula)){
    Mainterms<-"." #Main effects 
    Fullformula <- as.formula(paste0(yName, "~",Mainterms) ) #combine to full formula
  }else{
    Fullformula <- control$formula
  }
  x_vars  <- model.matrix(y~.-1, data=df)
  y_var <- df$y  
  lambda_seq <- 10^seq(2, -2, by = -.01)
  #
  # train = sample(1:nrow(x_vars), nrow(x_vars))
  #x_test = (-train)
  #y_test = y_var[x_test]
  cv_output <- cv.glmnet(x_vars, y_var,
                         alpha = 1, lambda = lambda_seq, 
                         nfolds = 50)
  best_lam <- cv_output$lambda.min
  
  fit <- glmnet(x_vars, y_var, alpha = 1, lambda = best_lam)
  # fit$best_lam <- best_lam
  #pred <- predict(lasso_best, s = best_lam, newx = x_vars[x_test,])
  
  ## Fit model:	
  # fit <- lm(Fullformula, data=df)
  # alpha=1 means lasso regression. 
  # fit <- glmnet(scale(x_vars), y_var, alpha=1)
  # for testing:
  # plot(fit, xvar = "lambda", label=T)
  # Cross validation to find the optimal lambda penalization
  # cv.lasso <- cv.glmnet(varmtx, response, alpha=1)

  ## Prepare return
  control$best_lam <- best_lam
  control$fit <- fit  
  control$pNames <- pNames
  control$yName <- yName	
  class(control)<-"spotLassoModel"
  
	## Return
  return(control)
}

#' @title Prediction method for lasso model
#'
#' @description Wrapper for \code{predict.glmnet}.
#'
#' @param object fit of the model, an object of class \code{"spotLassoModel"}, 
#' produced by \code{\link{buildLasso}}.
#' @param newdata matrix of new data.
#' @param ... not used
#' 
#' @importFrom stats predict
#' 
#' @export
#' @keywords internal
predict.spotLassoModel<-function(object, newdata, ...){
# 	newdata <- as.data.frame(newdata)
#   if(!all(colnames(newdata) %in% object$pNames))
#     colnames(newdata) <- object$pNames
  res<-predict(object = object$fit,
               s = object$best_lam,
               newx= newdata)
  list(y=res)
}

#' @title Print method for lasso model
#' 
#' @description  Wrapper for \code{print.lasso}.
#'
#' @param object fit of the model, an object of class \code{"spotLassoModel"}, 
#' produced by \code{\link{buildLasso}}.
#' @param ... not used
#' 
#' @importFrom stats coef
#' @export
#' @keywords internal
print.spotLassoModel <- function(x,...){
  print(coef(x$fit))
  print(paste0("Best lambda: ",x$best_lam))
}