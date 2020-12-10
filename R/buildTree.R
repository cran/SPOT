 
#' Tree Regression Interface
#'
#' This is a simple wrapper for the rpart function from the rpart package.
#' The purpose of this function is to provide an interface as required by SPOT, to enable
#' modeling and model-based optimization with regression trees.
#'
#' @param x matrix of input parameters. Rows for each point, columns for each parameter.
#' @param y one column matrix of observations to be modeled.
#' @param control list of control parameters, currently not used.
#' 
#' @importFrom rpart rpart
#' @importFrom stats predict
#' @importFrom stats as.formula
#' 
#' @return an object of class \code{"spotTreeModel"}, with a \code{predict} method and a \code{print} method.
#'
#' @export
#'
#' @examples
#' ## Create design points
#' set.seed(1)
#' x <- cbind(runif(20)*15-5, runif(20)*15)
#' ## Compute observations at design points (for Branin function)
#' y <- funBranin(x)
#' ## Create model
#' fit <- buildTreeModel(x,y)
#' ## Print model parameters
#' print(fit)
#' ## Predict at new location
#' predict(fit,cbind(1,2))
#' ## True value at location
#' funBranin(matrix( c(1,2), 1, ))
#' ## 
#' set.seed(123)
#' x <- seq(-1,1,1e-2)
#' y0 <- c(-10,10)
#' sfun0  <- stepfun(0, y0, f = 0)
#' y <- sfun0(x)
#' fit <- buildTreeModel(x,y)
#' # plot(fit)
#' # plot(x,y, type = "l")
#' yhat <- predict(fit, newdata = 1)
#' yhat$y == 10
# 
#'
 
buildTreeModel <- function(x,y,control=list()){
  ## Control settings
  con<-list()
  con[names(control)] <- control
  control<-con
  
  control$x <- x
  control$y <- y
  
  ## Convert inputs to combined data frame
  x <- as.data.frame(x)
  
  if(!is.null(control$xnames)) names(x) <- control$xnames	else 	names(x) <- paste0('x', 1:(ncol(x)))
  pNames <- colnames(x)
  y <- as.data.frame(y)
  names(y) <- "y"
  df1 <- data.frame(y,x)
  
  nVars <- ncol(x)
  fmla <- as.formula(paste("y ~ ", paste(pNames, collapse= "+")))
  fit = rpart(fmla, data = df1, model = TRUE)
  
  ## Prepare return
  control$fit <- fit  
  control$pNames <- pNames
  control$yName <- names(y)	
  class(control)<-"spotTreeModel"
  
  ## Return
  return(control)
}
#update.spotRandomForest <- function(x,y,fit){
#  
#}

 
#' Prediction method for rpart tree models
#'
#' Wrapper for \code{predict.rpart}.
#'
#' @param object fit of the model, an object of class \code{"spotTreeModel"}, produced by \code{\link{buildTreeModel}}.
#' @param newdata matrix of new data.
#' @param ... not used
#'
#' @importFrom rpart rpart
#'  
#' @export
#' @keywords internal
 
predict.spotTreeModel <- function(object,newdata,...){
  #if(!all(colnames(newdata) %in% object$pNames))
  #  colnames(newdata) <- object$pNames
  newdata <- as.data.frame(newdata)
  colnames(newdata) <- object$pNames
  res <- predict(object$fit,
                 newdata,
                 type = "matrix",
                 ...)  
  list(y=res)
}

 
#' Print method for rpart tree models 
#' 
#' Wrapper for \code{print.rpart}.
#'
#' @param object fit of the model, an object of class \code{"spotRandomForest"}, produced by \code{\link{buildRandomForest}}.
#' @param ... not used
#' 
#' @importFrom rpart rpart
#' 
#' @export
#' @keywords internal
 
print.spotTreeModel <- function(x,...){
  print(x$fit)
}


#' Plot rpart tree model
#' 
#' Plot model produced by \code{\link{buildTreeModel}}.
#'
#' @param x tree model (settings and parameters) of class \code{spotTreeModel}.
#' @param ... parameters passed to rpart.plot plotting function 
#' @importFrom rpart.plot rpart.plot
#' @export
#' @keywords internal
#' 
plot.spotTreeModel <- function(x,...){
  rpart.plot(x$fit)
}

