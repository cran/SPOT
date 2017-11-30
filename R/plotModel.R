
###################################################################################################
#' Surface plot of a model
#'
#' A (filled) contour or perspective plot of a fitted model.
#'
#' @param object fit created by a modeling function, e.g., \code{\link{buildRandomForest}}.
#' @param which a vector with two elements, each an integer giving the two independent variables of the plot 
#' (the integers are indices of the respective data set). 
#' @param constant a numeric vector that states for each variable a constant value that it will take on
#' if it is not varied in the plot. This affects the parameters not selected by the \code{which} parameter.
#' By default, this will be fixed to the best known solution, i.e., the one with minimal y-value, according
#' to \code{which.min(object$y)}. The length of this numeric vector should be the same as the number of columns in \code{object$x}
#' @param xlab a vector of characters, giving the labels for each of the two independent variables.
#' @param ylab character, the value of the dependent variable predicted by the corresponding model.
#' @param type string describing the type of the plot:  \code{"filled.contour"} (default), \code{"contour"} or \code{"persp"} (perspective) plot.
#' @param ... additional parameters passed to the \code{contour} or \code{filled.contour} function.
#'
#' @examples
#' ## generate random test data
#' testfun <- function (x) sum(x^2)
#' set.seed(1)
#' k <- 30
#' x <- cbind(runif(k)*15-5,runif(k)*15,runif(k)*2-7,runif(k)*5+22)
#' y <- as.matrix(apply(x,1,testfun))
#' fit <- buildLM(x,y)
#' plotModel(fit)
#' plotModel(fit,type="contour")
#' plotModel(fit,type="persp")
#' plotModel(fit,which=c(1,4))
#' plotModel(fit,which=2:3)
#'
#' @seealso \code{\link{plotFunction}}, \code{\link{plotData}}
#'
#' @export
###################################################################################################
plotModel <- function(object,which=1:2,
						constant=object$x[which.min(object$y),], #best known solution. default.
						xlab= paste("x",which,sep=""),ylab="y",type="filled.contour",...){
  xlab <- xlab[order(which)]
  which <- sort(which)
	#number of variables
  nvar <- ncol(object$x)
	#bounds
  lower <- apply(object$x[,which],2,min)
  upper <- apply(object$x[,which],2,max)	
	#varied variables
	vary <-  (1:nvar) %in% which
	force(object)
	force(nvar)
	force(vary)
	force(constant)
  if(nvar == 2){
		plotfun <- evaluateModel(object)
  }else if(nvar < 2 | length(which) != 2){
    stop("This plot of a model can only be generated for exactly 2 variables.")
  }else if(nvar > 2){
		plotfun2 <- evaluateModel(object)
		plotfun2
	  plotfun <- function(xx){ #fix constants
		  z2 <- matrix(constant,nrow(xx),nvar,byrow=TRUE)
		  z2[,which(vary)[1]] <- xx[,1]
			z2[,which(vary)[2]] <- xx[,2]
			plotfun2(z2)
		}	
  }	
  plotFunction(f=plotfun,lower=lower,upper=upper,
                     type=type,
                     xlab=xlab[1],ylab=xlab[2],zlab=ylab,points1=object$x[,which],...)	
	
}