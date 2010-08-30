###################################################################################
#' Spot Predictor Kriging  
#'
#' Kriging model based on mlegp  
#' 
#' @param rawB matrix of raw x and y values
#' @param mergedB matrix of merged x and y values, does not have replicate entries
#' @param largeDesign design points to be evaluated by the meta model  
#' @param spotConfig the list of all parameters is given 
#'
#' @return data.frame \code{lhd} \cr
#' - \code{lhd} is a sorted (with respect to fitness, i.e., smallest estimated function value) largeDesign   
#' Best is first 
#'
#' @references  \code{\link{SPOT}}
####################################################################################
spotPredictMlegp <- function(rawB,mergedB,largeDesign,spotConfig){
	spotWriteLines(spotConfig$io.verbosity,2,"spotPredictMlegp started");	
	spotInstAndLoadPackages("mlegp")	
	xNames <- setdiff(names(rawB),"y")
	x <- rawB[,xNames]
	y <- rawB$y	
	fit <- mlegp(x,y, verbose=spotConfig$io.verbosity  )
	# as.matrix makes the prediction faster:
	res<-predict(fit,as.matrix(largeDesign))
	largeDesign <-  largeDesign[order(res,decreasing=FALSE),];
	largeDesign <- largeDesign[1:spotConfig$seq.design.new.size,];
	spotWriteLines(spotConfig$io.verbosity,2,"spotPredictMlegp finished successfully");	
	return(largeDesign)	
}

