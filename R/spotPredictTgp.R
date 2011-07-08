###################################################################################
#' SPOT Predictor Tgp
#'
#' This function implements a  model for prediction, based on Mat's tgp 
#' 
#' @param rawB unmerged data
#' @param mergedB merged data
#' @param lhd new design points which should be predicted
#' @param spotConfig global list of all options, needed to provide data for calling functions
#' 
#' @return returns the list \code{spotConfig} with a new entry:\cr
#'  spotConfig$seq.modelFit fit of the Krig model used with predict() \cr
#'	spotConfig$seq.largeDesignY the y values of the large design, evaluated with the fit
#'
#' @references \code{\link{SPOT}}
###################################################################################
spotPredictTgp <- function(rawB,mergedB,lhd,spotConfig){
	spotWriteLines(spotConfig$io.verbosity,1,"spotPredictTgp started");
	spotInstAndLoadPackages("tgp")
	
	xNames <- setdiff(names(rawB),"y")
	x <- rawB[xNames]	#MZ: Bugfix for 1 dimensional optimization	
	y <- rawB$y
	fit <- btgpllm(X=x,Z=y,XX=lhd,verb=spotConfig$io.verbosity,improv=c(1,spotConfig$seq.design.size));	
	#res=predict(fit,lhd)$ZZ.mean;
	res=fit$improv$rank;
	spotConfig$seq.modelFit=fit; #can be used for adding new points later on, or in ensemble
	spotConfig$seq.largeDesignY<-as.data.frame(res);
	return(spotConfig);
}
