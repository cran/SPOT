###################################################################################
#' Meta Model Interface: Multivariate Adaptive Regression Spline
#' 
#' Prediction based on earth package, using Multivariate Adaptive Regression Spline models
#' Can be used both for single and multi objective SPOT.
#'
#' @param rawB unmerged data
#' @param mergedB merged data
#' @param largeDesign new design points which should be predicted
#' @param spotConfig global list of all options, needed to provide data for calling functions
#' @param externalFit if an existing model fit is supplied, the model will not be build based on 
#'				data, but only evaluated with the model fit (on the largeDesign data). To build the model, 
#'				this parameter has to be NULL. If it is not NULL the paramters mergedB and rawB will not be 
#'				used at all in the function.
#'
#' @return returns the list \code{spotConfig} with two new entries:\cr
#' 	spotConfig$seq.modelFit fit of the earth model used with predict() \cr
#'	spotConfig$seq.largeDesignY the y values of the large design, evaluated with the fit
#' @export
###################################################################################
spotPredictEarth <- function(rawB,mergedB,largeDesign,spotConfig,externalFit=NULL){	
	spotWriteLines(spotConfig$io.verbosity,1,"spotPredictEarth started");
	spotInstAndLoadPackages("earth")	
	if(is.null(externalFit)){
		xNames <- row.names(spotConfig$alg.roi)
		yNames <- setdiff(names(rawB),xNames)
		which(names(rawB)==yNames)
		x <- rawB[xNames]		
		y <- rawB[yNames]
		fit <- earth(x=x,y=y)
		res <- predict(fit,largeDesign)
	}else{
		fit <-externalFit
		res <- predict(fit,largeDesign[,])	
	}	
	#browser()
	spotWriteLines(spotConfig$io.verbosity,1,"spotPredictEarth finished");
	spotConfig$seq.modelFit<-fit;
	spotConfig$seq.largeDesignY<-as.data.frame(res);
	return(spotConfig);
}
