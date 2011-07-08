###################################################################################
#' spotPredictKrig
#' 
#' Kriging based on fields
#'
#' @param rawB unmerged data
#' @param mergedB merged data
#' @param largeDesign new design points which should be predicted
#' @param spotConfig global list of all options, needed to provide data for calling functions
#' 
#'
#' @return returns the list \code{spotConfig} with two new entries:\cr
#' 	spotConfig$seq.modelFit fit of the Krig model used with predict() \cr
#'	spotConfig$seq.largeDesignY the y values of the large design, evaluated with the fit
###################################################################################
spotPredictKrig <- function(rawB,mergedB,largeDesign,spotConfig){	
	spotWriteLines(spotConfig$io.verbosity,1,"spotPredictKrig started");
	spotInstAndLoadPackages("fields")	
	xNames <- setdiff(names(rawB),"y")
	x <- rawB[xNames]
	y <- rawB$y
	fit <- Krig(x=x,Y=y);
	res <- predict(fit,largeDesign)
	#largeDesign <-  as.data.frame(largeDesign[order(res,decreasing=FALSE),]);	
	#newDesign <- as.data.frame(largeDesign[1:spotConfig$seq.design.new.size,]);
	#names(newDesign)=xNames; 
	#return(newDesign)
	spotWriteLines(spotConfig$io.verbosity,1,"spotPredictKrig finished");
	spotConfig$seq.modelFit<-fit;
	spotConfig$seq.largeDesignY<-as.data.frame(res);
	return(spotConfig);
}
