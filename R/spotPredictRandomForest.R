###################################################################################
#' SPOT Predictor Random Forest
#'  
#' A prediction model based on rpart, using a random forest
#' 
#' @param rawB unmerged data
#' @param mergedB merged data
#' @param largeDesign new design points which should be predicted
#' @param spotConfig global list of all options, needed to provide data for calling functions
#' 
#' @return returns the list \code{spotConfig} with two new entries:\cr
#' 	spotConfig$seq.modelFit fit of the Krig model used with predict() \cr
#'	spotConfig$seq.largeDesignY the y values of the large design, evaluated with the fit
#'
#' @references  \code{\link{SPOT}}
###################################################################################
spotPredictRandomForest <- function(rawB,mergedB,largeDesign,spotConfig){	
	spotWriteLines(spotConfig$io.verbosity,1,"spotPredictRandomForest started");
	spotInstAndLoadPackages("randomForest")	
	xNames <- setdiff(names(rawB),"y") 
	x <- rawB[xNames]	#MZ: Bugfix for 1 dimensional optimization
	y <- rawB$y	
	fit <- randomForest(x, y)		
	res <- predict(fit,largeDesign)
	#largeDesign <-  as.data.frame(largeDesign[order(res,decreasing=FALSE),]);	#MZ: Bugfix for 1 dimensional optimization
	#res2 <-  as.data.frame(res[order(res,decreasing=FALSE)]);
#	newDesign <- as.data.frame(largeDesign[1:spotConfig$seq.design.new.size,]);	#MZ: Bugfix for 1 dimensional optimization
	#names(largeDesign)=xNames; #MZ: Bugfix for 1 dimensional optimization
	spotWriteLines(spotConfig$io.verbosity,1,"spotPredictRandomForest finished");
	spotConfig$seq.modelFit<-fit;
	spotConfig$seq.largeDesignY<-as.data.frame(res);
	return(spotConfig);
}
