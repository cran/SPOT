###################################################################################
#' SPOT Predictor Tree
#'  
#' A prediction model based on rpart, using a single tree model 
#' 
#' @param rawB unmerged data
#' @param mergedB merged data
#' @param lhd new design points which should be predicted
#' @param spotConfig global list of all options, needed to provide data for calling functions
#' 
#' @return returns the list \code{spotConfig} with two new entries:\cr
#' 	spotConfig$seq.modelFit fit of the Krig model used with predict() \cr
#'	spotConfig$seq.largeDesignY the y values of the large design, evaluated with the fit
#'  
#' @references \code{\link{SPOT}}
###################################################################################
spotPredictTree <- function(rawB,mergedB,lhd,spotConfig){	
	spotWriteLines(spotConfig$io.verbosity,1,"spotPredictTree started");
	spotInstAndLoadPackages("rpart")
	xNames <- setdiff(names(rawB),"y")
	x <- rawB[xNames] #MZ: Bugfix for 1 dimensional optimization	
	y <- rawB$y	
	fit <- rpart(y ~ ., data= rawB)	
	res<-predict(fit,lhd)
	#lhd <-   as.data.frame(lhd[order(res,decreasing=FALSE),]); #MZ: Bugfix for 1 dimensional optimization
	#lhd <-  as.data.frame(lhd[1:spotConfig$seq.design.new.size,]); #MZ: Bugfix for 1 dimensional optimization
	#names(lhd)=xNames; #MZ: Bugfix for 1 dimensional optimization
	#spotPrint(spotConfig$io.verbosity,1,lhd)
	spotWriteLines(spotConfig$io.verbosity,1,"spotPredictTree finished");
	
	#spotConfig$newDesign<-lhd;
	#spotConfig$newDesignPredictedY<-predict(fit,lhd);
	spotConfig$seq.modelFit<-fit;
	spotConfig$seq.largeDesignY<-as.data.frame(res);
	return(spotConfig);
}
