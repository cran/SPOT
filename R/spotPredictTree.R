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
#' @return data.frame \code{lhd} \cr
#' - \code{lhd} is a sorted (with respect to fitness, i.e., smallest estimated function value) largeDesign. 
#' Best is first   
#' @references \code{\link{SPOT}}
###################################################################################
spotPredictTree <- function(rawB,mergedB,lhd,spotConfig){	
	writeLines("spotPredictTree started");
	spotInstAndLoadPackages("rpart")
	xNames <- setdiff(names(rawB),"y")
	x <- rawB[,xNames]	
	y <- rawB$y	
	fit <- rpart(y ~ ., data= rawB)	
	res<-predict(fit,lhd)
	lhd <-  lhd[order(res,decreasing=FALSE),];
	lhd <- lhd[1:spotConfig$seq.design.new.size,];	
	print(lhd)
	writeLines("spotPredictTree finished");
	return(lhd)
}
