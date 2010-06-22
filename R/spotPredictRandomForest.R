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
#' @return data.frame \code{lhd} \cr
#' - \code{lhd} is a sorted (with respect to fitness, i.e., smallest estimated function value) largeDesign. 
#' Best is first   
#'
#' @references  \code{\link{SPOT}}
###################################################################################
spotPredictRandomForest <- function(rawB,mergedB,largeDesign,spotConfig){	
	writeLines("spotPredictRandomForest started");
	spotInstAndLoadPackages("randomForest")	
	xNames <- setdiff(names(rawB),"y")
	x <- rawB[,xNames]	
	y <- rawB$y	
	fit <- randomForest(x, y)		
	res <- predict(fit,largeDesign)
	largeDesign <-  largeDesign[order(res,decreasing=FALSE),];	
	newDesign <- largeDesign[1:spotConfig$seq.design.new.size,];	
	writeLines("spotPredictRandomForest finished");
	return(newDesign)
}
