###################################################################################
#' SPOT Predictor Random Forest combined with Mlegp
#'  
#' A prediction model based on rpart, using a random forest and mlegp
#' 
#' @param rawB unmerged data
#' @param mergedB merged data
#' @param largeDesign new design points which should be predicted
#' @param spotConfig global list of all options, needed to provide data for calling functions
#' 
#' @return data.frame \code{largeDesign} \cr
#' - \code{largeDesign} is a sorted (with respect to fitness, i.e., smallest estimated function value) largeDesign. 
#' Best is first   
#'
#' @references  \code{\link{SPOT}}
###################################################################################
spotPredictRandomForestMlegp <- function(rawB,mergedB,largeDesign,spotConfig){	
	writeLines("spotPredictRandomForestMlegp started");
	spotInstAndLoadPackages("randomForest")	
	spotInstAndLoadPackages("mlegp")	
	xNames <- setdiff(names(rawB),"y")
	x <- rawB[,xNames]	
	y <- rawB$y	
	rf.fit <- randomForest(x, y)		
	rf.res <- predict(rf.fit,largeDesign)
	rf.largeDesign <- largeDesign[order(rf.res,decreasing=FALSE),]
	rf.s <- round(spotConfig$seq.design.new.size/2)
	mlegp.s <- spotConfig$seq.design.new.size - rf.s
	rf.largeDesign <- rf.largeDesign[1:rf.s,]
	if (mlegp.s> 0){
		mlegp.fit <- mlegp(x, y)		
		mlegp.res <- predict(mlegp.fit,largeDesign)
		mlegp.largeDesign <- largeDesign[order(rf.res,decreasing=FALSE),]
		mlegp.largeDesign <- largeDesign[1:mlegp.s,]
		return(rbind(rf.largeDesign,mlegp.largeDesign))
	}
	else{ return(rf.largeDesign)}
	writeLines("spotPredictRandomForestMlegp finished");
	return(largeDesign)
}
