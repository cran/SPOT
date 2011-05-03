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
#' @return returns the list \code{spotConfig} with two new entries:\cr
#' 	spotConfig$seq.modelFit fit of the Krig model used with predict() \cr
#'	spotConfig$seq.largeDesignY the y values of the large design, evaluated with the fit
#'
#' @references  \code{\link{SPOT}}
###################################################################################
spotPredictRandomForestMlegp <- function(rawB,mergedB,largeDesign,spotConfig){	
	spotWriteLines(spotConfig$io.verbosity,1,"spotPredictRandomForestMlegp started");
	spotInstAndLoadPackages("randomForest")	
	spotInstAndLoadPackages("mlegp")	
	xNames <- setdiff(names(rawB),"y") 
	x <- rawB[xNames]	#MZ: Bugfix for 1 dimensional optimization
	y <- rawB$y	
	rf.fit <- randomForest(x, y)		
	rf.res <- predict(rf.fit,largeDesign)
	#rf.largeDesign <- as.data.frame(largeDesign[order(rf.res,decreasing=FALSE),]);	#MZ: Bugfix for 1 dimensional optimization
	#rf.s <- round(spotConfig$seq.design.new.size/2)
	#mlegp.s <- spotConfig$seq.design.new.size - rf.s
	#rf.largeDesign <-  as.data.frame(rf.largeDesign[1:rf.s,]);	#MZ: Bugfix for 1 dimensional optimization
	#names(rf.largeDesign)=xNames; #MZ: Bugfix for 1 dimensional optimization
#	if (mlegp.s> 0){
        ##mlegp.fit <- mlegp(X=x, Z=y, verbose=0, min.nugget=0.1, constantMean = spotConfig$seq.mlegp.constantMean)
        constantMean <- spotConfig$seq.mlegp.constantMean
        if (constantMean != 1) {
          ones = rep(1, dim(x)[1])
          dx = cbind(ones, x)
          t = try(solve(t(dx) %*% dx), TRUE)
          if (class(t) == "try-error") {
            constantMean <-1
            }
        }
        mlegp.fit <- mlegp(X=x
                     , Z=y
                     , verbose = 0
                     , constantMean = constantMean
                     , min.nugget = spotConfig$seq.mlegp.min.nugget
                     )
        
        mlegp.res <- predict(mlegp.fit,largeDesign)
        #mlegp.largeDesign <-  as.data.frame(largeDesign[order(rf.res,decreasing=FALSE),]);	#MZ: Bugfix for 1 dimensional optimization
		#mlegp.largeDesign <-  as.data.frame(largeDesign[1:mlegp.s,]);	#MZ: Bugfix for 1 dimensional optimization
		#names(mlegp.largeDesign)=xNames; #MZ: Bugfix for 1 dimensional optimization
		newDesignPrediction<-as.data.frame((mlegp.res+rf.res)/2)
#	}
#	else{ 
#		newDesign<-rf.largeDesign
#		newDesignPrediction<-as.data.frame(rf.res)
#	}
	spotWriteLines(spotConfig$io.verbosity,1,"spotPredictRandomForestMlegp finished");
	spotConfig$seq.largeDesignY<-newDesignPrediction;
	return(spotConfig);
}
