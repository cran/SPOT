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
#' @return returns the list \code{spotConfig} with two new entries:\cr
#' 	spotConfig$seq.modelFit fit of the Krig model used with predict() \cr
#'	spotConfig$seq.largeDesignY the y values of the large design, evaluated with the fit
#'
#' @references  \code{\link{SPOT}}
####################################################################################
spotPredictMlegp <- function(rawB,mergedB,largeDesign,spotConfig){
	spotWriteLines(spotConfig$io.verbosity,2,"spotPredictMlegp started");	
	spotInstAndLoadPackages("mlegp")	
	xNames <- setdiff(names(rawB),"y")#MZ: Bugfix for 1 dimensional optimization
        x <- unname(as.matrix(rawB[xNames]))
        y <- unname(as.matrix(rawB$y))
        
        constantMean <- spotConfig$seq.mlegp.constantMean
        if (constantMean != 1) {
          ones = rep(1, dim(x)[1])
          dx = cbind(ones, x)
          t = try(solve(t(dx) %*% dx), TRUE)
          if (class(t) == "try-error") {
            constantMean <-1
            }
        }
		#if(spotConfig$io.verbosity==0){
		#	if(.Platform$OS.type=="windows"){
		#		sink("NUL");
		#	}else{
		#		sink("/dev/null");
		#	}
		#}
        fit <- mlegp(X=x
                     , Z=y
                     , verbose = spotConfig$io.verbosity
                     , constantMean = constantMean
                     , min.nugget = spotConfig$seq.mlegp.min.nugget
                     )
		#if(spotConfig$io.verbosity==0){sink();}		
        ## as.matrix makes the prediction faster:
        ###print(largeDesign)
	res<-predict(fit,as.matrix(largeDesign))
	###print("**** RES:") # TODO debug
	###print(res) # TODO debug
	largeDesign <-  as.data.frame(largeDesign[order(res,decreasing=FALSE),]);#MZ: Bugfix for 1 dimensional optimization
	largeDesign <- as.data.frame(largeDesign[1:spotConfig$seq.design.new.size,]); #MZ: Bugfix for 1 dimensional optimization
	names(largeDesign)=xNames; #MZ: Bugfix for 1 dimensional optimization
	spotWriteLines(spotConfig$io.verbosity,2,"spotPredictMlegp finished successfully");	
	#spotConfig$newDesign<-largeDesign;
	#spotConfig$newDesignPredictedY<-predict(fit,largeDesign);
	spotConfig$seq.modelFit<-fit;
	spotConfig$seq.largeDesignY<-as.data.frame(res);
	return(spotConfig);
}

