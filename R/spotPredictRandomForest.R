###################################################################################
#' Meta Model Interface: Random Forest
#'  
#' A prediction model interface based on randomForest package, using a random forest
#' for regression.
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
#' 	spotConfig$seq.modelFit fit of the Krig model used with predict() \cr
#'	spotConfig$seq.largeDesignY the y values of the large design, evaluated with the fit
#'
#' @seealso \code{\link{SPOT}}
#' @export
###################################################################################
spotPredictRandomForest <- function(rawB,mergedB,largeDesign,spotConfig,externalFit=NULL){	
	spotWriteLines(spotConfig$io.verbosity,1,"spotPredictRandomForest started");
	spotInstAndLoadPackages("randomForest")	
	if(is.null(externalFit)){#IF NO EXTERNAL FIT: BUILD MODEL AND EVALUATE
		xNames <- row.names(spotConfig$alg.roi); 
		yNames <- setdiff(names(rawB),xNames)
		x <- rawB[xNames]		
		if(length(yNames)==1){
			y <- rawB[[yNames]]
			fit <- randomForest(x, y)		
			res <- predict(fit,largeDesign)
		}
		else{#Distinction for multi criteria spot 
			y <- rawB[yNames]
			fit=list()
			res=list()
			for (i in 1:length(yNames)){
				fit[[i]]<-randomForest(x,y[,i])
				res[[i]]<-predict(fit[[i]],largeDesign)
			}			
		}
	}else{# ELSE DO NOT BUILD MODEL; EVALUATE ONLY		
		fit <-externalFit
		if(length(spotConfig$alg.resultColumn)>1){
			res=list()
			for (i in 1:length(fit)){
				res[[i]]<-predict(fit[[i]],largeDesign[,])
			}	
		}
		else{res <- predict(fit,largeDesign[,])}	
	}	
	spotWriteLines(spotConfig$io.verbosity,1,"spotPredictRandomForest finished");
	spotConfig$seq.modelFit<-fit;
	spotConfig$seq.largeDesignY<-as.data.frame(res);
	return(spotConfig);
}

