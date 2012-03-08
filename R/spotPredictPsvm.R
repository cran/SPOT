###################################################################################
#' Meta Model Interface: Penalized Support Vector Machine
#' 
#' This meta model for SPOT is based on the penalizedSVM package, using svm()
#' to build a support vector machine model.
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
#' 	spotConfig$seq.modelFit fit of the model used with predict() \cr
#'	spotConfig$seq.largeDesignY the y values of the large design, evaluated with the fit
#' @export
###################################################################################
spotPredictPsvm <- function(rawB,mergedB,largeDesign,spotConfig,externalFit=NULL){	
	spotWriteLines(spotConfig$io.verbosity,1,"spotPredictKsvm started");
	spotInstAndLoadPackages("penalizedSVM")	
	if(is.null(externalFit)){
		xNames <- setdiff(names(rawB),"y")
		x <- rawB[xNames]
		y <- rawB$y
		fit<-svm(y~.,data=data.frame(x,y))
		res <- predict(fit,largeDesign)
	}else{
		fit<-externalFit
		pNames <- row.names(spotConfig$alg.roi);
		if(ncol(largeDesign)<nrow(spotConfig$alg.roi)){ #ugly, but necessary because R is strange about converting one dimensional vectors to dataframes (confusing rows/columns)
			largeDesign<-t(data.frame(largeDesign))			
		}
		else{
			largeDesign<-data.frame(largeDesign)
		}
		colnames(largeDesign)<-pNames
		res <- predict(fit,largeDesign)	
	}	
	spotWriteLines(spotConfig$io.verbosity,1,"spotPredictKsvm finished");
	spotConfig$seq.modelFit<-fit;
	spotConfig$seq.largeDesignY<-as.data.frame(res);
	return(spotConfig);
}
