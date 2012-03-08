###################################################################################
#' Meta Model Interface: Fields Kriging
#' 
#' Kriging meta model based on fields package.
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
#' @export
###################################################################################
spotPredictKrig <- function(rawB,mergedB,largeDesign,spotConfig,externalFit=NULL){	
	spotWriteLines(spotConfig$io.verbosity,1,"spotPredictKrig started");
	spotInstAndLoadPackages("fields")	

	if(is.null(externalFit)){
		xNames <- setdiff(names(rawB),"y")
		x <- rawB[xNames]
		y <- rawB$y
		fit <- Krig(x=x,Y=y);
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
	res[which(is.na(res)==TRUE)] = median(fit$y,na.rm = T) #replaces NA values with median of y..?
	spotWriteLines(spotConfig$io.verbosity,1,"spotPredictKrig finished");
	spotConfig$seq.modelFit<-fit;
	spotConfig$seq.largeDesignY<-as.data.frame(res);
	return(spotConfig);
}
