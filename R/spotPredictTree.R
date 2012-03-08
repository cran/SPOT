###################################################################################
#' Meta Model Interface: Tree
#'  
#' A prediction model based on rpart, using a single tree model .
#' 
#' @param rawB unmerged data
#' @param mergedB merged data
#' @param lhd new design points which should be predicted
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
spotPredictTree <- function(rawB,mergedB,lhd,spotConfig,externalFit=NULL){
	spotWriteLines(spotConfig$io.verbosity,1,"spotPredictTree started");
	spotInstAndLoadPackages("rpart")
	if(is.null(externalFit)){
		xNames <- setdiff(names(rawB),"y")
		x <- rawB[xNames] 
		y <- rawB$y 
		fit <- rpart(y ~ ., data= rawB)
		res <- predict(fit,lhd)
	}else{
		fit<-externalFit
		pNames <- row.names(spotConfig$alg.roi);
		if(ncol(lhd)<nrow(spotConfig$alg.roi)){ #ugly, but necessary because R is strange about converting one dimensional vectors to dataframes (confusing rows/columns)
			lhd<-t(data.frame(lhd))			
		}
		else{
			lhd<-data.frame(lhd)
		}
		colnames(lhd)<-pNames
		res <- predict(fit,data.frame(lhd))	
	}	
	spotWriteLines(spotConfig$io.verbosity,1,"spotPredictTree finished");
	spotConfig$seq.modelFit<-fit;
	spotConfig$seq.largeDesignY<-as.data.frame(res);
	return(spotConfig);
}