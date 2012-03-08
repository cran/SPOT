###################################################################################
#' Meta Model Interface: Treed Gaussian Processes
#'
#' This function implements a  model for prediction, based on Mat's tgp 
#' 
#' @param rawB unmerged data
#' @param mergedB merged data
#' @param lhd new design points which should be predicted
#' @param spotConfig global list of all options, needed to provide data for calling functions
#' @param externalFit if an existing model fit is supplied, the model will not be build based on 
#'				data, but only evaluated with the model fit (on the lhd data). To build the model, 
#'				this parameter has to be NULL. If it is not NULL the paramters mergedB and rawB will not be 
#'				used at all in the function.
#'
#' @return returns the list \code{spotConfig} with a new entry:\cr
#'  spotConfig$seq.modelFit fit of the Krig model used with predict() \cr
#'	spotConfig$seq.largeDesignY the y values of the large design, evaluated with the fit
#'
#' @seealso \code{\link{SPOT}}
#' @export
###################################################################################
spotPredictTgp <- function(rawB,mergedB,lhd,spotConfig,externalFit=NULL){
	spotWriteLines(spotConfig$io.verbosity,1,"spotPredictTgp started");
	spotInstAndLoadPackages("tgp")	
	if(is.null(externalFit)){
		xNames <- setdiff(names(rawB),"y")
		x <- rawB[xNames]
		y <- rawB$y
		fit <- btgpllm(X=x,Z=y,XX=lhd,verb=spotConfig$io.verbosity,improv=c(1,spotConfig$seq.design.size));	
		res=predict(fit,lhd)$ZZ.mean;
		#res=fit$improv$rank;# TODO this must be tested, else use predict as above 	
	}else{
		fit <-externalFit
		if(ncol(lhd)<nrow(spotConfig$alg.roi)){ #ugly, but necessary because R is strange about converting one dimensional vectors to dataframes (confusing rows/columns)
			lhd<-t(lhd)			
		}
		else{
			lhd<-lhd
		}
		res=predict(fit,lhd)$ZZ.mean;
	}	
	spotConfig$seq.modelFit=fit;
	spotConfig$seq.largeDesignY<-as.data.frame(res);
	return(spotConfig);
}