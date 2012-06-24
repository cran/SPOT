###################################################################################
#' Meta Model Interface: Dice Kriging  
#'
#' Kriging meta model based on the DiceKriging package.
#' It usually provides good performance, but is very unstable.
#' 
#' @param rawB matrix of raw x and y values
#' @param mergedB matrix of merged x and y values, does not have replicate entries
#' @param largeDesign design points to be evaluated by the meta model 
#' @param spotConfig the list of all parameters is given.
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
####################################################################################

spotPredictDice <- function(rawB,mergedB,largeDesign,spotConfig,externalFit=NULL){	
	spotWriteLines(spotConfig$io.verbosity,1,"spotPredictDice started");
	spotInstAndLoadPackages("DiceKriging")	
	if(is.null(externalFit)){
		xNames <- setdiff(names(rawB),"y")
		x <- mergedB[xNames]
		y <- data.frame(y=mergedB$y)
		if(spotConfig$io.verbosity>2){fit<-km(design=x,response=y,nugget.estim=TRUE)}
		else{fit<-km(design=x,response=y,control=list(trace=FALSE),nugget.estim=TRUE)}
		res <- predict(fit,largeDesign,"UK")
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
		res <- predict(fit,largeDesign,"UK")	
	}
	res=res$mean #TODO: using lower/upper/sd ?
	spotWriteLines(spotConfig$io.verbosity,1,"spotPredictDice finished");
	spotConfig$seq.modelFit<-fit;
	spotConfig$seq.largeDesignY<-as.data.frame(res);
	return(spotConfig);
}
