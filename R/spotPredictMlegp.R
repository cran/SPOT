###################################################################################
#' Meta Model Interface: Maximum Likelihood Estimation for Gaussian Processes, Kriging 
#'
#' Kriging model based on mlegp package.
#' This function uses two settings, which are stored in the spotConfig parameter:\cr
#' \code{spotConfig$seq.mlegp.constantMean} Use constant mean (mu) in mlegp (=1) or linear model (=0); 1 by default\cr
#' \code{spotConfig$seq.mlegp.min.nugget} minimum value of nugget term; 0 by default\cr\cr
#' If those settings are not in spotConfig their mentioned defaults will be used. \cr\cr
#' If the numeric value of \code{spotConfig$mlegp.reduce} is smaller than the observations in \code{mergedB}, 
#' \code{spotConfig$mlegp.reduce} will specifiy how many samples should be drawn without replacement from mergedB. 
#' This can prevent explosion of time consumption in this function.
#' Mlegp can be used both for single and multi objective SPOT.
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
spotPredictMlegp <- function(rawB,mergedB,largeDesign,spotConfig,externalFit=NULL){	
    if(is.null(spotConfig$seq.mlegp.constantMean)) spotConfig$seq.mlegp.constantMean = 1;  #default handling for user options  
	if(is.null(spotConfig$seq.mlegp.min.nugget)) spotConfig$seq.mlegp.min.nugget = 0.0;	         	
	spotWriteLines(spotConfig$io.verbosity,2,"spotPredictMlegp started");	
	spotInstAndLoadPackages("mlegp")	
	if(is.null(externalFit)){
		xNames <- row.names(spotConfig$alg.roi); 
		yNames <- setdiff(names(rawB),xNames)
		x <- unname(as.matrix(mergedB[xNames]))
		y <- unname(as.matrix(mergedB[yNames]))
		#x <- unname(as.matrix(rawB[xNames]))
		#y <- unname(as.matrix(rawB[yNames]))
		if(!is.null(spotConfig$mlegp.reduce)){
			if(nrow(as.matrix(mergedB[xNames]))>spotConfig$mlegp.reduce){
				samp<-sample(1:nrow(as.matrix(mergedB[xNames])),spotConfig$mlegp.reduce,replace=F)
				x<-x[samp,]
				y<-y[samp,]
				#if(nrow(spotConfig$alg.roi)==1){
				#	x<-x[samp]
				#}else{ x<-x[,samp]
			}
		}
		constantMean <- spotConfig$seq.mlegp.constantMean
		if (constantMean != 1) {
			ones = rep(1, dim(x)[1])
			dx = cbind(ones, x)
			t = try(solve(t(dx) %*% dx), TRUE)
			if (class(t) == "try-error") {
				constantMean <-1
			}		
		}
		#if (spotConfig$seq.log.y == TRUE){ #TODO nicht hier, besser woanders
		#	y <- log(y)
		#}		
#######################################################################################################		
#		START ERROR Handling and Model Building	
#######################################################################################################		
		fit<-mlegp(X=x,Z=y, verbose = spotConfig$io.verbosity
                      , constantMean = constantMean
                      , min.nugget = spotConfig$seq.mlegp.min.nugget)
		if(is.null(fit) || any(sapply(fit,is.null))){ #mlegp will create a NULL fit sometimes, seems to be fixable by using non zero min.nugget
			#browser()
			fit<-mlegp(X=x,Z=y, verbose = spotConfig$io.verbosity
                      , constantMean = constantMean
                      , min.nugget = 1)
		}
		#TODO:Problem: adapt this second problem for mco
#######################################################################################################	
#		END ERROR Handling and Model Building		
#######################################################################################################				
		if(length(yNames)==1){
			res<-predict(fit,as.matrix(largeDesign))
		}
		else{#Distinction for multi criteria spot 
			res=list()
			for (i in 1:length(yNames)){
				res[[i]]<-predict(fit[[i]],as.matrix(largeDesign))
			}			
		}		
	}else{
		fit <-externalFit
		if(length(spotConfig$alg.resultColumn)>1){#Distinction for multi criteria spot 
			res=list()
			for (i in 1:length(spotConfig$alg.resultColumn)){
				res[[i]]<-predict(fit[[i]],as.matrix(largeDesign)[,])
			}
		}
		else{res<-predict(fit,as.matrix(largeDesign)[,])}
	}	
	spotWriteLines(spotConfig$io.verbosity,2,"spotPredictMlegp finished successfully");	
	spotConfig$seq.modelFit<-fit;
	spotConfig$seq.largeDesignY<-as.data.frame(res);
	return(spotConfig);
}
