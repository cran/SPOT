###################################################################################
#' Multi criteria optimization of predicted surrogate models
#'  
#' Uses by default the number of design points expected as population size for multi criteria optimization of the
#' models build in the current sequential step. Executed after building the prediction models.
#' 
#' @param startPoint initial information, not yet used
#' @param spotConfig list of all options, needed to provide data for calling functions.
#' 
#' This function uses the parameter \code{spotConfig$seq.modelFit}. This is supposed to be a list of fits (i.e. one fit for each objective), 
#' that can be evaluated by calling the original model interface with that fit list.
#' The parameter \code{spotConfig$seq.predictionOpt.method} will be used to choose the optimization method to be used to find the minimum of the fitted model:\cr
#' "nsga2" \cr
#' "sms-emoa"\cr
#' 
#' @return returns the list \code{spotConfig} with two new entries:\cr
#' 	spotConfig$optDesign are the parameters of the new pareto optimal design points \cr
#'	spotConfig$optDesignY are the associated values of the objective functions (e.g. meta model values)
#' @seealso \code{\link{spotPredictOptMulti}} solves the same task for single objective optimization (i.e. just one surrogate model)\cr
#' See \code{\link{spotSmsEmoa}} for the used SMS-EMOA implementation
#' @export
###################################################################################
spotParetoOptMulti <- function(startPoint,spotConfig){
	#startPoint=as.numeric(startPoint[1,])
	spotWriteLines(spotConfig$io.verbosity,2,"spotParetoOptMulti started");
	if(is.null(spotConfig$seq.predictionOpt.method)) spotConfig$seq.predictionOpt.method = "sms-emoa"; 
	pNames <- row.names(spotConfig$alg.roi);
	nParam <- length(pNames)
	fit<-spotConfig$seq.modelFit;	
	tempVerbose<-spotConfig$io.verbosity
	#building the local objective function
	spotGetFitness <- function(x){
		if(any(is.na(x))){return(rep(NA,length(x)))}  #sms-emoa starts with NA values to determine dimension
		spotConfig$io.verbosity=0;
		spotConfig1 <- eval(call(spotConfig$seq.predictionModel.func
                                , NULL 
								, NULL
								, as.data.frame(x)
								, spotConfig
                                , fit #external fit is used, model is only evaluated not build, therefore the NULLS are no prob
								));
		return(as.numeric(spotConfig1$seq.largeDesignY))
	}
	lowROI<-as.numeric(spotConfig$alg.roi[[1]]);
	upROI<-as.numeric(spotConfig$alg.roi[[2]]);
	subMethod=spotConfig$seq.predictionOpt.method;
	budget=spotConfig$seq.predictionOpt.budget;
	psize=spotConfig$seq.predictionOpt.psize;
	if(is.null(psize))psize=spotConfig$seq.design.new.size;  #limit population size to number of points to be evaluated!
	#browser()
	if (subMethod=="nsga2"){
		spotInstAndLoadPackages("mco");		
		if((psize%%4)!=0)psize=psize+4-(psize%%4) #make sure that psize is multiple of 4 for nsga2
		r1 <- nsga2(spotGetFitness, nParam, length(spotConfig$alg.resultColumn),
           generations=floor(budget/psize), popsize=psize,
           lower.bounds=lowROI,
           upper.bounds=upROI)
		newDesignPrediction <- r1$value;		
		newDesign <- r1$par;
	}
	else if (subMethod=="sms-emoa"){
		r1 <- spotSmsEmoa(spotGetFitness,
           lower=lowROI,
           upper=upROI,
		   control=list(mu=psize,maxeval=budget))
		newDesignPrediction <- t(r1$value);		
		newDesign <- t(r1$par);
	}
	else{
		warning("Invalid submethod for spotParetoOptMulti - No optimization done.")
		newDesign <- NULL;
		newDesignPrediction <- NULL;
	}

	spotPrint(spotConfig$io.verbosity,2,newDesign);
	spotWriteLines(spotConfig$io.verbosity,2,"spotParetoOptMulti finished");
	
	spotConfig$optDesign<-newDesign;
	spotConfig$optDesignY<-newDesignPrediction;
	spotConfig$io.verbosity<-tempVerbose
	return(spotConfig);
}