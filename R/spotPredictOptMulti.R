###################################################################################
#' Optimize predicted meta model
#'  
#' Optimizes an existing fit of a model to get an optimal new design point. 
#' Executed after building the prediction model in the sequential SPOT step.
#' 
#' @param startPoint initial points for the optimization
#' @param spotConfig list of all options, needed to provide data for calling functions.
#' 
#' This function uses the parameter \code{spotConfig$seq.modelFit}. This is supposed to be a fit, that can be evaluated by the predict() interface.
#' The parameter \code{spotConfig$seq.predictionOpt.method} will be used to choose the optimization method to be used to find the minimum of the fitted model:\cr
#' "optim-BFGS"\cr
#' "optim-L-BFGS-B"\cr
#' "optim-CG"\cr
#' "optim-NM"\cr
#' "optim-SANN"\cr
#' "pso"\cr
#' "cmaes"\cr
#' "genoud"\cr
#' "DEoptim"\cr
#' "subplex"\cr
#' "bobyqa"\cr
#' "newuoa"\cr
#' "uobyqa"\cr
#' 
#' @return returns the list \code{spotConfig} with two new entries:\cr
#' 	spotConfig$optDesign are the parameters of the new minimal design point \cr
#'	spotConfig$optDesignY is the associated value of the objective function
#' @export
###################################################################################
spotPredictOptMulti <- function(startPoint,spotConfig){
	startPoint=as.numeric(startPoint[which.min(spotConfig$seq.largeDesignY[[1]]),])
	spotWriteLines(spotConfig$io.verbosity,2,"spotPredictOptMulti started");
	if(is.null(spotConfig$seq.predictionOpt.method)) spotConfig$seq.predictionOpt.method = "optim-BFGS";
	pNames <- row.names(spotConfig$alg.roi);
	nParam <- length(pNames)
	fit<-spotConfig$seq.modelFit;	
	tempVerbose<-spotConfig$io.verbosity
	#building the local objective function, differences depend on type of model.
	spotGetFitness <- function(x){
		spotConfig$io.verbosity=0;
		spotConfig1 <- eval(call(spotConfig$seq.predictionModel.func
                                , NULL 
								, NULL
								, as.data.frame(x)
								, spotConfig
                                , fit #external fit is used, model is only evaluated not build, therefor the NULLS are no prob
								));
		return(spotConfig1$seq.largeDesignY[[1]])
	}
	lowROI<-as.numeric(spotConfig$alg.roi[[1]]);
	upROI<-as.numeric(spotConfig$alg.roi[[2]]);
	if (spotConfig$seq.predictionModel.func=="spotPredictTree"||spotConfig$seq.predictionModel.func=="spotPredictRandomForest"){
		delta <- (upROI-lowROI)*0.1; #for tree and RF model delta needs to be rather high
	}
	else{
		delta <- rep(1e-3,length(upROI)); #default value for all other cases
	}
	subMethod=spotConfig$seq.predictionOpt.method;
	budget=spotConfig$seq.predictionOpt.budget;
	#browser()
	if (subMethod=="optim-BFGS"){
		newDesign <- optim(startPoint, spotGetFitness,method="BFGS",control=list(maxit=budget,ndeps=delta));
		newDesignPrediction <- newDesign$value;		
		newDesign <- newDesign$par;
	}
	else if (subMethod=="optim-L-BFGS-B"){
		newDesign <- optim(startPoint, spotGetFitness,method="L-BFGS-B",lower=lowROI,upper=upROI,control=list(maxit=budget,ndeps=delta)); 
		newDesignPrediction <- newDesign$value;	
		newDesign <- newDesign$par;	
	}
	else if (subMethod=="optim-CG"){
		newDesign <- optim(startPoint, spotGetFitness,method="CG",control=list(maxit=budget,ndeps=delta)); 
		newDesignPrediction <- newDesign$value;	
		newDesign <- newDesign$par;	
	}
	else if (subMethod=="optim-NM"){
		newDesign <- optim(startPoint, spotGetFitness,method="Nelder-Mead",control=list(maxit=budget,ndeps=delta)); 
		newDesignPrediction <- newDesign$value;	
		newDesign <- newDesign$par;	
	}
	else if (subMethod=="optim-SANN"){
		newDesign <- optim(startPoint, spotGetFitness,method="SANN",control=list(maxit=budget,ndeps=delta)); 
		newDesignPrediction <- newDesign$value;	
		newDesign <- newDesign$par;	
	}
	else if (subMethod=="pso"){ 
		spotInstAndLoadPackages("pso");
		newDesign <- psoptim(par=startPoint, fn=spotGetFitness,lower=lowROI,upper=upROI,control=list(maxf=budget)); 
		newDesignPrediction <- newDesign$value;	
		newDesign <- newDesign$par;	
	}
	else if (subMethod=="cmaes"){ 
		spotInstAndLoadPackages("cmaes");
		newDesign <- cma_es(startPoint, spotGetFitness,lower=lowROI,upper=upROI,control=list(maxit=ceiling(budget / (4 + floor(3 * log(length(startPoint))))))); 
		if(is.null(newDesign$par)){#error handling, see bug example in start.runit.test at end of file, reason unclear
			newDesignPrediction <- spotGetFitness(startPoint);	
			newDesign <- startPoint;
		}
		else{
			newDesignPrediction <- newDesign$value;	
			newDesign <- newDesign$par;
		}			
	}	
	else if (subMethod=="genoud"){ 
		spotInstAndLoadPackages("rgenoud");
		newDesign <- genoud(nvars=length(lowROI),starting.values=startPoint,fn<-spotGetFitness,boundary.enforcement=1,Domains=cbind(lowROI,upROI),print.level=0,pop.size=20,max.generations=floor(budget/20),wait.generations=floor(budget/20)); 
		newDesignPrediction <- newDesign$value;	
		newDesign <- newDesign$par;	
	}
	else if (subMethod=="DEoptim"){
		spotInstAndLoadPackages("DEoptim");
		newDesign <- DEoptim(fn=spotGetFitness ,lower=lowROI,upper=upROI,control=DEoptim.control(NP=20,itermax=floor(budget/20),trace=FALSE)); 
		newDesignPrediction <- newDesign$optim$bestval;	
		newDesign <- newDesign$optim$bestmem;	
	}		
	else if (subMethod=="subplex"){ 
		spotInstAndLoadPackages("subplex");
		newDesign <- subplex(startPoint, spotGetFitness ,control=list(maxit=budget)); 
		newDesignPrediction <- newDesign$value;	
		newDesign <- newDesign$par;	
	}		
	else if (subMethod=="bobyqa"){ 
		spotInstAndLoadPackages("minqa");
		newDesign <- bobyqa(startPoint,lower=lowROI,upper=upROI,spotGetFitness ,control=list(maxfun=budget,iprint=FALSE)); 
		newDesignPrediction <- newDesign$fval;	
		newDesign <- newDesign$par;	
	}		
	else if (subMethod=="newuoa"){ 
		spotInstAndLoadPackages("minqa");
		newDesign <- newuoa(startPoint,spotGetFitness ,control=list(maxfun=budget,iprint=FALSE)); 
		newDesignPrediction <- newDesign$fval;	
		newDesign <- newDesign$par;	
	}		
	else if (subMethod=="uobyqa"){ 
		spotInstAndLoadPackages("minqa");
		newDesign <- uobyqa(startPoint,spotGetFitness ,control=list(maxfun=budget,iprint=FALSE)); 
		newDesignPrediction <- newDesign$fval;	
		newDesign <- newDesign$par;	
	}
	else{
		newDesign <- startPoint;
		newDesignPrediction <- spotGetFitness(startPoint);
	}
    #newDesign <- as.data.frame(rbind(newDesign,  largeDesign[1:spotConfig$seq.design.new.size-1,]));	
	#names(newDesign)=xNames; 
	spotPrint(spotConfig$io.verbosity,2,newDesign);
	spotWriteLines(spotConfig$io.verbosity,2,"spotPredictOptMulti finished");
	
	spotConfig$optDesign<-newDesign;
	spotConfig$optDesignY<-newDesignPrediction;
	spotConfig$io.verbosity<-tempVerbose
	return(spotConfig);
}


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