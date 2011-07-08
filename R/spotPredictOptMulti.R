###################################################################################
#' SPOT Optimize predicted model
#'  
#' Optimizes a fit of a model to get an optimal new design point. Executed after building the prediction model.
#' 
#' @param startPoint initial point of optimization
#' @param spotConfig list of all options, needed to provide data for calling functions.
#' 
#' This function uses the parameter \code{spotConfig$seq.modelFit}. This is supposed to be a fit, that can be evaluated by the predict() interface.
#' The parameter \code{spotConfig$seq.predict.subMethod} will be used to choose the optimization method to be used to find the minimum of the fitted model:\cr
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
###################################################################################
spotPredictOptMulti <- function(startPoint,spotConfig){
	spotWriteLines(spotConfig$io.verbosity,2,"spotPredictOptMulti started");
	pNames <- row.names(spotConfig$alg.roi);
	nParam <- length(pNames)
	fit<-spotConfig$seq.modelFit;	
	makeNNames <- function(n) { Map(function(i) paste("x", i, sep = ""), 1:n) }
	#building the local objective function, differences depend on type of model.
	if (spotConfig$seq.predictionModel.func=="spotPredictLm"){#TODO: This might not work, due to missing normalization? See spotPredictLm
		spotGetFitness <- function(x){names(x)<- as.character(makeNNames(nParam)); predict(fit,as.data.frame(rbind(x)));}	
	}
#	else if (spotConfig$seq.predictionModel.func=="spotPredictTree"||spotConfig$seq.predictionModel.func=="spotPredictKrig"){
#		spotGetFitness <- function(x){names(x) <- row.names(spotConfig$alg.aroi); predict(fit,as.data.frame(rbind(x)));}
#	}
	else{
		spotGetFitness <- function(x){names(x) <- row.names(spotConfig$alg.aroi); predict(fit,as.data.frame(rbind(x)));}	
	}	
	#res <- predict(fit,largeDesign);
	#largeDesignPrediction <- res;
	#largeDesignPrediction <- as.data.frame(largeDesignPrediction[order(res,decreasing=FALSE)]);	
    #largeDesign <-  as.data.frame(largeDesign[order(res,decreasing=FALSE),]);
	#startPoint <-  as.numeric(largeDesign[1,]);
	lowROI<-as.numeric(spotConfig$alg.roi[[1]]);
	upROI<-as.numeric(spotConfig$alg.roi[[2]]);
	if (spotConfig$seq.predictionModel.func=="spotPredictTree"||spotConfig$seq.predictionModel.func=="spotPredictRandomForest"){
		delta <- (upROI-lowROI)*0.1; #for tree and RF model delta needs to be rather high
	}
	else{
		delta <- rep(1e-3,length(upROI)); #default value for all other cases
	}
	subMethod=spotConfig$seq.predict.subMethod;
	budget=1000; #TODO: Should be set in spotGetOptions
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
		newDesignPrediction <- newDesign$value;	
		newDesign <- newDesign$par;	
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
		newDesignPrediction <- predict(fit, startPoint);
	}
    #newDesign <- as.data.frame(rbind(newDesign,  largeDesign[1:spotConfig$seq.design.new.size-1,]));	
	#names(newDesign)=xNames; 
	spotPrint(spotConfig$io.verbosity,2,newDesign);
	spotWriteLines(spotConfig$io.verbosity,2,"spotPredictOptMulti finished");
	
	spotConfig$optDesign<-newDesign;
	spotConfig$optDesignY<-newDesignPrediction;
	return(spotConfig);
}