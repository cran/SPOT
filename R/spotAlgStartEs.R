# es.R
#thomas.bartz-beielstein@udo.edu
#liftFinal -> 10 times lift(), so that simfx is not evaluated

## rm(list=ls()); source("/home/bartz/workspace/SvnSpot/trunk/SPOT/R/spot.R")
## spot("es1.conf","auto","/home/bartz/workspace/SvnSpot/trunk/SPOT/R")

### Threshold functions ###############################

###################################################################################################
#' get Success Rate
#'
#' This function is a threshold function, evaluating the success rate.
#'
#' @param gen Generation number
#' @param pop Population to be evaluated (only the generation \code{gen} will be evaluated)
#'
#' @return number \cr
#' - the success rate of the given population
#'
#' @references  \code{\link{spotAlgEs}} \code{\link{spotAlgStartEs}} 
###################################################################################################
spotAlgEsGetSuccessRate <- function(gen,pop){
	succ <- length(subset(pop, pop$generation==gen)[,1])
	len <- length(pop[,1])
	return(succ/len)
}  

### Fitness functions ###################################

###################################################################################################
#' Function Call to Fitness functions
#'
#' This function is used by the ES-implementation in SPOT to call the different fitness functions:
#' \code{\link{spotNoisyBraninFunction}}, \code{\link{spotAlgEsSphere}}, \code{\link{spotAlgEsSphere1}}, \code{\link{spotAlgEsBanana}}, \code{\link{spotAlgEsWild}}, \code{\link{spotAlgEsRastrigin}}.
#'
#' @param x vector to be evaluated by the fitness function
#' @param fType name of fitness function, choose by \code{branin, sphere, sphere1, banana, wild, rastrigin}
#'
#' @return number \code{y} \cr
#' - \code{res} is the function value of the corresponding vector \code{x} 
#'
#' @references  \code{\link{spotAlgEs}} \code{\link{spotAlgStartEs}}
###################################################################################################
spotAlgEsF <- function(x, fType){
	switch(fType,
			branin=spotNoisyBraninFunction(x),
			sphere=spotAlgEsSphere(x),
			sphere1=spotAlgEsSphere1(x),
			banana=spotAlgEsBanana(x),
			wild = spotAlgEsWild(x),
			rastrigin = spotAlgEsRastrigin(x)
	)
}

###################################################################################################
#' Wild Function
#'
#' This is a fitness function that could be used for the ES which is an example algorithm to be optimized by SPOT.
#'
#' @param x vector to be evaluated by the fitness function
#' @param noise the noise is added to the result
#'
#' @return number \code{res} \cr
#' - \code{res} is the function value of the corresponding vector \code{x}
#'
#' @references  \code{\link{spotAlgEsF}}
###################################################################################################
#TODO: noise wird nicht benutzt? 
spotAlgEsWild <- function (x,noise=0.0){
	res <- 10*sin(0.3*x)*sin(1.3*x^2) + 0.00001*x^4 + 0.2*x+80  
}

###################################################################################################
#' Rosenbrock Banana Function
#'
#' This is a fitness function that could be used for the ES which is an example algorithm to be optimized by SPOT.
#'
#' @param x vector to be evaluated by the fitness function
#' @param noise the noise is added to the result, defaults to zero
#'
#' @return number \code{res} \cr
#' - \code{res} is the function value of the corresponding vector \code{x}
#'
#' @references  \code{\link{spotAlgEsF}}
###################################################################################################
#TODO: noise wird nicht benutzt? 
spotAlgEsBanana <- function(x,noise=0.0) {   ## Rosenbrock Banana function
	x1 <- x[1]
	x2 <- x[2]
	res <- 100 * (x2 - x1 * x1)^2 + (1 - x1)^2
}

###################################################################################################
#' Gradient Function of 'spotAlgEsBanana'
#'
#' This is a fitness function that could be used for the ES which is an example algorithm to be optimized by SPOT.
#'
#' @param x vector to be evaluated by the fitness function
#'
#' @return number \code{res} \cr
#' - \code{res} is the function value of the corresponding vector \code{x}
#'
#' @references  \code{\link{spotAlgEsBanana}} \code{\link{spotAlgEsF}}
###################################################################################################
spotAlgEsBananaGradient <- function(x) { ## Gradient of `spotAlgEsBanana'
	x1 <- x[1]
	x2 <- x[2]
	c(-400 * x1 * (x2 - x1 * x1) - 2 * (1 - x1),
			200 *      (x2 - x1 * x1))
}
#TODO: Funktion wird nicht verwendet

###################################################################################################
#' Sphere Function
#'
#' This is a fitness function that could be used for the ES which is an example algorithm to be optimized by SPOT.
#'
#' @param x vector to be evaluated by the fitness function
#'
#' @return number \code{res} \cr
#' - \code{res} is the function value of the corresponding vector \code{x}
#'
#' @references  \code{\link{spotAlgEsSphere1}} \code{\link{spotAlgEsF}}
###################################################################################################
spotAlgEsSphere <- function(x){
	res <- sum(x^2)
}

###################################################################################################
#' Sphere Function
#'
#' This is a fitness function that could be used for the ES which is an example algorithm to be optimized by SPOT.
#'
#' @param x vector to be evaluated by the fitness function
#'
#' @return number \code{res} \cr
#' - \code{res} is the function value of the corresponding vector \code{x}
#'
#' @references  \code{\link{spotAlgEsSphere}} \code{\link{spotAlgEsF}}
###################################################################################################
spotAlgEsSphere1 <- function(x){
	sum <- 0
	for(i in 1:length(x)){
		sum <- sum+(i^2*x[[i]]^2 - i)^2
	}
	## Sum[i^2*(x[[i]]-i)^2, {i, 1, ndim}] )
	sum
}

###################################################################################################
#' rastrigin
#'
#' This is a fitness function that could be used for the ES which is an example algorithm to be optimized by SPOT.
#'
#' @param x vector to be evaluated by the fitness function
#'
#' @return number \code{res} \cr
#' - \code{res} is the function value of the corresponding vector \code{x}
#'
#' @references  \code{\link{spotAlgEsF}}
###################################################################################################
spotAlgEsRastrigin <- function(x){
	res <- sum( x^2 - 10*cos(2*pi*x) + 10)
}

### Initialization ########################################################

###################################################################################################
#' Individual Initialization
#'
#' Creates a new Individual for the Evolution Strategy implemented in SPOT.
#'
#' @param s sigma
#' @param n n
#' @param dimension number of target function dimension
#' @param noise noise to be added to fitness value
#' @param fName function name
#' @param gen generation
#' @param low lower limit
#' @param high upper limit
#' @param des des
#'
#' @return numeric vector \cr
#' - contains x value, sigma value, real fitness value, fitness with noise, and generation number
#'
#' @references  \code{\link{spotAlgEs}} \code{\link{spotAlgEsF}}
###################################################################################################
spotAlgEsIndividualInitial <- function(s,
		dimension,
		n,
		noise=0,
		fName,
		gen,
		low=-1.0,
		high=1.0,
		des){
	c(x = x <- low + (high-low)*des,     #  *runif(dimension),
			sigma = sigma <- rep(s,n),
			realFitness = realFitness <- spotAlgEsF(x,fName),
			fitness = fitness <- realFitness + rnorm(1,0,noise),
			generation = generation <- gen
	)
}

###################################################################################################
#' spotAlgEsInitParentPop
#'
#' Creates initial parent population
#'
#' @param sigmaInit initial sigma value (standard deviation)
#' @param dimension number of target function dimension
#' @param nSigma number of standard deviations
#' @param noise noise to be added to fitness value
#' @param fName function name
#' @param gen generation
#' @param low lower limit
#' @param high upper limit
#' @param mue number of parents in the ES
#'
#' @return matrix \cr
#' - holds the parent population created by this function
#'
#' @references  \code{\link{spotAlgEs}} \code{\link{spotAlgEsIndividualInitial}}
###################################################################################################
spotAlgEsInitParentPop <- function(sigmaInit, dimension, nSigma, noise, fName, gen, low, high, mue)
{
	parentPop <- NULL	
	ld <- randomLHS(mue, dimension)
	###ld <- optimumLHS(mue, dimension, 2, 0.1)
	for(i in 1:mue){      
		parentPop <- rbind(parentPop,
				spotAlgEsIndividualInitial(s=sigmaInit,
						dimension=dimension,
						n=nSigma,
						noise=noise,
						fName=fName,
						gen=gen,
						low=low,
						high=high,
						des = ld[i,]))
	}                                        
	return(parentPop)
}

### Recombination ######################################################
###################################################################################################
#' Marriage with replace
#'
#' Recombination function for the Evolution Strategy.
#'
#' @param pop Population
#' @param rhoVal number of parents involved in the procreation of an offspring
#'
#' @return \code{pop} \cr
#' - \code{pop} Population
#'
#' @references  \code{\link{spotAlgEs}} \code{\link{spotAlgEsMarriageWithReplace}} 
###################################################################################################
spotAlgEsMarriageWithReplace <- function(pop,rhoVal){
	return(pop[sample(nrow(pop), rhoVal, replace=TRUE),])
}
#TODO:Funktion wird garnicht benutzt?

###################################################################################################
#' Marriage
#'
#' Recombination function for the Evolution Strategy.
#'
#' @param pop Population
#' @param rhoVal number of parents involved in the procreation of an offspring
#'
#' @return \code{pop} \cr
#' - \code{pop} Population
#'
#' @references  \code{\link{spotAlgEs}} \code{\link{spotAlgEsMarriage}} 
###################################################################################################
spotAlgEsMarriage <- function(pop,rhoVal){
	return(pop[sample(nrow(pop), rhoVal, replace=FALSE),])
}

###################################################################################################
#' spotAlgEsInterRecoBeSw02
#'
#' Recombination function for the Evolution Strategy.
#'
#' @param parents Parent individuals
#' @param dimension number of dimensions
#' @param nSigma number of standard deviations
#' @param objType string, default is "obj"
#'
#' @references  \code{\link{spotAlgEs}} \code{\link{spotAlgEsInterReco}} \code{\link{spotAlgEsDominantReco}} 
###################################################################################################
spotAlgEsInterRecoBeSw02 <- function(parents, dimension, nSigma, objType="obj"){
	rObject <- NULL
	if(objType=="obj"){
		for(i in 1:dimension)  rObject <- c(rObject, mean(parents[,i]))
	}
	else{
		for(i in 1:nSigma)  rObject <- c(rObject, mean(parents[,I(dimension+i)]))
	}
	return(rObject)
}

###################################################################################################
#' spotAlgEsInterReco
#'
#' Recombination function for the Evolution Strategy.
#'
#' @param parents Parent individuals
#' @param rhoVal number of parents involved in the procreation of an offspring
#' @param dimension number of dimensions
#' @param nSigma number of standard deviations
#' @param objType string, default is "obj"
#'
#' @references  \code{\link{spotAlgEs}} \code{\link{spotAlgEsInterRecoBeSw02}} \code{\link{spotAlgEsDominantReco}} 
###################################################################################################
spotAlgEsInterReco <- function(parents, rhoVal, dimension, nSigma, objType="obj"){
	rObject <- NULL
	if(objType=="obj"){
		for(i in 1:dimension){
			select2 <- sample(rhoVal,min(2,rhoVal)) # necessary for (1,+\mue)
			# if(verbosity==2)   print(paste("Selected for XReco: ", select2))
			rObject <- c(rObject, mean(parents[select2,i]))
		}
	}
	else{
		for(i in 1:nSigma){
			select2 <- sample(rhoVal,min(2,rhoVal)) # necessary for (1,+\mue)
			# if(verbosity==2)   print(paste("Selected for SReco: ", select2))
			rObject <- c(rObject, mean(parents[select2,I(dimension+i)]))
		}
	}
	return(rObject)
}

###################################################################################################
#' spotAlgEsDominantReco
#'
#' Recombination function for the Evolution Strategy.
#'
#' @param parents Parent individuals
#' @param rhoVal number of parents involved in the procreation of an offspring
#' @param dimension number of dimensions
#' @param nSigma number of standard deviations
#' @param objType string, default is "obj"
#'
#' @references  \code{\link{spotAlgEs}} \code{\link{spotAlgEsInterRecoBeSw02}}  \code{\link{spotAlgEsInterReco}} 
###################################################################################################
spotAlgEsDominantReco <- function(parents, rhoVal, dimension, nSigma, objType="obj"){
	rObject <- NULL
	if(objType=="obj"){
		for (i in 1:dimension) rObject <- c(rObject, parents[sample(rhoVal,1),i])
	}
	else
	{
		for (i in (1:nSigma))
			rObject <- c(rObject, parents[sample(rhoVal,1),I(dimension+i)])
	}
	return(rObject)
}


### Mutation #############################################################

###################################################################################################
#' spotAlgEsStratMutation
#'
#' Mutation function for the ES. 
#'
#' @param strat Strat
#' @param tau0 Tau0
#' @param tau Tau, learning parameter for self adaption
#' @param sigmaRestart Sigma on restart
#' @param sigmaInit initial sigma value (standard deviation)
#'
#' @return number \code{s} \cr
#' - \code{s} is the new sigma value
###################################################################################################
spotAlgEsStratMutation <- function(strat, tau0, tau, sigmaRestart, sigmaInit){
	if (runif(1) < sigmaRestart){
		s <- rep(sigmaInit, length(strat))
	}
	else{
		s <- exp(tau0*rnorm(1,0,1))*strat*exp(tau*rnorm(length(strat),0,1))
	}
	return(s)
}
###################################################################################################
#' spotAlgEsObjMutation
#'
#' Mutation function for the ES 
#'
#' @param obj Object
#' @param strat Strat
###################################################################################################
spotAlgEsObjMutation <- function(obj, strat){
	return(obj + strat*rnorm(length(obj),0,1))
}

### Selection ##################################################################
###################################################################################################
#' spotAlgEsSelection
#'
#' Selection function for the ES
#'
#' @param parentPop Parent population
#' @param offspringPop Offspring population
#' @param sel number of individuals to be selected
#' @param gen generation number
#' @param iter current iteration number
#' @param maxIter maximum iteration number
###################################################################################################      
spotAlgEsSelection <- function(parentPop, offspringPop, sel, gen, iter, maxIter){
	mue <- nrow(parentPop)
	if(sel==-1)
	{ # plus selection
		parentPop <- rbind(parentPop,offspringPop)
	}
	else
	{ ## kappa selection 
		##parentPop <- parentPop[parentPop$generation>I(gen-sel)]
		##parentPop <- rbind(parentPop,offspringPop)
		## print(c(gen, sel))
		## print(parentPop)
		parentPop <- parentPop[parentPop[,"generation"] > gen - sel,]
		## cat("reduced parentPop: \n")
		## print(parentPop)
		parentPop <- rbind(parentPop,offspringPop)
		## cat("new parentPop: \n")
		## print(parentPop)
		
	}
	parentPop <- parentPop[order(parentPop$fitness),]
	row.names(parentPop) <- 1:nrow(parentPop)
	return(parentPop[1:mue,])
}

### Termination #################################################################
###################################################################################################
#' Termination hps
#'
#' Termination function for the ES. Terminates the ES at a given target value.
#'
#' @param xk current best value of the optimization run
#' @param xOpt target value of the optimization run
#'
#' @return \code{boolean} \cr
#' - TRUE as long as the current value has not yet reached its limit. Once the
#' given termination criterion is reached the function returns FALSE.
###################################################################################################  
spotAlgEsHps <- function(xk, xOpt){
	n <- length(xk)
	tfVec <- 10*sqrt(n)*(xk-xOpt)<=1
	return(is.element(FALSE, tfVec))
}

###################################################################################################
#' Termination
#'
#' Handles the termination functions for the ES. 
#'
#' @param it iteration 
#' @param maxIt Maximum number of iterations
#' @param ge generation
#' @param maxGe Maximum number of generations
#' @param xk current best value of the optimization run
#' @param xOpt target value of the optimization run
#' @param term string that tells which termination criterion matters: \code{"gen"} terminates after given number of generations.
#' \code{"iter"} terminates after given number of iterations.  \code{"hps"} terminates at fiven target value.
#'
#' @return \code{boolean} \cr
#' - TRUE as long as the current value has not yet reached its limit. Once the
#' given termination criterion is reached the function returns FALSE.
#'
#' @references  \code{\link{spotAlgEs}} \code{\link{spotAlgEsHps}}
###################################################################################################
spotAlgEsTermination <- function(it, maxIt, ge, maxGe, xk, xOpt, term){
	switch(term,
			"gen" = (ge < maxGe),
			"iter" = (it < maxIt),
			"hps" = spotAlgEsHps(xk,xOpt),
			(it < maxIt)
	)
}

### Main Loop #######################################################  

###################################################################################################
#' Evolution Strategy Implementation for SPOT package
#'
#' This function is used by \code{\link{spotAlgStartEs}} as a main loop for running
#' the Evolution Strategy with the given parameter set specified by SPOT.
#'
#' @param mue number of parents, default is \code{10}
#' @param nu number, default is \code{10}
#' @param dimension dimension number of the target function, default is \code{2}
#' @param mutation string of mutation type, default is \code{"selfA"}
#' @param sigmaInit initial sigma value (standard deviation), default is \code{1.0}
#' @param nSigma number of standard deviations, default is \code{1}
#' @param tau0 number, default is \code{0.0}
#' @param tau number, learning parameter for self adaption, default is \code{1.0}
#' @param rho number of parents involved in the procreation of an offspring (mixing number), default is \code{"bi"}
#' @param sel number of selected individuals, default is \code{1}
#' @param stratReco string, Recombination operator for strategy variables, default is \code{"1"}
#' @param objReco string, Recombination operator for object variables, default is \code{"2"}
#' @param maxGen number of generations, stopping criterion, default is \code{Inf}
#' @param maxIter number of iterations, stopping criterion, default is \code{100}
#' @param seed number, random seed, default is \code{1}
#' @param noise number, value of noise added to fitness values, default is \code{0.0}
#' @param thrs threshold string, default is \code{"no"}
#' @param thrsConstant number, default is \code{0.0}
#' @param fName string, name of fitness function, default is \code{"sphere"}
#' @param lowerLimit number, lower limit for search space, default is \code{-1.0}
#' @param upperLimit number, upper limit for search space, default is \code{1.0}
#' @param verbosity defines output verbosity of the ES, default is \code{0}
#' @param plotResult boolean, asks if results are plotted, default is \code{FALSE}
#' @param logPlotResult boolean, asks if plot results should be logarithmic, default is \code{FALSE}
#' @param term string, which termination criterion should be used, default is \code{"iter"}
#' @param resFileName string, name of the file the results are written to, default is \code{"es.res"}
#' @param sigmaRestart number, value of sigma on restart, default is \code{0.1}
#' @param preScanMult initial population size is multiplied by this number for a pre-scan, default is \code{1}
#' @param globalOpt termination criterion on reaching a desired optimum value, default is \code{rep(0,dimension)}
#' @param conf config number passed to the result file, default is \code{-1}
#'
#' @references  \code{\link{SPOT}} \code{\link{spotAlgStartEs}} 
###################################################################################################
spotAlgEs <- function(mue = 10,
		nu = 10,
		dimension = 2,
		mutation = "selfA",
		sigmaInit = 1.0,
		nSigma = 1,
		tau0 = 0.0,
		tau = 1.0,
		rho = "bi",
		sel = 1,
		stratReco = "1",
		objReco = "2",
		maxGen = Inf,
		maxIter = 100,
		seed = 1,
		noise = 0.0,
		thrs = "no",
		thrsConstant = 0.0,
		fName = "sphere",
		lowerLimit = -1.0,
		upperLimit = 1.0,
		verbosity=0,
		plotResult=FALSE,
		logPlotResult=FALSE,
		term="iter",
		resFileName = "es.res",
		sigmaRestart = 0.1,
		preScanMult= 1,
		globalOpt=rep(0,dimension),
		conf   = -1){                  # /WK/
	# load packages needed for this 
	spotInstAndLoadPackages("lhs")
	### Parameter corrections
	lambda <- round(mue*nu)
	mue <- round(mue)
	nSigma <- round(nSigma)
	
	if(fName=="banana"){
		dimension <- 2
		nSigma <- min(nSigma,2)
	}
	if(fName=="wild"){
		dimension <- 1
		nSigma <- 1
	}
	nSigma <- min(nSigma,dimension) # correction: if nSigma > dimension
	lambda <- max(mue,lambda) # correction: if lambda < mue
	if(plotResult==TRUE){
		logPlotResult=FALSE
	}
	###
	tauMult <- tau #save tau multiplier for res-file
	if(nSigma==1){
		tau0=0.0
		###  tau <- tau/sqrt(dimension)
	}
	#else{
	#  tau0 <- tau/sqrt(2*dimension)
	#  tau <- tau/sqrt(2*sqrt(dimension))
	#}
	###
	if(rho=="bi"){
		rhoVal=min(2,mue) # necessary for (1,+\mue)
	}
	else{
		rhoVal=mue
	}  
	set.seed(seed)
	parentPop <- NULL
	gen <- 0
	succ <- 1
	if(verbosity>=1){
		sigmaAvgList <- rep(sigmaInit,nSigma)
		sigmaMedList <- rep(sigmaInit,nSigma)
	}
	bestFitness <- NULL
	realBest <-  NULL
	allTimeBest <- NULL
	currentBest <- 0.0
	###
	## Perform pre-scan:
	## the initial population size is multiplied by preScanMult, say 10. Then the mue best ind are
	## selected from 10*mue individuals
	if (gen == 0)
		###
		parentPop <- spotAlgEsInitParentPop(sigmaInit, dimension, nSigma, noise, fName, gen, lowerLimit, upperLimit, round(mue*preScanMult))
	parentPop <- data.frame(parentPop)
	parentPop <- parentPop[order(parentPop$fitness),]
	###
	if(verbosity==2) print(parentPop)
	
	bestInd <- parentPop[1,]
	bestFitness <- parentPop$fitness[[1]]
	allTimeBest <- bestFitness
	realBest <- parentPop$realFitness[[1]]
	parentPop<-parentPop[1:mue,]
	### End pre-scan
	
	iter <-  0
	##while(iter < maxIter && gen < maxGen ){
	while(currentBest < Inf & spotAlgEsTermination(it=iter, maxIt=maxIter, ge=gen, maxGe=maxGen, xk=bestInd[1:dimension], xOpt=globalOpt, term=term )){
		gen <- gen +1
		###
		if(verbosity==2)   print(paste("Gen:", gen))
		
		offspringPop <- NULL
		for(i in 1:lambda){
			marriagePop <- NULL
			marriagePop <- spotAlgEsMarriage(parentPop, rhoVal)
			###
			if(verbosity==2)   print(paste("MarriagePop for Offspring:", i))
			if(verbosity==2)   print(marriagePop)
# Recombination
			stratRecombinant <- switch(stratReco,
					"interRecoBeSw02" = spotAlgEsInterRecoBeSw02(marriagePop, dimension, nSigma, objType="strat"),
					"inter" = spotAlgEsInterReco(marriagePop, rhoVal, dimension, nSigma, objType="strat"),
					"disc" = spotAlgEsDominantReco(marriagePop, rhoVal, dimension, nSigma, objType="strat"),
					"no" = as.matrix(marriagePop[1,I(dimension+1):I(dimension+nSigma)]))
			
			objRecombinant <- switch(objReco,
					"interRecoBeSw02" = spotAlgEsInterRecoBeSw02(marriagePop, dimension, nSigma, objType="obj"),
					"inter" = spotAlgEsInterReco(marriagePop, rhoVal, dimension, nSigma, objType="obj"),
					"disc" = spotAlgEsDominantReco(marriagePop, rhoVal, dimension, nSigma, objType="obj"),
					"no" = as.matrix(marriagePop[1,1:dimension]))
			###
			if(verbosity==2)   print(paste("stratRecombinant:", i))
			if(verbosity==2)   print(stratRecombinant)
			if(verbosity==2)   print(paste("objRecombinant:", i))
			if(verbosity==2)   print(objRecombinant)
			
			sigmaNew <- switch(mutation,
					"no" = stratRecombinant,
					"selfA" = spotAlgEsStratMutation(stratRecombinant, tau0, tau, sigmaRestart, sigmaInit))
			####
			xNew <- switch(mutation,
					"no" = objRecombinant,
					"selfA" = spotAlgEsObjMutation(objRecombinant,sigmaNew))
			###
			if(verbosity==2)   print(paste("sigmaNew:", i))
			if(verbosity==2)   print(sigmaNew)
			if(verbosity==2)   print(paste("xNew:", i))
			if(verbosity==2)   print(xNew)
			
			result <- spotAlgEsF(xNew,fName)
			realFitness <- result
			fitness <- result
			
			iter <- iter +1
			offspring <- NULL
			offspring <- c(x=x <- xNew,
					sigma=sigma <- sigmaNew ,
					realFitness=realFitness <-  realFitness,
					fitness=fitness <- fitness,
					generation=generation <- gen)
			#####
			if(verbosity==2)   print(offspring)
			offspringPop <- rbind(offspringPop, offspring)
		}
		row.names(offspringPop) <- 1:nrow(offspringPop)
		offspringPop <- data.frame(offspringPop)
		###
		if(verbosity==2)   print("OffspringPop")
		if(verbosity==2)   print(offspringPop)
		
		parentPop <- spotAlgEsSelection(parentPop,offspringPop, sel, gen, iter, maxIter)
		###
		if(verbosity==2)   print("parentPop")
		if(verbosity==2)   print(parentPop)
		
		succ <- spotAlgEsGetSuccessRate(gen,parentPop)
		currentBest <- parentPop$fitness[[1]]
		currentReal <- parentPop$realFitness[[1]]
		
		if(verbosity>=1) {
#      currentSigma<- parentPop[1,I(dimension+1)]
#      currentSigma<- parentPop[1,I(dimension+1):I(dimension+nSigma)]
			currentSigma<- parentPop[1,I(dimension+1):I(dimension+nSigma)]
			if(verbosity==2){
				print(c(currentBest, currentReal, currentSigma))
			}
			sigmaAvgList <- c(sigmaAvgList, mean(unlist(currentSigma)))
			if(verbosity==2){
				cat("sigmaAvgList:",sigmaAvgList,"\n")
			}
			sigmaMedList <- c(sigmaMedList, median(unlist(currentSigma)))
			if(verbosity==2){
				cat("sigmaMedList:",sigmaMedList,"\n")
			}
		}
		# store only the doe values
		if(verbosity==0){
			realBest <- currentReal
			bestFitness <- currentBest
			if (currentBest < allTimeBest){
				allTimeBest <- currentBest
				bestInd <- parentPop[1,]
			}
		}
		else{
			## log every generation:
			##
			realBest <-  c(realBest, currentReal)
			allBest <- allTimeBest[length(allTimeBest)]
			bestFitness <- c(bestFitness, currentBest)
			####
			####cat(gen, "fitness: ", currentBest, "\n")
			if (currentBest < allBest) {
				allTimeBest <- c(allTimeBest, currentBest)
				bestInd <- parentPop[1,]
			}
			else{
				allTimeBest <- c(allTimeBest, allBest)
			}
		}
		
		#### Plotting ##################################
		if(sel==-1){
			stratName="Plus"}
		else{
			stratName="Kappa"}
		
		if(verbosity>=1 && plotResult==TRUE){
			par(mfrow=c(2,1))
			#xlim=c(1,maxIter/lambda)
			plot(bestFitness, ylim=c(min(min(bestFitness), min(realBest)), max(max(bestFitness), max(realBest))), type="l",
					xlab = paste( "Generation: " , as.character(gen), "Fitness: ", as.character(currentBest)),
					main = paste(as.character(mue), stratName, as.character(lambda), ", nSgm: ",as.character(nSigma), ", tau0: ", as.character(tau0), ", tau: ", as.character(tau)  )
			)
			lines(realBest, col="red", type="b")
			plot(sigmaAvgList, type="l", col="green", xlab = paste( "Generation: " , as.character(gen), "AvgSgm: ",
							as.character(sigmaAvgList[length(sigmaAvgList)])))
			lines(sigmaMedList, col="blue", type="l")
			## TBB 25 2 2009:
			##
		}
		####
		if(verbosity>=1 && logPlotResult==TRUE){
			par(mfrow=c(2,1))
			##xlim=c(1,maxIter/lambda)
			plot(log(bestFitness), ylim=c(min(min(log(bestFitness)), min(log(realBest))), max(max(log(bestFitness)), max(log(realBest)))), type="b",
					xlab = paste( "Generation: " , as.character(gen), "Fitness: ", as.character(currentBest)),
					main = paste(as.character(mue), stratName, as.character(lambda), ", nSgm: ",as.character(nSigma), ", tau0: ", as.character(tau0), ", tau: ", as.character(tau)  )           
			)
			lines(log(realBest), col="red", type="b")
			plot(log(sigmaAvgList), type="l", col="green")
			lines(log(sigmaMedList), col="blue", type="l")
		}
	}
	if(verbosity>=1){
		print(list(real=realBest,
						best=bestFitness,
						allTime=allTimeBest,
						bestInd=bestInd))
	}
	##print(  realBest[[length(realBest)]])
	##
	##data frame written to the DOE file:
	##(1) bestInd is the allTimeBestInd, and *not* the bestInd in the last generation.
	##(2) realBest is the fitness of the bestInd (that is not necessarily a member of the last generation)
	##(3) allTimeBest is the best fitness found during the whole optimization run, the (noisy) fitness of the bestInd. 
	##(4) NoisyFitness is the best Fitness in the last generation.
	res <- NULL
	## special treatment for factors. Their indices should be written as strings.
	recoType <- c("no", "disc","inter", "interRecoBeSw02")
	#OBJRECO = paste('"',toString((1:length(recoType))[recoType==objReco]),'"', sep="")
	#STRATRECO = paste('"',toString((1:length(recoType))[recoType==stratReco]),'"', sep="")
	## TODO: use which for the following selection:
	OBJRECO = toString((1:length(recoType))[recoType==objReco])
	STRATRECO = toString((1:length(recoType))[recoType==stratReco])
	res <- list(Y=realBest[[length(realBest)]], # last value
			NPARENTS=mue,
			NU=nu,
			KAPPA=sel,
			InitSgm=sigmaInit,
			TauMult=tauMult,
			XRecombination=objReco,
			SRecombination=stratReco,
			Rho=rho,
			NSIGMA=nSigma,
			SIGMARESTART=sigmaRestart,
			NoisyFitness=bestFitness[[length(bestFitness)]],
			AllTimeBest=allTimeBest[[length(allTimeBest)]],
			Generation=bestInd$generation,
			Percentage=bestInd$generation/gen*100,
			Mutation=mutation,
			TAU0=tau0,
			TAU=tau,
			SIGMAINIT=sigmaInit,
			PRESCANMULT=preScanMult,
			OBJRECO = OBJRECO,
			STRATRECO = STRATRECO,
			Function=fName,
			MaxIter=maxIter,
			Dim=dimension,
			SEED=seed,
			CONFIG=conf  
	)
	res <-data.frame(res)	
	return(res)
}

###################################################################################################
#' ES function call for SPOT
#'
#' SPOT uses this function for some demos to call an Evolution Strategy. The ES can use 
#' different fitness functions. The results are written to the res file.
#' This function is needed as an interface, to ensure the right information
#' are passed from SPOT to the target algorithm (e.g. the ES) and vice versa.
#'
#' @param io.apdFileName name of the apd file
#' @param io.desFileName name of the des file
#' @param io.resFileName name of the res file
#'
#' @references  \code{\link{SPOT}} \code{\link{spotAlgEs}} \code{\link{spotAlgEsF}} 
###################################################################################################
spotAlgStartEs <- function(io.apdFileName, io.desFileName, io.resFileName){
	dimension<-NULL
	mutation<-NULL
	rho<-NULL
	maxGen<-NULL
	maxIter<-NULL
	noise<-NULL
	fName<-NULL
	lowerLimit<-NULL
	upperLimit<-NULL
	verbosity<-NULL
	plotResult<-NULL
	resFileName<-NULL
	#print( io.apdFileName)
	## read default problem design
	source( io.apdFileName,local=TRUE)
	## read doe/dace etc settings:
	#print( io.desFileName)
	writeLines(paste("Loading design file data from::",  io.desFileName), con=stderr());
	des <- read.table( io.desFileName
			, sep=" "
			, header = TRUE
	);
	#print(summary(des));
	##  NPARENTS NU TAU NSIGMA REPEATS SEED
	config<-nrow(des);
	print(config);
	attach(des)
	if (!exists("CONFIG"))
		stop("Design file is missing the required column CONFIG!")
	for (k in 1:config){		
		for (i in 1:des$REPEATS[k]){
			##
			if (exists("NPARENTS")){
				mue <- des$NPARENTS[k]
			}
			if (exists("NU")){
				nu <- des$NU[k]
			}
			if (exists("NSIGMA")){
				nSigma <- des$NSIGMA[k]
			}
			if (exists("TAU0")){
				tau0 <- des$TAU0[k]
			}
			if (exists("TAU")){
				tau <- des$TAU[k]
			}
			if (exists("KAPPA")){
				kappa <- des$KAPPA[k]
			}
			if (exists("SIGMARESTART")){
				sigmaRestart <- des$SIGMARESTART[k]
			}
			if (exists("SIGMAINIT")){
				sigmaInit <- des$SIGMAINIT[k]
			}
			prescanmult <- 1
			if (exists("PRESCANMULT")){
				prescanmult <- des$PRESCANMULT[k]
			}				
			## special treatment for factors
			recoType <- c("no", "disc","inter","interRecoBeSw02")
			if (exists("OBJRECO")){
				objReco <- recoType[des$OBJRECO[k]]
			}
			if (exists("STRATRECO")){
				stratReco <- recoType[des$STRATRECO[k]]
			}	  
			conf <- k
			if (exists("CONFIG")){
				conf <- des$CONFIG[k]
			}
			spotStep<-NA
			if (exists("STEP")){
				spotStep <- des$STEP[k]
			}				
			seed <- des$SEED[k]+i-1				
			print(c("Config:",k ," Repeat:",i))				
			res <- spotAlgEs(mue=mue,
					nu=nu,
					dimension=dimension,
					mutation=mutation,
					sigmaInit=sigmaInit,
					nSigma=nSigma,
					tau0=tau0,
					tau=tau,
					rho=rho,
					stratReco=stratReco,
					objReco=objReco,
					sel=kappa,
					maxGen=maxGen,
					maxIter=maxIter,
					seed=seed,
					noise=noise,
					fName=fName,
					lowerLimit=lowerLimit,
					upperLimit=upperLimit,
					verbosity = verbosity,
					plotResult=plotResult,
					sigmaRestart=sigmaRestart,
					preScanMult=prescanmult,
					resFileName=resFileName,
					conf=des$CONFIG[k])     
			print(res$Y)			
			if (exists("STEP")){
				res=c(res,STEP=spotStep)
			} 
			res <-data.frame(res)
			colNames = TRUE
			if (file.exists(io.resFileName)){
				colNames = FALSE
			}			
			## quote = false is required for JAVA
			write.table(res
					, file =  io.resFileName
					, row.names = FALSE
					, col.names = colNames
					, sep = " "              
					, append = !colNames
					, quote = FALSE
			);			
		}
	}	
	detach(des)
}

###################################################################################################
#' ES Quick Test
#'
#' This is a quick test function for the ES.
#'
#' @param mue value of mue (number of parents) to be used with the ES default is 2
#' @param nu value of nu to be used with the ES default is 2
#' @param tau value of tau (learning parameter) to be used with the ES default is 1
#'
#' @references  \code{\link{spotAlgEs}} \code{\link{spotAlgStartEs}} 
###################################################################################################
spotAlgEsQuickTest <- function(mue=2, nu=2,tau=1){
	## Problem design:
	dimension=2;
	mutation="selfA";
	sigmaInit=0.1;
	nSigma=2;
	tau0=1.5;
	#tau=1;
	rho="bi";
	stratReco=1;
	objReco=1;
	kappa=-1; # 10
	maxGen=Inf;
	maxIter=200;
	seed=123;
	noise=0.0;
	#fName="rastrigin";
	fName="branin";
	lowerLimit=-1;
	upperLimit=1;
	#mue = 2;
	#nu = 2;
	plotResult=TRUE;
	logPlotResult=FALSE;
	verbosity=1;
	sigmaRestart = 0.2;
	resFileName="esQuickTest.res";
	prescanmult=1;
	##
	esRun <- spotAlgEs(mue=mue,
			nu=nu,
			dimension=dimension,
			mutation=mutation,
			sigmaInit=sigmaInit,
			nSigma=nSigma,
			tau0=tau0,
			tau=tau,
			rho=rho,
			stratReco=stratReco,
			objReco=objReco,
			sel=kappa,
			maxGen=maxGen,
			maxIter=maxIter,
			seed=seed,
			noise=noise,
			fName=fName,
			lowerLimit=lowerLimit,
			upperLimit=upperLimit,
			verbosity = verbosity,
			plotResult=plotResult,
			logPlotResult=logPlotResult,
			sigmaRestart = sigmaRestart,
			preScanMult=prescanmult,
			resFileName=resFileName)
}







