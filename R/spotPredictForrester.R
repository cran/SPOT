###################################################################################
#' Meta Model Interface: Forrester's Kriging
#' 
#' Interface to the Kriging model based on Matlab code by Forrester et al. 2008.
#' Can be used both for single and multi objective SPOT.
#'
#' @param rawB unmerged data
#' @param mergedB merged data
#' @param design new design points which should be predicted
#' @param spotConfig global list of all options, needed to provide data for calling functions. The model specific settings in this list are\cr
#'	\code{spotConfig$seq.forr.loval} lower boundary for theta, default is \code{1e-3}\cr
#'	\code{spotConfig$seq.forr.upval} upper boundary for theta, default is \code{100}\cr
#'	\code{spotConfig$seq.forr.opt.p} boolean that specifies whether the exponents (\code{p}) should be optimized. Else they will be set to two. Default value is \code{FALSE}. Default is highly recommended as the implementation of this feature is not yet well tested and might be faulty.\cr
#'	\code{spotConfig$seq.forr.algtheta} algorithm used to find theta, default is \code{"NLOPT_LN_NELDERMEAD"} which is a bounded simplex method from the package nloptr. Else, any from the list of possible \code{method} values in \code{\link{spotOptimizationInterface}} can be chosen.\cr
#'	\code{spotConfig$seq.forr.budgetalgtheta} budget for the above mentioned algorithm, default is \code{100}. The value will be multiplied with the length of the model parameter vector to be optimized.
#'	\code{spotConfig$seq.forr.reinterpolate} boolean that specifies whether re-interpolation should be used during the prediction process. Default value is \code{FALSE}. Setting this to \code{TRUE} is recommended, when an error estimate of nearly zero is desired at sample locations, regardless of chosen regularization constant (nugget). Please note that prediction with interpolation will take longer than without.\cr
#'	\code{spotConfig$seq.forr.savetheta} boolean that specifies whether the exponents (\code{p}) should be optimized. Else they will be set to two. Default value is \code{FALSE}. Default is recommended since this feature not yet well tested, and might lead to a preference of local optima.\cr
#' @param fit if an existing model fit is supplied, the model will not be build based on 
#'				data, but only evaluated with the model fit (on the design data). To build the model, 
#'				this parameter has to be NULL. If it is not NULL the parameters mergedB and rawB will not be 
#'				used at all in the function.
#'
#' @return returns the list \code{spotConfig} with two new entries:\cr
#' 	spotConfig$seq.modelFit fit of the model used with the predictor functions \cr
#'	spotConfig$seq.largeDesignY the y values of the design, evaluated with the fit
#' @export
#' @seealso \code{\link{forrBuilder}} \code{\link{forrRegPredictor}} \code{\link{forrReintPredictor}} 
#' @references Forrester, Alexander I.J.; Sobester, Andras; Keane, Andy J. (2008). Engineering Design via Surrogate Modelling - A Practical Guide. John Wiley & Sons.
###################################################################################
spotPredictForrester <- function(rawB,mergedB,design,spotConfig,fit=NULL){	
	design <- spotInitializePredictor(design,"data.frame",spotConfig$alg.roi,NULL,"spotPredictForrester",spotConfig$io.verbosity)	#MZ: removed MASS. not necessary?
	if(is.null(spotConfig$seq.forr.reinterpolate))spotConfig$seq.forr.reinterpolate=FALSE
	########################################################
	# BUILD
	########################################################	
	if(is.null(fit)){

		########################################################
		# DEFAULTS FOR MODEL BUILDING
		########################################################	
		if(is.null(spotConfig$seq.forr.savetheta))spotConfig$seq.forr.savetheta=FALSE
		if(is.null(spotConfig$seq.forr.loval))spotConfig$seq.forr.loval=1e-3
		if(is.null(spotConfig$seq.forr.upval))spotConfig$seq.forr.upval=100
		if(is.null(spotConfig$seq.forr.opt.p))spotConfig$seq.forr.opt.p=FALSE; #TODO, not working properly in case of TRUE
		if(is.null(spotConfig$seq.forr.algtheta))spotConfig$seq.forr.algtheta="NLOPT_LN_NELDERMEAD"
		if(is.null(spotConfig$seq.forr.budgetalgtheta))spotConfig$seq.forr.budgetalgtheta=100
		if(is.null(spotConfig$seq.forr.lambda.loval))spotConfig$seq.forr.lambda.loval=-6
		if(is.null(spotConfig$seq.forr.lambda.upval))spotConfig$seq.forr.lambda.upval=0
		
		xNames <- row.names(spotConfig$alg.roi)
		yNames <- setdiff(names(rawB),xNames)
		x <- as.matrix(rawB[xNames])
		if(length(yNames)==1){
			y <- as.matrix(rawB[[yNames]])
			if(!is.null(spotConfig$seq.modelFit) && spotConfig$seq.forr.savetheta){ #if available use last theta/lambda for startpoint of this run
				spotConfig$seq.forr.starttheta=c(spotConfig$seq.modelFit$Theta,spotConfig$seq.modelFit$Lambda)
			}
			fit <- forrBuilder(x, y, spotConfig$seq.forr.loval, spotConfig$seq.forr.upval, 
							spotConfig$seq.forr.algtheta, spotConfig$seq.forr.budgetalgtheta,
								spotConfig$alg.roi$lower, spotConfig$alg.roi$upper, 
								spotConfig$seq.forr.opt.p,
								spotConfig$seq.forr.lambda.loval, spotConfig$seq.forr.lambda.upval)
		}
		else{#Distinction for multi criteria spot 
			y <- rawB[yNames]
			fit=list()
			for (i in 1:length(yNames)){
				if(!is.null(spotConfig$seq.modelFit) && spotConfig$seq.forr.savetheta){ #if available use last theta/lambda for startpoint of this run
					spotConfig$seq.forr.starttheta=c(spotConfig$seq.modelFit[[i]]$Theta,spotConfig$seq.modelFit[[i]]$Lambda)
				}
				fit[[i]]<-forrBuilder(x, as.matrix(y[,i]),spotConfig$seq.forr.loval, spotConfig$seq.forr.upval, 
							spotConfig$seq.forr.algtheta, spotConfig$seq.forr.budgetalgtheta,
								spotConfig$alg.roi$lower, spotConfig$alg.roi$upper, 
								spotConfig$seq.forr.opt.p,
								spotConfig$seq.forr.lambda.loval, spotConfig$seq.forr.lambda.upval)
				}			
		}
	}
	########################################################
	# PREDICT
	########################################################
	if(!is.null(design)){ 		
		pred.all=spotConfig$seq.model.variance
		nmodel <- length(spotConfig$alg.resultColumn)
		if(nmodel>1){ #do multi criteria prediction
			resy=matrix(0,nrow(design),nmodel)
			resvar=matrix(NA,nrow(design),nmodel)
			y=list()
			#design = spotNormalizeMatrix2(t(as.matrix(design)),0,1,fit[[1]]$normalizexmin,fit[[1]]$normalizexmax);
			for (i in 1:length(fit)){ #predict			
				if(spotConfig$seq.forr.reinterpolate){
					res= forrReintPredictor(design,fit[[i]],pred.all)
				}else{
					res= forrRegPredictor(design,fit[[i]],pred.all)
				}
				resy[,i]= res$f
				if(pred.all)resvar[,i]= res$s
				y[[i]]= fit[[i]]$y
			}
			if(is.function(spotConfig$seq.infill)){# do EI 
				resy= spotConfig$seq.infill(resy,resvar,y,spotConfig$mco.refPoint)
			}
		}else{ #do single criteria prediction
			#design = spotNormalizeMatrix2(t(as.matrix(design)),0,1,fit$normalizexmin,fit$normalizexmax);
			if(spotConfig$seq.forr.reinterpolate){
				res= forrReintPredictor(design,fit,pred.all)
			}else{
				res= forrRegPredictor(design,fit,pred.all)
			}
			resy=res$f
			resvar=matrix(NA,nrow(design),1)
			if(pred.all)resvar=res$s
			if(is.function(spotConfig$seq.infill)){ # do EI			
				resy= spotConfig$seq.infill(resy,resvar,min(fit$y))
			}
		}			
	}else{
		resy <- NULL
		resvar <- NULL
	}	
	########################################################
	# OUTPUT
	########################################################	
	spotWriteLines(spotConfig$io.verbosity,3,"spotPredictForrester finished")
	spotConfig$seq.largeDesignY=as.data.frame(resy)	
	spotConfig$seq.largeDesignVar=as.data.frame(resvar)	
	spotConfig$seq.modelFit<-fit;
	spotConfig
}

###################################################################################
#' Print Function Forrester Kriging
#'
#' Print information about a Forrester Kriging fit, as produced by \code{\link{forrBuilder}}.
#'
#' @rdname print
#' @method print forr
#' @S3method print forr
#' @param x	fit returned by \code{\link{forrBuilder}}.
#' @param ... additional parameters	
#' @export
#' @keywords internal
###################################################################################
print.forr <- function(x,...){
	cat("------------------------\n")
	cat("Forrester Kriging model.\n")
	cat("------------------------\n")
	cat("Estimated activity parameters (theta) sorted \n")
	cat("from most to least important variable \n")
	cat(paste("x",order(x$dmodeltheta,decreasing=TRUE),sep="",collaps=" "))
	cat("\n")	
	cat(sort(x$dmodeltheta,decreasing=TRUE))	
	cat("\n \n")
	cat("exponent(s) p:\n")
	if(x$opt.p)
		cat(x$P)
	else
		cat(2)
	cat("\n \n")
	cat("Estimated regularization constant (or nugget) lambda:\n")	
	cat(x$dmodellambda)
	cat("\n \n")
	cat("Number of Likelihood evaluations during MLE:\n")	
	cat(x$nevals)
	cat("\n")	
	cat("------------------------\n")
}

###################################################################################
#' Build Forrester Kriging
#'
#' This function builds a Kriging model based on code by Forrester et al..
#' By default exponents (p) are fixed at a value of two, and a nugget (or regularization constant) is used.
#' 
#' @param X design matrix (sample locations)
#' @param y vector of observations at X
#' @param loval lower boundary for theta, default is \code{1e-3}
#' @param upval upper boundary for theta, default is \code{100}
#' @param algtheta algorithm used to find theta, default is \code{"NLOPT_LN_NELDERMEAD"} which is a bounded simplex method from the package nloptr. Else, any from the list of possible \code{method} values in \code{\link{spotOptimizationInterface}} can be chosen.
#' @param budgetalgtheta budget for the above mentioned algorithm, default is \code{100}. The value will be multiplied with the length of the model parameter vector to be optimized.
#' @param lb lower boundary of the design space. Will be extracted from the matrix \code{X} if not given.
#' @param ub upper boundary of the design space. Will be extracted from the matrix \code{X} if not given.
#' @param opt.p boolean that specifies whether the exponents (\code{p}) should be optimized. Else they will be set to two. Default value is \code{FALSE}. Default is highly recommended as the implementation of this feature is not yet well tested and might be faulty.
#' @param lambda.loval lower boundary for regularization constant (nugget), default is \code{-6}. (lambda=10^lambda, e.g. 10^-6)
#' @param lambda.upval upper boundary for regularization constant (nugget), default is \code{0}. (lambda=10^lambda, e.g. 1)
#'
#' @return a fit (list), with the options and found parameters for the model which has to be passed to the predictor function:\cr
#' \code{X} sample locations (scaled to values between 0 and 1)\cr
#' \code{y} observations at sample locations (see parameters)\cr
#' \code{loval} lower boundary for theta (see parameters)\cr
#' \code{upval} upper boundary for theta (see parameters)\cr
#' \code{algtheta} algorithm to find theta (see parameters)\cr
#' \code{budgetalgtheta} budget for the above mentioned algorithm (see parameters)\cr
#' \code{opt.p} boolean that specifies whether the exponents (\code{p}) were optimized (see parameters)\cr
#' \code{normalizeymin} minimum in normalized space\cr
#' \code{normalizeymax} maximum in normalized space\cr
#' \code{normalizexmin} minimum in input space\cr
#' \code{normalizexmax} maximum in input space\cr
#' \code{dmodeltheta} vector of activity parameters\cr
#' \code{Theta} log_10 vector of activity parameters (i.e. \code{log10(dmodeltheta)})\cr
#' \code{dmodellambda} regularization constant (nugget) \cr
#' \code{Lambda} log_10 of regularization constant (nugget) (i.e. \code{log10(dmodellambda)})\cr
#' \code{yonemu} \code{Ay-ones*mu} \cr
#' \code{ssq} sigma square\cr
#' \code{mu} mean mu\cr
#' \code{Psi} matrix large Psi\cr
#' \code{Psinv} inverse of Psi\cr
#' \code{nevals} number of Likelihood evaluations during MLE
#'
#' @export
#' @seealso \code{\link{spotPredictForrester}} \code{\link{forrRegPredictor}} \code{\link{forrReintPredictor}}
#' @references Forrester, Alexander I.J.; Sobester, Andras; Keane, Andy J. (2008). Engineering Design via Surrogate Modelling - A Practical Guide. John Wiley & Sons.
#'
#' @examples
#' ## Create design points
#' x = cbind(runif(20)*15-5,runif(20)*15)
#' ## Compute observations at design points (for Branin function)
#' y = as.matrix(apply(x,1,spotBraninFunction))
#' ## Create model with default settings
#' fit = forrBuilder(x,y)
#' ## Print model parameters
#' print(fit)
#'
###################################################################################
forrBuilder <- function(X, y, loval=1e-3, upval=100, algtheta= "NLOPT_LN_NELDERMEAD", budgetalgtheta=100, lb=NULL, ub=NULL, opt.p= FALSE, lambda.loval = -6, lambda.upval = 0){
	fit = list(loval=loval, upval=upval, opt.p=opt.p, algtheta=algtheta, budgetalgtheta=budgetalgtheta)
	k = ncol(X)
	fit$X = X
	fit$y = y
	# normalize input data
	ymin = 0
	ymax = 1
	fit$normalizeymin=ymin
	fit$normalizeymax=ymax
	res = spotNormalizeMatrix(fit$X, ymin, ymax, lb, ub)
	fit$X =res$y
	fit$normalizexmin=res$xmin
	fit$normalizexmax=res$xmax
	LowerTheta = rep(1,k)*log10(fit$loval)

	UpperTheta = rep(1,k)*log10(fit$upval)

	#Wrapper for optimizing theta  based on forrRegLikelihood:
	fitFun <- function (x, fX, fy,opt.p){ #todo vectorize, at least for cma_es with active vectorize?
		as.numeric(forrRegLikelihood(x,fX,fy,opt.p)$NegLnLike)
		#print(c(as.numeric(result$NegLnLike),x))
		#return(as.numeric(result$NegLnLike)) #without return should be faster
	}
	a=log10(fit$loval)
	b=log10(fit$upval)
	n=nrow(fit$X) #number of observations
	
	x0 = fit$starttheta	
	# start point for theta:
	#x1 =  a+(b-a)*runif(k)
	x1 =  rep(n/(100*k),k)
	# start value for lambda:
	x2 = lambda.loval + (lambda.upval - lambda.loval)*runif(1)
	
	
	
	####instead of the old solution above, concatenate matrices. is faster in likelihood function.
	#f2<-function(){
	#A=matrix(0,k,n*n) #preallocate array
	#for(i in 1:n){ #calculate array for reglikelihood function
	#	A[,(1+(i-1)*n):(n*i)]=(fit$X[i,]-t(fit$X))
	#}	
	#}
	#f1<-function(){
	#A=NULL
	#for(i in 1:k){
	#	A=rbind(A, as.numeric(as.matrix(dist(fit$X[,i]))))
	#}
	#}		
	#f0<-function(){
	A=matrix(0,k,n*n)
	for(i in 1:k){
		A[i,]=as.numeric(as.matrix(dist(fit$X[,i]))) #MZ: speedup fix, using dist function: 100%
	}
	#}		
	#f3<-function(){
	#fn<-function(x){as.numeric(as.matrix(dist(x)))}
	#AB=t(apply(fit$X,2,fn))
	#}	
	#require(microbenchmark)
	#print(microbenchmark(f1()))
	#browser()
	if(fit$opt.p){ # optimize p
		LowerTheta = c(LowerTheta, rep(1,k)*0.01)
		UpperTheta = c(UpperTheta, rep(1,k)*2)		
		x3 = rep(1,k)* 1.9 #start values for p
		if(is.null(x0)){x0 = c(x1,x3,x2)}
	}else{ # p  is fixed to 2 and the array A is completely precalculated
		A=A^2
		if(is.null(x0)){x0 = c(x1,x2)}
	}			
	#append regression constant lambda (nugget)
	LowerTheta = c(LowerTheta,lambda.loval)
	UpperTheta = c(UpperTheta, lambda.upval)
	
	opts=list(fevals=fit$budgetalgtheta*length(x0), reltol=1e-6, restarts=TRUE)	
	res <- spotOptimizationInterface(par=x0,fn=fitFun,gr=NULL,lower=LowerTheta,upper=UpperTheta,method=fit$algtheta,
						control=opts,fX=A,fy=fit$y,opt.p=fit$opt.p)	
	if(is.null(res$par))res$par=x0;
	Params = res$par
	nevals = as.numeric(res$counts[[1]])

	fit$dmodeltheta=10^Params[1:k]
	if(fit$opt.p){	
		fit$P=Params[(k+1):(2*k)]
	}
	fit$dmodellambda=10^Params[length(Params)]
	# extract model parameters:
	fit$Theta = Params[1:k]
	fit$Lambda = Params[length(Params)];
	res=forrRegLikelihood(c(fit$Theta,fit$P, fit$Lambda),A,fit$y,fit$opt.p);
#*#browser()
	fit$yonemu=res$yonemu
	fit$ssq=res$ssq
	fit$mu=res$mu
	fit$Psi=res$Psi
	fit$Psinv=res$Psinv
	fit$nevals=nevals
	fit$like=res$NegLnLike
	class(fit)<- "forr"
	fit
}

###################################################################################
#' Normalize design
#'
#' Normalize design with given maximum and minimum in input space
#' 
#' @param x design matrix in input space (n rows for each point, k columns for each parameter)
#' @param ymin minimum vector of normalized space
#' @param ymax maximum vector of normalized space
#' @param xmin minimum vector of input space
#' @param xmax maximum vector of input space
#'
#' @return normalized design matrix
#' @seealso \code{\link{spotPredictForrester}} \code{\link{forrBuilder}}
#'  \code{\link{spotNormalizeMatrix}} 
#' \code{\link{spotReverseNormalizeMatrix}}
#' @export
#' @keywords internal
###################################################################################
spotNormalizeMatrix2 <- function (x,ymin,ymax,xmin,xmax){ 
	rangex = xmax-xmin
	rangey = ymax-ymin
	s = dim(x)[1]
	y = rangey * (x-matrix(rep(xmin,s),nrow=s,byrow=TRUE))/matrix(rep(rangex,s),nrow=s,byrow=TRUE) + ymin
}

###################################################################################
#' Normalize design
#' 
#' Normalize design by using minimum and maximum of the design values for input space
#'
#' @param x design matrix in input space
#' @param ymin minimum vector of normalized space
#' @param ymax maximum vector of normalized space
#'
#' @return normalized design matrix
#' @seealso \code{\link{spotPredictForrester}} \code{\link{forrBuilder}}
#'  \code{\link{spotNormalizeMatrix2}}
#' \code{\link{spotReverseNormalizeMatrix}} 
#' @export
#' @keywords internal
###################################################################################
spotNormalizeMatrix <- function(x,ymin, ymax, xmin=NULL, xmax=NULL){
	# Return the maximum from each row:
	if(is.null(xmax))
		xmax = apply(x,2,max)
	# Return the minimum from each row:
	if(is.null(xmin))
		xmin = apply(x,2,min)
	s = dim(x)[1]
	rangex = xmax-xmin
	rangey = ymax-ymin
	xmin[rangex==0] = xmin[rangex==0]-0.5
	xmax[rangex==0] = xmax[rangex==0]+0.5
	rangex[rangex==0] = 1
	y = rangey * (x-matrix(rep(xmin,s),nrow=s,byrow=TRUE))/matrix(rep(rangex,s),nrow=s,byrow=TRUE) + ymin
	list(y=y,xmin=xmin,xmax=xmax)
}

###################################################################################
#' Reverse Normalize
#' 
#' Reverse normalization of a design matrix
#'
#' @param y design matrix in norm space
#' @param ymin minimum vector of normalized space
#' @param ymax maximum vector of normalized space
#' @param xmin minimum vector of output space
#' @param xmax maximum vector of output space
#'
#' @return denormalized design matrix
#' @seealso \code{\link{spotPredictForrester}} \code{\link{forrBuilder}}
#'  \code{\link{spotNormalizeMatrix2}} \code{\link{spotNormalizeMatrix}} 
#' @export
#' @keywords internal
###################################################################################
spotReverseNormalizeMatrix <- function(y,xmin,xmax,ymin,ymax){
	s = dim(y)[2]
	rangex = xmax-xmin
	rangey = ymax-ymin
	x = matrix(rep(rangex,s),ncol=s) * (y-ymin)*(1/rangey) + matrix(rep(xmin,s),ncol=s)
}

###################################################################################
#' Calculate negative log-likelihood
#' 
#' Used to determine theta/lambda values for the Kriging model in \code{\link{forrBuilder}}.
#'
#' @param x vector, containing log10(theta) and lambda
#' @param AX 3 dimensional array, constructed by forrBuilder from the sample locations
#' @param Ay vector of observations at sample locations
#'
#' @return list with elements\cr
#' \code{NegLnLike}  concentrated log-likelihood *-1 for minimising \cr
#' \code{Psi} correlation matrix\cr
#' \code{Psinv} inverse of correlation matrix (to save computation time in forrRegPredictor)\cr
#FIXMZ: \code{U} U matrix of the LU decomposition of correlation matrix \cr
#' \code{mu} \cr
#' \code{ssq}
#' @seealso \code{\link{spotPredictForrester}} \code{\link{forrBuilder}}
#' @export
#' @keywords internal
###################################################################################
forrRegLikelihood <- function(x,AX,Ay,opt.p=FALSE){
	if(opt.p){
		nx<-nrow(AX)
		theta=10^x[1:nx];		
			if(any(is.na(abs(AX)^(10^x[(nx+1):(2*nx)]))))stop("NA values in theta")
		AX=abs(AX)^(x[(nx+1):(2*nx)])
	}else{
		theta=10^x[1:(length(x)-1)];
	}
	lambda=10^x[length(x)];	
	if( any(c(theta,lambda)==0) ||  any(c(theta,lambda)==Inf)){ #for instance caused by bound violation
		return(list(NegLnLike=1e4,Psi=NA,Psinv=NA,mu=NA,ssq=NA))
	}
	n=dim(Ay)[1]
	one=rep(1,n);
	#benchmarking original gegen vektorisierte schnellere varianten:
	#a<-function(){Psi=matrix(0,n,n);for (i in 1:(n-1)){for (j in (i+1):n){Psi[i,j]=exp(-sum(theta*abs(AX[i,]-AX[j,])^p));}};Psi=Psi+t(Psi)+diag(1,n)*(lambda+1);}
	#browser()
	#b<-function(){Psi=matrix(0,n,n);for (i in 1:n){ Psi[,i]=exp(-colSums(theta*t((matrix(AX[i,],ncol=ncol(AX),nrow=nrow(AX),byrow=TRUE)-AX)^p)));};Psi=Psi+diag(1,n)*lambda;}
	#cc<-function(){Psi=matrix(0,n,n);for (i in 1:n){ Psi[,i]=exp(-colSums(theta*((-t(AX)+AX[i,])^p)));};Psi=Psi+diag(1,n)*lambda;}
	#dd<-function(){Psi=matrix(0,n,n);for (i in 1:(n-1)){ Psi[(i+1):n,i]=exp(-colSums(theta*((-t(AX[(i+1):n,])+AX[i,])^p)));};Psi=Psi+t(Psi)+diag(1,n)*(lambda+1);}
	#ee<-function(){Psi=matrix(0,n,n);for (i in 1:(n-1)){ Psi[(i+1):n,i]=exp(-colSums(theta*t((matrix(AX[i,],ncol=ncol(AX),nrow=nrow(AX)-i,byrow=TRUE)-AX[(i+1):n,])^p)));};Psi=Psi+t(Psi)+diag(1,n)*(lambda+1);}
	#e<-function(){Psi1=matrix(0,n,n);Psi1=apply(AX,1,function(xx){exp(-colSums(theta*t((matrix(xx,ncol=ncol(AX),nrow=nrow(AX),byrow=TRUE)-AX)^p)))});}
	#f<-function(){Psi=matrix(0,n,n);for (i in 1:n){ Psi[,i]=exp(-rowSums(((matrix(AX[i,],ncol=ncol(AX),nrow=nrow(AX),byrow=TRUE)-AX)^p)%*%diag(theta)));};Psi=Psi+diag(1,n)*lambda;}
	#ff<-function(){Psi=matrix(0,n,n);for (i in 1:n){ Psi[,i]=-colSums(theta*((-t(AX)+AX[i,])^p));};Psi=exp(Psi)+diag(1,n)*lambda;}
	#res1<-b()
	#res2<-e()
	#require(microbenchmark)
	#print(microbenchmark(a()))
	#print(microbenchmark(b()))
	#print(microbenchmark(cc()))
	#print(microbenchmark(dd()))
	#print(microbenchmark(ee()))
	#print(microbenchmark(e()))
	#print(microbenchmark(ff()))
	#ff is best	

	####original
	#	Psi=matrix(0,n,n);
	#	for (i in 1:n){
	#		for (j in (i+1):n){ 
	#			if(i<n)Psi[i,j]=exp(-sum(theta*abs(AX[i,]-AX[j,])^p));
	#	}}
	#	Psi=Psi+t(Psi)+diag(1,n)*(lambda+1); # add upper and lower halves and diagonal of ones plus lambda
	
	####schneller (ausser bei sehr wenigen observations, dann ists aber ohnehin recht flott)
	#Psi=matrix(0,n,n)
	#for (i in 1:n){ 
	#	Psi[,i]=-colSums(theta*((AX[i,]-t(AX))^p))
	#}
	#Psi=exp(Psi)+diag(1,n)*lambda
	###noch schneller, weil matrix AX[i,]-t(AX) schon vorher in forrBuilder berechnet wird_
	#Psi=matrix(0,n,n)
	#for (i in 1:n){ 
	#	Psi[,i]=colSums(theta*AX[,,i])
	#}
	#Psi=exp(-Psi)+diag(1,n)*lambda
	#browser()
	
	
	#A=NULL
	#for (i in 1:n){ 
	#	A=cbind(A,AX[,,i])
	#}

	Psi=matrix(colSums(theta*AX),n,n)
	Psi=exp(-Psi)+diag(1,n)*lambda
	#browser()
	####benchmarking original gegen vektorisierte schnellere varianten:
	#a<-function(){Psi=matrix(0,n,n);for(i in 1:n){Psi[,i]=colSums(theta*AX[,(1+(i-1)*n):(n*i)]);};Psi}
	####fnt<-function(x){-theta*x}
	####b<-function(){Psi=matrix(0,n,n);Psi=apply(AX,3,fnt);}	
	####b<-function(){Psi=matrix(0,n,n);for(i in 1:n){Psi[,i]=colSums(AX[,,i]);};apply(Psi,1,fnt)}
	#b<-function(){Psi=matrix(colSums(theta*AX),n,n);}
	#require(microbenchmark)
	#print(microbenchmark(a(),times=100))
	#print(microbenchmark(b(),times=100))
	

	
	
	# concentrated log-likelihood calculation
	LnDetPsi=as.numeric(determinant(Psi)$modulus) 
	
	###Folgende Alternativen gibt es für LnDetPsi:	
	# log(abs(det(Psi)))  # ---> klappt nicht, det(Psi)=0 wenn werte zu klein werden. numerisches problem mit exp()
	# 2*sum(log(abs(diag(chol(Psi))))) # original code, chol kann crashen
	# sum(log(abs(diag(qr(Psi)$qr)))) # noch langsamer, aber vielleicht stabiler als chol
	# as.numeric(determinant(Psi)$modulus) #ungefähr so schnell wie det(), kein numerisches problem
	
	Psinv= try(solve(Psi), TRUE)
	if(class(Psinv) == "try-error"){		
		return(list(NegLnLike=1e4,Psi=NA,Psinv=NA,mu=NA,ssq=NA))
	}
	mu=sum(Psinv%*%Ay)/sum(Psinv%*%one)# note: matrix%*%onevector is signif. faster than rowSums(matrix)
	yonemu=Ay-mu  #%yonemu=Ay-one*mu
	SigmaSqr=(t(yonemu)%*%Psinv%*%yonemu)/n;
	NegLnLike=0.5*(n*log(SigmaSqr) + LnDetPsi);
	list(NegLnLike=NegLnLike,Psi=Psi,Psinv=Psinv,mu=mu,yonemu=yonemu,ssq=SigmaSqr)
}


###################################################################################
#' Predict Forrester Model
#' 
#' Predict samples on a Forrester Kriging model.
#'
#' @param x design matrix to be predicted
#' @param ModelInfo fit of the Kriging model (settings and parameters)
#' @param pred.all if TRUE return all (RMSE and prediction, in a dataframe), else return only prediction
#'
#' @return Returned value is dependent on the setting of \code{pred.all}\cr
#' TRUE: data.frame with columns f (function values) and s (RMSE)\cr
#' FALSE: vector of function values only
#'
#' @examples
#' ## Create design points
#' x = cbind(runif(20)*15-5,runif(20)*15)
#' ## Compute observations at design points (for Branin function)
#' y = as.matrix(apply(x,1,spotBraninFunction))
#' ## Create model
#' fit = forrBuilder(x,y)
#' ## Create candidate design points
#' xx = cbind(runif(20)*15-5,runif(20)*15)
#' ## Predict candidates
#' y1 = forrRegPredictor(xx,fit)$f
#' ## Plot model (in comments, due to time consumption)
#' #fn <- function(x){forrRegPredictor(as.matrix(x),fit)$f}
#' #spotSurf3d(fn,c(-5,0),c(10,15))
#' ## Plot real function
#' #spotSurf3d(function(x){apply(x,1,spotBraninFunction)},c(-5,0),c(10,15))
#'
#' @seealso \code{\link{forrBuilder}} \code{\link{forrReintPredictor}}
#' @export
###################################################################################
forrRegPredictor <- function(x,ModelInfo,pred.all=FALSE){
	#normalize input x
	x <- spotNormalizeMatrix2(as.matrix(x),0,1,ModelInfo$normalizexmin,ModelInfo$normalizexmax)
	AX=ModelInfo$X
	Ay=ModelInfo$y
	theta=ModelInfo$dmodeltheta
	Psinv=ModelInfo$Psinv #fixed: does not need to be computed, is already done in likelihood function
	n=dim(AX)[1]
	one=rep(1,n)
	mu=ModelInfo$mu
	yonemu=ModelInfo$yonemu	
	SigmaSqr=ModelInfo$ssq
	psi=matrix(1,nrow(x),n);
	if(ModelInfo$opt.p){
		p=ModelInfo$P
		for (i in 1:n){
			psi[,i]=exp(-colSums(theta*(abs(AX[i,]-t(x))^p)))
		}	
	}else{
		p=2
		for (i in 1:n){
			psi[,i]=exp(-colSums(theta*((AX[i,]-t(x))^p)))
		}
	}	
	f=as.numeric(psi%*%(Psinv%*%(yonemu)))+mu #vectorised
	##########################################################################
	#if (ModelInfo$Option!="Pred"){
	if (pred.all){
		lambda=ModelInfo$dmodellambda;
		SSqr= SigmaSqr*(1+lambda-diag(psi%*%(Psinv%*%t(psi)))) #vectorised
		#TODO "diag(psi%*%...)" is excessive, since diag wastes alot of values computed by %*%
		s=sqrt(abs(SSqr));
	}
	result=if(!pred.all){list(f=f)}else{data.frame(f=f,s=s)}
}

###################################################################################
#' Predict Forrester Model (Re-interpolating)
#' 
#' Kriging predictor with re-interpolation to avoid stalling the optimization process which employs this model as a surrogate.
#' This is supposed to be used with deterministic experiments, which do need a non-interpolating model that avoids predicting non-zero error at sample locations.
#' This can be useful when the model is deterministic (i.e. repeated evaluations of one parameter vector do not yield different values) but does have a "noisy" structure (e.g. due to computational inaccuracies, systematical error).
#'
#' Please note that this re-interpolation implementation will not necessarily yield values of exactly zero at the sample locations used for model building. Slight deviations can occur.
#'
#' @param x design matrix to be predicted
#' @param ModelInfo fit of the Kriging model (settings and parameters)
#' @param pred.all if TRUE return all (RMSE and prediction, in a dataframe), else return only prediction
#'
#' @return Returned value is dependent on the setting of \code{pred.all}\cr
#' TRUE: data.frame with columns f (function values) and s (RMSE)\cr
#' FALSE: vector of function values only
#'
#' @examples
#' ## Create design points
#' x = cbind(runif(20)*15-5,runif(20)*15)
#' ## Compute observations at design points (for Branin function)
#' y = as.matrix(apply(x,1,spotBraninFunction))
#' ## Create model
#' fit = forrBuilder(x,y)
#' ## first estimate error with regressive predictor
#' sreg = forrRegPredictor(x,fit,TRUE)$s
#' ## now estimate error with re-interpolating predictor
#' sreint = forrReintPredictor(x,fit,TRUE)$s
#' print(sreg)
#' print(sreint)
#' ## sreint should be close to zero, significantly smaller than sreg
#'
#' @seealso \code{\link{forrBuilder}} \code{\link{forrCoBuilder}} \code{\link{forrRegPredictor}}
#' @export
###################################################################################
forrReintPredictor <- function(x,ModelInfo,pred.all=FALSE){
	#normalize input x
	x <- spotNormalizeMatrix2(as.matrix(x),0,1,ModelInfo$normalizexmin,ModelInfo$normalizexmax)
	AX=ModelInfo$X
	Ay=ModelInfo$y
	theta=ModelInfo$dmodeltheta
	Psi=ModelInfo$Psi
	Psinv=ModelInfo$Psinv 
	lambda=ModelInfo$dmodellambda;
	n=dim(AX)[1]
	one=rep(1,n)
	mu=ModelInfo$mu	
	yonemu=ModelInfo$yonemu	
	#
	PsiB=Psi-diag(1,n)*lambda+diag(1,n)*.Machine$double.eps
	SigmaSqr=(t(yonemu)%*%Psinv%*%PsiB%*%Psinv%*%yonemu)/n;
	#	
	psi=matrix(1,nrow(x),n);
	if(ModelInfo$opt.p){
		p=ModelInfo$P
		for (i in 1:n){
			psi[,i]=exp(-colSums(theta*(abs(AX[i,]-t(x))^p)))
		}	
	}else{
		p=2
		for (i in 1:n){
			psi[,i]=exp(-colSums(theta*((AX[i,]-t(x))^p)))
		}
	}	
	f=as.numeric(psi%*%(Psinv%*%(yonemu)))+mu #vectorised
	##########################################################################
	#if (ModelInfo$Option!="Pred"){
	if(pred.all){
		#
		Psinv= try(solve(PsiB), TRUE)
		if(class(Psinv) == "try-error"){
			Psinv=ginv(Psi)
		}	
		#
		SSqr= SigmaSqr*(1-diag(psi%*%(Psinv%*%t(psi)))) #vectorised
		#TODO "diag(psi%*%...)" is excessive, since diag wastes alot of values computed by %*%
		s=sqrt(abs(SSqr));
	}
	result=if(!pred.all){list(f=f)}else{data.frame(f=f,s=s)}
}

