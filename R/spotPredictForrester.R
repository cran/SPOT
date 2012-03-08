###################################################################################
#' Meta Model Interface: Forresters Kriging
#' 
#' Kriging predictor based on matlab code by Forrester et.al. 
#' Can be used both for single and multi objective SPOT.
#'
#' @param rawB unmerged data
#' @param mergedB merged data
#' @param largeDesign new design points which should be predicted
#' @param spotConfig global list of all options, needed to provide data for calling functions. This also contains a list, with settings for forrester:\cr
#'	\code{spotConfig$seq.forrester$Option} What to predict, can be: "Pred"(default), "NegLogExpImp", "NegProbImp", "RMSE"\cr
#'	\code{spotConfig$seq.forrester$loval} Lower boundary for theta, default is 1e-3\cr
#'	\code{spotConfig$seq.forrester$upval} Upper boundary for theta, default is 100\cr
#'	\code{spotConfig$seq.forrester$algtheta} algorithm used to find theta, default is "cmaes"\cr
#'	\code{spotConfig$seq.forrester$budgetalgtheta} Budget for the above mentioend algorithm, default is 1000
#' @param externalFit if an existing model fit is supplied, the model will not be build based on 
#'				data, but only evaluated with the model fit (on the largeDesign data). To build the model, 
#'				this parameter has to be NULL. If it is not NULL the paramters mergedB and rawB will not be 
#'				used at all in the function.
#'
#' @return returns the list \code{spotConfig} with two new entries:\cr
#' 	spotConfig$seq.modelFit fit of the model used with predict() \cr
#'	spotConfig$seq.largeDesignY the y values of the large design, evaluated with the fit
#' @export
#' @seealso \code{\link{spotDetermineTheta}} \code{\link{spotNormalizeMatrix2}} \code{\link{spotNormalizeMatrix}} 
#' \code{\link{spotReverseNormalizeMatrix}} \code{\link{spotRegPredictor}} \code{\link{spotRegLikelihood}}
#' @references Forrester, Alexander I.J.; Sobester, Andras; Keane, Andy J. (2008). Engineering Design via Surrogate Modelling - A Practical Guide. John Wiley & Sons.
###################################################################################
spotPredictForrester <- function(rawB,mergedB,largeDesign,spotConfig,externalFit=NULL){	
	spotWriteLines(spotConfig$io.verbosity,1,"spotPredictForrester started");
	spotInstAndLoadPackages("MASS")	
	xNames <- row.names(spotConfig$alg.roi); 
	if(is.null(externalFit)){
		if(is.null(spotConfig$seq.forrester)){
			spotConfig$seq.forrester=list()
			spotConfig$seq.forrester$Option="Pred"#="Pred" oder "NegLogExpImp" "NegProbImp" "RMSE"
			spotConfig$seq.forrester$loval=1e-3 
			spotConfig$seq.forrester$upval=100
			spotConfig$seq.forrester$algtheta="cmaes" #else optim-L-BFGS-B
			spotConfig$seq.forrester$budgetalgtheta=1000		
		}
		if(is.null(spotConfig$seq.forrester$Option))spotConfig$seq.forrester$Option="Pred";
		if(is.null(spotConfig$seq.forrester$loval))spotConfig$seq.forrester$loval=1e-3;
		if(is.null(spotConfig$seq.forrester$upval))spotConfig$seq.forrester$upval=100;
		if(is.null(spotConfig$seq.forrester$algtheta))spotConfig$seq.forrester$algtheta="cmaes";
		if(is.null(spotConfig$seq.forrester$budgetalgtheta))spotConfig$seq.forrester$budgetalgtheta=1000;
		yNames <- setdiff(names(rawB),xNames)
		x <- as.matrix(rawB[xNames])
		if(length(yNames)==1){
			y <- as.matrix(rawB[[yNames]])
			fit <- spotDetermineTheta(x, y, spotConfig$seq.forrester);		
		}
		else{#Distinction for multi criteria spot 
			y <- rawB[yNames]
			fit=list()
			for (i in 1:length(yNames)){
				fit[[i]]<-spotDetermineTheta(x, as.matrix(y[,i]), spotConfig$seq.forrester);	
			}			
		}
	}else{
		fit<-externalFit
		if(ncol(largeDesign)<nrow(spotConfig$alg.roi)){
			largeDesign<-t(data.frame(largeDesign))			
		}
		else{
			largeDesign<-data.frame(largeDesign)
		}
		colnames(largeDesign)<-xNames
	}
	if(length(spotConfig$alg.resultColumn)>1){
		res=list()
		largeDesign = spotNormalizeMatrix2(t(as.matrix(largeDesign)),0,1,fit[[1]]$normalizexmin,fit[[1]]$normalizexmax);
		for (i in 1:length(spotConfig$alg.resultColumn)){			
			res[[i]]= spotRegPredictor(t(largeDesign),fit[[i]])
		}
	}else{
		largeDesign = spotNormalizeMatrix2(t(as.matrix(largeDesign)),0,1,fit$normalizexmin,fit$normalizexmax);
		res= spotRegPredictor(t(largeDesign),fit)
	}			
	spotConfig$seq.largeDesignY=as.data.frame(res)
	spotConfig$seq.modelFit<-fit;
	return(spotConfig)
}

###################################################################################
#' Determine Theta for Forresters Kriging
#' 
#' @param X design matrix
#' @param y vector of observations at X
#' @param fit list of settings and parameters of a forrester model
#'
#' @return a fit, with the found parameters for the model
#' @export
#' @seealso \code{\link{spotPredictForrester}} \code{\link{spotNormalizeMatrix2}} \code{\link{spotNormalizeMatrix}}
#' \code{\link{spotReverseNormalizeMatrix}} \code{\link{spotNormalizeMatrix}}
#' \code{\link{spotRegPredictor}} \code{\link{spotRegLikelihood}}
#' @keywords internal
###################################################################################
spotDetermineTheta <- function(X, y, fit=list()){
	k = ncol(X);
	fit$X = t(X); 
	fit$y = y;
	# normalize input data
	ymin = 0;
	ymax = 1;
	fit$normalizeymin=ymin;
	fit$normalizeymax=ymax;
	res = spotNormalizeMatrix(fit$X, ymin, ymax);
	fit$X =t(res$y);
	fit$normalizexmin=res$xmin;
	fit$normalizexmax=res$xmax;
	lambda.loval = -6;
	lambda.upval = 0;
	LowerTheta = rep(1,k)*log10(fit$loval);
	LowerTheta = c(LowerTheta,lambda.loval);
	UpperTheta = rep(1,k)*log10(fit$upval);
	UpperTheta = c(UpperTheta, lambda.upval);	
	#Wrapper for optimizing theta  based on spotRegLikelihood:
	fitFun <- function (x, fX, fy){ #todo vectorize, at least for cma_es with active vectorize?
		result=spotRegLikelihood(x,fX,fy)
		return(as.numeric(result$NegLnLike))
	}
	a=log10(fit$loval);
	b=log10(fit$upval);
	# start point for theta:
	x0 =  a+(b-a)*runif(k);
	# start value for lambda:
	x1 = lambda.loval + (lambda.upval - lambda.loval)*runif(1);
	x0 = c(x0 , x1);
	if(fit$algtheta=="cmaes"){
		opts=list()
		opts$sigma0=0.3*abs(b-a);
		spotInstAndLoadPackages("cmaes")	
		opts$maxit=ceiling(fit$budgetalgtheta / (4 + floor(3 * log(length(x0)))))
		res = cma_es(par=x0,fn=fitFun,fX=fit$X,fy=fit$y,lower=LowerTheta,upper=UpperTheta,control=opts); 
		if(is.null(res$par))res$par=x0;
		Params = res$par	
	}else if (fit$algtheta=="optim-L-BFGS-B"){
		res <- optim(x0, fitFun,NULL,fX=fit$X,fy=fit$y,method="L-BFGS-B",lower=LowerTheta,upper=UpperTheta,
			control=list(maxit=fit$budgetalgtheta)); 
		Params <- res$par;	
	}
	else{stop("Invalid choice of spotConfig$seq.forrester$algtheta. Please check the help of spotPredictForrester for possible values.")}
	fit$dmodeltheta=10^Params[1:k];
	fit$dmodellambda=10^Params[length(Params)];
	# extract model parameters:
	fit$Theta = Params[1:k];
	fit$Lambda = Params[length(Params)];
	res=spotRegLikelihood(c(fit$Theta, fit$Lambda),fit$X,fit$y);
	fit$ssq=res$ssq
	fit$mu=res$mu
	fit$Psi=res$Psi
	return(fit)
}

###################################################################################
#' Normalize design
#'
#' Normalize design with given maximum and minimum in input space
#' 
#' @param x design matrix in input space
#' @param ymin minimum vector of normalized space
#' @param ymax maximum vector of normalized space
#' @param xmin minimum vector of input space
#' @param xmax maximum vector of input space
#'
#' @return normalized design matrix
#' @seealso \code{\link{spotPredictForrester}} \code{\link{spotDetermineTheta}}
#'  \code{\link{spotNormalizeMatrix}} 
#' \code{\link{spotReverseNormalizeMatrix}}
#' @export
#' @keywords internal
###################################################################################
spotNormalizeMatrix2 <- function (x,ymin,ymax,xmin,xmax){ 
	rangex = xmax-xmin;
	rangey = ymax-ymin;
	s = dim(x)[2];
	y = rangey * (x-matrix(rep(xmin,s),ncol=s))/matrix(rep(rangex,s),ncol=s) + ymin;
	return(y)
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
#' @seealso \code{\link{spotPredictForrester}} \code{\link{spotDetermineTheta}}
#'  \code{\link{spotNormalizeMatrix2}}
#' \code{\link{spotReverseNormalizeMatrix}} 
#' @export
#' @keywords internal
###################################################################################
spotNormalizeMatrix <- function(x,ymin, ymax){
	# Return the maximum from each row:
	xmax = apply(x,1,max);
	# Return the minimum from each row:
	xmin = apply(x,1,min);
	s = dim(x)[2];
	rangex = xmax-xmin;
	rangey = ymax-ymin;
	xmin[rangex==0] = xmin[rangex==0]-0.5
	xmax[rangex==0] = xmax[rangex==0]+0.5
	rangex[rangex==0] = 1;
	y = rangey * (x-matrix(rep(xmin,s),ncol=s))/matrix(rep(rangex,s),ncol=s) + ymin;
	return(list(y=y,xmin=xmin,xmax=xmax))
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
#' @seealso \code{\link{spotPredictForrester}} \code{\link{spotDetermineTheta}}
#'  \code{\link{spotNormalizeMatrix2}} \code{\link{spotNormalizeMatrix}} 
#' @export
#' @keywords internal
###################################################################################
spotReverseNormalizeMatrix <- function(y,xmin,xmax,ymin,ymax){
	s = dim(y)[2];
	rangex = xmax-xmin;
	rangey = ymax-ymin;
	x = matrix(rep(rangex,s),ncol=s) * (y-ymin)*(1/rangey) + matrix(rep(xmin,s),ncol=s);
	return(x);
}

###################################################################################
#' Calculate negative log-likelihood
#' 
#' Used to determine theta/lambda values for the Kriging model in \code{\link{spotDetermineTheta}}.
#'
#' @param x vector, containing log(theta) and lambda
#' @param AX design matrix (sample locations)
#' @param Ay vector of observations
#'
#' @return list with elements\cr
#' \code{NegLnLike}  concentrated log-likelihood *-1 for minimising \cr
#' \code{Psi} correlation matrix\cr
#FIXMZ: \code{U} U matrix of the LU decomposition of correlation matrix \cr
#' \code{mu} \cr
#' \code{ssq}
#' @seealso \code{\link{spotPredictForrester}} \code{\link{spotDetermineTheta}}
#' @export
#' @keywords internal
###################################################################################
spotRegLikelihood <- function(x,AX,Ay){
	theta=10^x[1:(length(x)-1)];
	p=2;
	lambda=10^x[length(x)];
	n=dim(AX)[1]
	one=rep(1,n);
	Psi=matrix(0,n,n); # initialise to zeros
	for (i in 1:n){		
		for (j in (i+1):n){ #TODO can this be vectorised?
			if(i<n)Psi[i,j]=exp(-sum(theta*abs(AX[i,]-AX[j,])^p));# % build top right corner
		}
	}
	# add upper and lower halves and diagonal of ones plus lambda
	Psi=Psi+t(Psi)+diag(1,n)*(lambda+1); 
	# concentrated log-likelihood calculation
	LnDetPsi=as.numeric(determinant(Psi)$modulus) 
	
	###Folgende Alternativen gibt es für LnDetPsi:	
	# log(abs(det(Psi)))  # ---> klappt nicht, det(Psi)=0 wenn werte zu klein werden. numerisches problem mit exp()
	# 2*sum(log(abs(diag(chol(Psi))))) # original code, chol kann crashen
	# sum(log(abs(diag(qr(Psi)$qr)))) # noch langsamer, aber vielleicht stabiler als chol
	# as.numeric(determinant(Psi)$modulus) #ungefähr so schnell wie det(), kein numerisches problem
	
	Psinv= try(solve(Psi), TRUE)
	if(class(Psinv) == "try-error"){		
		Psinv=ginv(Psi)
	}
	mu=sum(Psinv%*%Ay)/sum(Psinv%*%one)# note: matrix%*%onevector is signif. faster than rowSums(matrix)
	yonemu=Ay-one*mu
	SigmaSqr=(t(yonemu)%*%Psinv%*%yonemu)/n;
	NegLnLike=0.5*(n*log(SigmaSqr) + LnDetPsi);
	return(list(NegLnLike=NegLnLike,Psi=Psi,mu=mu,ssq=SigmaSqr))
}


###################################################################################
#' Predict with Forrester Model
#' 
#' Predict values with forrester
#'
#' @param x design matrix to be predicted (largeDesign in SPOT)
#' @param ModelInfo fit of the Kriging model (settings and parameters)
#'
#' @return metric, dependent on the setting of the string \code{ModelInfo$Option}\cr
#' "Pred"(default) yields the predicted value, "NegLogExpImp" expected improvement, "NegProbImp", "RMSE"
#' 
#' @seealso \code{\link{spotPredictForrester}} \code{\link{spotDetermineTheta}}
#' @export
#' @keywords internal
###################################################################################
spotRegPredictor <- function(x,ModelInfo){
	AX=ModelInfo$X;
	Ay=ModelInfo$y;
	theta=10^ModelInfo$Theta;	
	p=2; 
	Psinv= try(solve(ModelInfo$Psi), TRUE)
	if(class(Psinv) == "try-error"){
		Psinv=ginv(ModelInfo$Psi)
	}
	n=dim(AX)[1];
	one=rep(1,n);
	mu=ModelInfo$mu
	SigmaSqr=ModelInfo$ssq
	#mu=sum(Psinv%*%Ay)/sum(Psinv%*%one)
	#######mu=(one%*%solve(U,solve(t(U),Ay)))/(one%*%solve(U,solve(t(U),one)));#mat mult ? #t()?   #TODO: or solve() instead of bslh
	#SigmaSqr=(t((Ay-one*mu))%*%solve(U,solve(t(U),(Ay-one*mu))))/n;
	psi=matrix(1,nrow(x),n);
	#browser()
	####fn1<-function(xx){exp(-sum(theta*abs(AX[i,]-xx)^p))}#applied
	for (i in 1:n){
		####psi[,i]=apply(x,1,fn1); #applied
		psi[,i]=exp(-colSums(theta*t((matrix(AX[i,],ncol=ncol(x),nrow=nrow(x),byrow=TRUE)-x)^p)))#vectorised
	}
	###fn2<-function(ppsi){mu+ppsi%*%solve(U,solve(t(U),(Ay-one*mu)))}#applied
	###f=apply(psi,1,fn2)#applied
	#f=as.numeric(psi%*%solve(U,solve(t(U),(Ay-one*mu))))+mu #vectorised
	f=as.numeric(psi%*%(Psinv%*%(Ay-one*mu)))+mu #vectorised
	##########################################################################
	if (ModelInfo$Option!="Pred"){
		lambda=10^ModelInfo$Lambda;
		SSqr= SigmaSqr*(1+lambda-diag(psi%*%(Psinv%*%t(psi)))) #vectorised
		#TODO "diag(psi%*%...)" is excessive, since diag wastes alot of values computed by %*%
		s=sqrt(abs(SSqr));
		if (ModelInfo$Option!="RMSE"){
			if(is.null(ModelInfo$ConstraintLimit)){
				yBest=min(Ay);
			}else{
				yBest=ModelInfo$ConstraintLimit;
			}
			if(ModelInfo$Option=="NegProbImp"){
				ProbImp=pnorm((yBest-f)/s);#todo check if correct
			}else{
				EITermOne=(yBest-f)*pnorm((yBest-f)/s);#todo check if correct
				EITermTwo=s*(1/sqrt(2*pi))*exp(-(1/2)*((yBest-f)^2/SSqr));
				ExpImp=log10(EITermOne+EITermTwo+(.Machine$double.eps));
			}
		}
	}
	if(ModelInfo$Option=="Pred"){
		metric=f;
	}else if(ModelInfo$Option=="RMSE"){
		metric=s;
	}else if(ModelInfo$Option=="NegLogExpImp"){
		metric=-ExpImp;
	}else if(ModelInfo$Option=="NegProbImp"){
		metric=-ProbImp;
	}
	return(metric)
}

