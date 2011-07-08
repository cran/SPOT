###################################################################################
#' Spot Predictor Linear Model Optimized
#' 
#' A linear prediction model with optimization on the response surface
#' and automated adaptation of the region of interest is used to predict 
#' improved design points
#' 
#' @param rawB matrix of raw x and y values
#' @param mergedB matrix of merged x and y values, does not have replicate entries
#' @param lhd design points to be evaluated by the meta model  
#' @param spotConfig the list of all parameters is given 
#'
#' @return returns the list \code{spotConfig} with two new entries:\cr
#' 	spotConfig$seq.modelFit fit of the Krig model used with predict() \cr
#'	spotConfig$seq.largeDesignY the y values of the large design, evaluated with the fit
#'
#' @references  \code{\link{SPOT}}
###################################################################################
spotPredictLmOptim <- function(rawB,mergedB,lhd,spotConfig) {
	spotWriteLines(spotConfig$io.verbosity,2,"  Entering spotPredictLmOptim");	
	spotInstAndLoadPackages("rsm")
	
	mergedData <- spotPrepareData(spotConfig)	
	pNames <- row.names(spotConfig$alg.aroi);
	## number of parameters
	nParam <- length(pNames)
	##
	# fmla update required based on new aroi:
	if(spotConfig$spot.fileMode){
		spotConfig$alg.aroi <- read.table(spotConfig$io.aroiFileName			
				, header = TRUE
				, as.is=TRUE
				, row.names = 1 #Parameter als Zeilennamen
		);
	}
	spotPrint(spotConfig$io.verbosity,1,spotConfig$alg.aroi)
	fmla <- NULL				
	for (i in 1:nParam) {
		a <- spotConfig$alg.aroi$low[i]
		spotPrint(spotConfig$io.verbosity,1,a)
		b <- spotConfig$alg.aroi$high[i]
		spotPrint(spotConfig$io.verbosity,1,b)
		v1 <- mean(c(a,b))
		v2 <- (b-a)/2	
		fmla <- c(fmla,
				as.formula(paste(paste("x",i,sep=""),
								"~ (", pNames[i], " - ", v1, ")/", v2
						)
				)
		)		
	}
	df2 <- coded.data(mergedB, formulas = fmla)
	## check feasibility of data points in df2:
	df2x <- df2[,-1]	
	df2 <- df2[apply(df2x, 1, function(x) all(x >= -1 & x <= 1)), ]
	##############################################################################
	## We build a regression model:	
	## Check if there are sufficiently many data for linear, quadratic etc. models
	nExp <- nrow(df2)
	## Linear model without interactions:
	nRequired1 <- 1 + nParam
	## Linear model with interactions:
	nRequired2 <- 1 + nParam * (nParam + 1) / 2 
	## Full quadratic model:
	nRequired3 <- 1 + nParam + nParam * (nParam + 1) / 2
	## Create the most powerful model possible given the current number of parameters...
	makeNNames <- function(n) { Map(function(i) paste("x", i, sep = ""), 1:n) }
	makeNParameters <- function(n) { Reduce(function(n1, n2) paste(n1, n2, sep=","), makeNNames(n)) }
	paramString <- makeNParameters(nParam) # the string "x1,x2,...,xnParams"
	makeNParametersSum <- function(n) { Reduce(function(n1, n2) paste(n1, n2, sep="+"), makeNNames(n)) }
	#rsmFormula<-NULL
	#rsmDf<-NULL
	if (nExp >= nRequired1 && nExp < nRequired2) {
		spotWriteLines(spotConfig$io.verbosity,2,"spotPredictLmOptim: First order (FO) effects estimated by rsm.");
		## Because rsm demands it, rsmDf and rsmFormula must be visible in the global environment:
		rsmDf <- df2
		rsmFormula <- as.formula(sprintf("y ~ FO(%s)", paramString))
		dfc.rsm1 <- spotRsm(formula = rsmFormula, data = rsmDf)
	}
	else if (nExp >= nRequired2 && nExp < nRequired3) {
		spotWriteLines(spotConfig$io.verbosity,2,"spotPredictLmOptim: First order (FO) with two-way interactions (TWI) effects estimated by rsm.");
		## Because rsm demands it, rsmDf and rsmFormula must be visible in the global environment:
		rsmDf <- df2
		rsmFormula <- as.formula(sprintf("y ~ FO(%s) + TWI(%s)", paramString, paramString))
		dfc.rsm1 <- spotRsm(formula = rsmFormula, data = rsmDf)
	}
	else if (nExp >= nRequired3) {
		spotWriteLines(spotConfig$io.verbosity,2,"spotPredictLmOptim: Second order (SO) effects estimated by rsm.");
		## Because rsm demands it, rsmDf and rsmFormula must be visible in the global environment:    
		rsmDf <- df2		
		rsmFormula <- as.formula(sprintf("y ~ FO(%s) + TWI(%s) + PQ(%s)", paramString, paramString, paramString))
		dfc.rsm1 <- spotRsm(formula = rsmFormula, data = rsmDf)
		spotPrint(spotConfig$io.verbosity,1,summary(dfc.rsm1))		
		### pdf and x11() plots
		nCol <- ceiling(sqrt(sum(1:(nParam-1))))
		par(mfrow=c(nCol,nCol) )		
		attach(dfc.rsm1)
		dev.new()
		contour(dfc.rsm1, as.formula(paste("~",makeNParametersSum(nParam))))
		dev.off()
		fName= paste (tail(mergedData$STEP,1), spotConfig$io.pdfFileName, sep="") 
		pdf(file=fName)
		contour(dfc.rsm1, as.formula(paste("~",makeNParametersSum(nParam))))
		dev.off()
		detach(dfc.rsm1)
		
	}
	###################################################################################
#	## In addition, we build a random forest:	
#	spotSafelyAddSource(spotConfig$init.design.path,"spotPredictRandomForest",spotConfig$io.verbosity)	
#	rfBest <- spotPredictRandomForest(rawB,mergedB,lhd,spotConfig)[1,]	
#	
#	## In addition, we build a tree:	
	spotSafelyAddSource(spotConfig$init.design.path,"spotPredictTree",spotConfig$io.verbosity)	
	treeBest <- spotPredictTree(rawB,mergedB,lhd,spotConfig)$newDesign
	
	#######################################################################################
	### Start optimization (experimental):  
	## a) even (=> if branch) determine steepest descent
	## b) odd (=> else branch) determine new center point 
	## even: we take results from the last step only
	## parameter names	
	## determine  points among the path of the steepest descent:
	if ( (mergedData$step.last %% 2) == 0){
		if ( (spotConfig$seq.useGradient == TRUE)){
			if (spotConfig$seq.useCanonicalPath == TRUE){ # start at saddle point in both directions (canonical path analysis)
				steepestDesc <-  as.data.frame(canonical.path(dfc.rsm1, descent=TRUE, dist = seq(-0.2,0.2, by = 0.1))[,2:eval(nParam+1)])
			}
			else{ # start at origin in one direction
				steepestDesc <-  as.data.frame(steepest(dfc.rsm1, descent=TRUE, dist = seq(0.1,0.5, by = 0.1))[,2:eval(nParam+1)])
			}
			## ensure feasibility			
			steepestDesc<- steepestDesc[apply(steepestDesc, 1, function(x) all(x > -1 & x < 1)), ]
			## use tree if steepestDesc is empty
			if (nrow(steepestDesc)==0){
				steepestDesc <- spotPredictTree(rawB,mergedB,lhd,spotConfig)[1,]				
			}
			steepestDesc <- code2val(steepestDesc, codings = codings(df2))			
			###steepestDesc <- steepestDesc[1:spotConfig$seq.design.new.size,];  
			spotConfig$newDesign<-steepestDesc;
			colnames(steepestDesc)<- as.character(makeNNames(nParam))
			spotConfig$newDesignPredictedY<-predict(dfc.rsm1,steepestDesc);
			return(spotConfig);
		}
		else{ # do not use gradient information
			largeDataCoded <- val2code(lhd,codings = codings(dfc.rsm1))
			yPred <- predict(dfc.rsm1, newdata=largeDataCoded)
			index <- order(yPred)
			lhd <- cbind(lhd, index)
			newPoints <- lhd[index <= spotConfig$seq.design.new.size, ]
			newPoints <- newPoints[pNames]
			spotConfig$newDesign<-newPoints;
			colnames(newPoints)<- as.character(makeNNames(nParam))
			spotConfig$newDesignPredictedY<-predict(dfc.rsm1,newPoints);
			return(spotConfig);			
		}
	}
	else{
		## now we have the evaluated path of the steepeest descent
		## take the best point as the new center point		
		df2.min <- df2$y==min(df2$y)
		best <- df2[df2.min,]
		## take the best, ensure that the best is used only once
		xB <- unique(best[,-1])
		spotPrint(spotConfig$io.verbosity,1,xB)
		nVals <- nrow(xB)		
		spotPrint(spotConfig$io.verbosity,1,nVals)
		# if different x vals result in the same y val, take one randomly
		xB <- xB[sample(1:nVals)[1],]
		spotPrint(spotConfig$io.verbosity,1,xB)
		##xB <- xB[1,]
		ds <- min( rep(1,nParam) - abs(xB))	
		if (ds > 0.1){
			spotPrint(spotConfig$io.verbosity,1,"ds:")
			spotPrint(spotConfig$io.verbosity,1,ds)
			low <- xB - ds
			high <- xB + ds      
			A <- t(rbind(low, high))		
			A <- t(code2val(data.frame(t(A)), attr(df2,"codings")))
			A <- data.frame(A, nrow =nParam)
			A <- cbind(pNames, A)  
			colnames(A) <- c("name", "low", "high")
			## Now we add type information
			if(spotConfig$spot.fileMode){ #CHECK IF CORRECT TODOMZ
				aroi<- spotReadAroi(spotConfig)	
			}else{
				aroi <- spotConfig$alg.aroi;
			}
			A <- cbind(A, type=aroi$type)		
			if(spotConfig$spot.fileMode){ 
				spotWriteAroi(spotConfig, A)	
			}#else{
				spotConfig$alg.aroi<-A;
			#}

			spotWriteLines(spotConfig$io.verbosity,2,"AROI modified. Execution with continued in the adapted ROI.");
			## generate a new design 
			spotConfig$seq.useAdaptiveRoi <- TRUE
			M <- spotCreateDesignFrF2(spotConfig)
		}
		else{
			spotConfig$seq.useAdaptiveRoi <- TRUE
			M <- spotCreateDesignLhs(spotConfig, noDesPoints=spotConfig$seq.design.new.size)
		}		
		## combine best tree point and new ccd points:		
		M <- rbind(treeBest,M)
		spotWriteLines(spotConfig$io.verbosity,2,"spotPredictLmOptim finished.")
		spotConfig$newDesign<-M;
		colnames(M)<- as.character(makeNNames(nParam))
		spotConfig$newDesignPredictedY<-predict(dfc.rsm1,M);
		return(spotConfig);		
	}
}  
