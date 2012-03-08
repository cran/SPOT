###################################################################################
#' Meta Model Interface: Linear Model
#'
#' A linear prediction model, which will use higher order interactions if data is sufficient.
#' Can be used both for single and multi objective SPOT.
#' 
#' This function implements a linear model for prediction. Depending on the numbers of variables 
#' either no interactions,  interaction  between the variables may be used or a full quadratic model is
#' provided.  
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
#' @return returns the list \code{spotConfig} with two new entries:\cr
#' 	spotConfig$seq.modelFit fit of the Krig model used with predict() \cr
#'	spotConfig$seq.largeDesignY the y values of the large design, evaluated with the fit
#'  
#' @seealso \code{\link{SPOT}}
#' @export
###################################################################################
spotPredictLm <- function(rawB,mergedB,lhd,spotConfig,externalFit=NULL){	
	spotWriteLines(spotConfig$io.verbosity,2,"  Entering spotPredictLm");	
	spotInstAndLoadPackages("rsm")	
	#rawB <- spotGetRawDataMatrixB(spotConfig);
	mergedData <- spotPrepareData(spotConfig)
	mergedB <- spotGetMergedDataMatrixB(mergedData, spotConfig);
	pNames <- row.names(spotConfig$alg.roi);
	nParam <- length(pNames)	
	yNames <- setdiff(names(rawB),pNames)
	X <- rawB[pNames] #MZ: Bugfix for 1 dimensional optimization
	if(length(spotConfig$alg.resultColumn)>1){ #for multi objective modeling
		fit<-list()
		res=list()
		for(ii in 1:length(spotConfig$alg.resultColumn)){
			Y<-rawB[spotConfig$alg.resultColumn[ii]]					
			df1 <- data.frame(cbind(Y,X))			
			fmla <- NULL				
			for (i in 1:nParam){
				a <- min(X[,i])
				b <- max(X[,i])
				v1 <- mean(c(a,b))
				v2 <- (b-a)/2	
				fmla <- c(fmla,
						as.formula(paste(paste("x",i,sep=""),
										"~ (", pNames[i], " - ", v1, ")/", v2
										)))		
			}	
			df2 <- coded.data(df1, formulas = fmla)
			makeNNames <- function(n) { Map(function(i) paste("x", i, sep = ""), 1:n) }
			if(is.null(externalFit)){
				###
				## Check if there are sufficiently many data for linear, quadratic etc. models
				nExp <- nrow(X)
				## Linear model without interactions:
				nRequired1 <- 1 + nParam
				## Linear model with interactions:
				nRequired2 <- 1 + nParam * (nParam + 1) / 2 
				## Full quadratic model:
				nRequired3 <- 1 + nParam + nParam * (nParam + 1) / 2
				## Create the most powerful model possible given the current number of parameters...
				makeNParameters <- function(n) { Reduce(function(n1, n2) paste(n1, n2, sep=","), makeNNames(n)) }
				paramString <- makeNParameters(nParam) # the string "x1,x2,...,xnParams"
				if (nExp >= nRequired1 && nExp < nRequired2) {
					spotWriteLines(spotConfig$io.verbosity,2,"spotPredictLm: First order (FO) effects estimated by rsm.");
					## Because rsm demands it, rsmDf and rsmFormula must be visible in the global environment:
					rsmDf <- df2
					rsmFormula <- as.formula(sprintf(paste(colnames(Y),"~ FO(%s)")
												, paramString))
					dfc.rsm1 <- spotRsm(formula = rsmFormula, data = rsmDf)
				}else if (nExp >= nRequired2 && nExp < nRequired3) {
					spotWriteLines(spotConfig$io.verbosity,2,"spotPredictLm: First order (FO) with two-way interactions (TWI) effects estimated by rsm.");
					## Because rsm demands it, rsmDf and rsmFormula must be visible in the global environment:
					rsmDf <- df2
					rsmFormula <- as.formula(sprintf(paste(colnames(Y),"~ FO(%s) + TWI(%s)")
												, paramString, paramString))
					dfc.rsm1 <- spotRsm(formula = rsmFormula, data = rsmDf)
				}else if (nExp >= nRequired3) {
					spotWriteLines(spotConfig$io.verbosity,2,"spotPredictLm: Second order (SO) effects estimated by rsm.");
					## Because rsm demands it, rsmDf and rsmFormula must be visible in the global environment:
					rsmDf <- df2
					## This
					## rsmFormula <- as.formula(sprintf("Y ~ SO(%s)", paramString))
					## generates warnings, so we use the long form:
					rsmFormula <- as.formula(sprintf(paste(colnames(Y),"~ FO(%s) + TWI(%s) + PQ(%s)")
														, paramString, paramString, paramString))
					dfc.rsm1 <- spotRsm(formula = rsmFormula, data = rsmDf)
				}
				lhd1 <- data.frame(apply(lhd, 2, spotHlpF.norm))
				colnames(lhd1)<- as.character(makeNNames(nParam))
				result<-predict(dfc.rsm1, lhd1)
			}else{
				dfc.rsm1 <-externalFit[[ii]]
				if(ncol(lhd)<nrow(spotConfig$alg.roi)){ #ugly, but necessary because R is strange about converting one dimensional vectors to dataframes (confusing rows/columns)
					lhd1 <- t(data.frame(apply(lhd, 2, spotHlpF.norm)))
				}
				else{
					lhd1 <- data.frame(apply(lhd, 2, spotHlpF.norm))
				}
				colnames(lhd1)<- as.character(makeNNames(nParam))
				result <- predict(dfc.rsm1,data.frame(lhd1))	
			}
			res[[ii]]<-result
			fit[[ii]]<-dfc.rsm1
		}
	}else{#for single objective modeling
		Y<-rawB[yNames]
		df1 <- data.frame(cbind(Y,X))			
		fmla <- NULL				
		for (i in 1:nParam){
			a <- min(X[,i])
			b <- max(X[,i])
			v1 <- mean(c(a,b))
			v2 <- (b-a)/2	
			fmla <- c(fmla,as.formula(paste(paste("x",i,sep=""),
						"~ (", pNames[i], " - ", v1, ")/", v2)))		
		}	
		df2 <- coded.data(df1, formulas = fmla)
		makeNNames <- function(n) { Map(function(i) paste("x", i, sep = ""), 1:n) }
		if(is.null(externalFit)){
			###
			## Check if there are sufficiently many data for linear, quadratic etc. models
			nExp <- nrow(X)
			## Linear model without interactions:
			nRequired1 <- 1 + nParam
			## Linear model with interactions:
			nRequired2 <- 1 + nParam * (nParam + 1) / 2 
			## Full quadratic model:
			nRequired3 <- 1 + nParam + nParam * (nParam + 1) / 2
			## Create the most powerful model possible given the current number of parameters...
			makeNParameters <- function(n) { Reduce(function(n1, n2) paste(n1, n2, sep=","), makeNNames(n)) }
			paramString <- makeNParameters(nParam) # the string "x1,x2,...,xnParams"
			if (nExp >= nRequired1 && nExp < nRequired2) {
				spotWriteLines(spotConfig$io.verbosity,2,"spotPredictLm: First order (FO) effects estimated by rsm.");
				## Because rsm demands it, rsmDf and rsmFormula must be visible in the global environment:
				rsmDf <- df2
				rsmFormula <- as.formula(sprintf(paste(colnames(Y),"~ FO(%s)")
												, paramString))
				dfc.rsm1 <- spotRsm(formula = rsmFormula, data = rsmDf)
			}else if (nExp >= nRequired2 && nExp < nRequired3) {
				spotWriteLines(spotConfig$io.verbosity,2,"spotPredictLm: First order (FO) with two-way interactions (TWI) effects estimated by rsm.");
				## Because rsm demands it, rsmDf and rsmFormula must be visible in the global environment:
				rsmDf <- df2
				rsmFormula <- as.formula(sprintf(paste(colnames(Y),"~ FO(%s) + TWI(%s)")
												, paramString, paramString))
				dfc.rsm1 <- spotRsm(formula = rsmFormula, data = rsmDf)
			}else if (nExp >= nRequired3) {
				spotWriteLines(spotConfig$io.verbosity,2,"spotPredictLm: Second order (SO) effects estimated by rsm.");
				## Because rsm demands it, rsmDf and rsmFormula must be visible in the global environment:
				rsmDf <- df2
				## This
				## rsmFormula <- as.formula(sprintf("Y ~ SO(%s)", paramString))
				## generates warnings, so we use the long form:
				rsmFormula <- as.formula(sprintf(paste(colnames(Y),"~ FO(%s) + TWI(%s) + PQ(%s)")
														, paramString, paramString, paramString))
				dfc.rsm1 <- spotRsm(formula = rsmFormula, data = rsmDf)
			}
			lhd <- data.frame(apply(lhd, 2, spotHlpF.norm))
			colnames(lhd)<- as.character(makeNNames(nParam))
			res<-predict(dfc.rsm1, lhd)
		}else{
			dfc.rsm1 <-externalFit
			if(ncol(lhd)<nrow(spotConfig$alg.roi)){ #ugly, but necessary because R is strange about converting one dimensional vectors to dataframes (confusing rows/columns)
				lhd <- t(data.frame(apply(lhd, 2, spotHlpF.norm)))
			}
			else{
				lhd <- data.frame(apply(lhd, 2, spotHlpF.norm))
			}
			colnames(lhd)<- as.character(makeNNames(nParam))
			res <- predict(dfc.rsm1,data.frame(lhd))	
		}
		fit<-dfc.rsm1
	}
	spotWriteLines(spotConfig$io.verbosity,1,"spotPredictLm finished");
  	spotConfig$seq.modelFit<-fit;
	spotConfig$seq.largeDesignY<-as.data.frame(res);
	return(spotConfig);
}
