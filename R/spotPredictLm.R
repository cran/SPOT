###################################################################################
#' Spot Predictor Linear Model
#'
#' A simple linear prediction model
#' 
#' This function implements a linear model for prediction. Depending on the numbers of variables 
#' either no interactions,  interaction  between the variables may be used or a full quadratic model is
#' provided.  
#' 
#' @param rawB unmerged data
#' @param mergedB merged data
#' @param lhd new design points which should be predicted
#' @param spotConfig global list of all options, needed to provide data for calling functions
#' 
#' @return data.frame \code{lhd} \cr 
#' - \code{lhd} is a sorted (with respect to fitness, i.e., smallest estimated function value) largeDesign. 
#' Best is first 
#'  
#' @references  \code{\link{SPOT}}
###################################################################################
spotPredictLm <- function(rawB,mergedB,lhd,spotConfig) {
	spotWriteLines(spotConfig,2,"  Entering spotPredictLm");	
	spotInstAndLoadPackages("rsm")
	
	rawB <- spotGetRawDataMatrixB(spotConfig);
	mergedData <- spotPrepareData(spotConfig)
	mergedB <- spotGetMergedDataMatrixB(mergedData, spotConfig);		
	Y<-rawB$y
	pNames <- row.names(spotConfig$alg.roi);
	nParam <- length(pNames)
	X <- rawB[,pNames]
	df1 <- data.frame(cbind(Y,X))
			
	fmla <- NULL				
	for (i in 1:nParam) {
	a <- min(X[,i])
	b <- max(X[,i])
	v1 <- mean(c(a,b))
	v2 <- (b-a)/2	
	fmla <- c(fmla,
			as.formula(paste(paste("x",i,sep=""),
							"~ (", pNames[i], " - ", v1, ")/", v2
							)
					)
					)		
	}	
  df2 <- coded.data(df1, formulas = fmla)

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
  makeNNames <- function(n) { Map(function(i) paste("x", i, sep = ""), 1:n) }
  makeNParameters <- function(n) { Reduce(function(n1, n2) paste(n1, n2, sep=","), makeNNames(n)) }
  paramString <- makeNParameters(nParam) # the string "x1,x2,...,xnParams"
  #rsmDf<-NULL
  #rsmFormula<-NULL
  if (nExp >= nRequired1 && nExp < nRequired2) {
    spotWriteLines(spotConfig,2,"spotPredictLm: First order (FO) effects estimated by rsm.");
    ## Because rsm demands it, rsmDf and rsmFormula must be visible in the global environment:
    rsmDf <- df2
    rsmFormula <- as.formula(sprintf("Y ~ FO(%s)", paramString))
    dfc.rsm1 <- spotRsm(formula = rsmFormula, data = rsmDf)
  }
  else if (nExp >= nRequired2 && nExp < nRequired3) {
	  spotWriteLines(spotConfig,2,"spotPredictLm: First order (FO) with two-way interactions (TWI) effects estimated by rsm.");
    ## Because rsm demands it, rsmDf and rsmFormula must be visible in the global environment:
    rsmDf <- df2
    rsmFormula <- as.formula(sprintf("Y ~ FO(%s) + TWI(%s)", paramString, paramString))
    dfc.rsm1 <- spotRsm(formula = rsmFormula, data = rsmDf)
  }
  else if (nExp >= nRequired3) {
	  spotWriteLines(spotConfig,2,"spotPredictLm: Second order (SO) effects estimated by rsm.");
    ## Because rsm demands it, rsmDf and rsmFormula must be visible in the global environment:
    rsmDf <- df2
    ## This
    ## rsmFormula <- as.formula(sprintf("Y ~ SO(%s)", paramString))
    ## generates warnings, so we use the long form:
    rsmFormula <- as.formula(sprintf("Y ~ FO(%s) + TWI(%s) + PQ(%s)", paramString, paramString, paramString))
    dfc.rsm1 <- spotRsm(formula = rsmFormula, data = rsmDf)
  }
  lhd <- data.frame(apply(lhd, 2, spotHlpF.norm))
  colnames(lhd)<- as.character(makeNNames(nParam))
  res<-predict(dfc.rsm1, lhd)
  lhd <-  lhd[order(res,decreasing=FALSE),];
  lhd <- lhd[1:spotConfig$seq.design.new.size,];
  lhd <- code2val(lhd, codings = codings(df2))
  return(lhd)
  writeLines("spotPredictLm finished");
}
