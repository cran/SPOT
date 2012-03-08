
####################################################################################
## create a factorial design : chosen if the switch in spot::stepInitial is set to doe
## spotConfig$design.type="spotDesignDoe"
####################################################################################
#' Create a factorial design with resolution III  
#' 
#' Uses the function FrF2 from the FrF2-package to generate a (fractional) factorial design
#'
#' The dimension is determined from the number of rows of the .roi - file 
#' (each row in the roi file defines a variable). 
#'
#' @param spotConfig list of spotConfiguration, where only the table $alg.roi (.roi-file) is used,
#' @param noDesPoints optional parameter gives the number of design points
#' @param repeats is obsolete for "doe" for dow gives a deterministic reurn value. The parameter is given to meet the standard interface to design creating functions (default is 1)
#'
#' @return matrix \code{M} \cr
#' - \code{M} has \code{dimension} columns and \code{noDesPoints} rows
#' @export
####################################################################################
spotCreateDesignDoeR3 <- function(spotConfig, noDesPoints = 100, repeats=1){
#MZ: Default value of noDesPoints is bad, should be NA instead, which should lead to a automatical determination of the number.
	spotWriteLines(spotConfig$io.verbosity,2,"  Entering spotCreateDesignDoe.R::spotCreateDesignDoe()");
	spotInstAndLoadPackages(c('FrF2',  'DoE.wrapper'))
	
	pNames <- row.names(spotConfig$alg.roi);
	lowerBound <-  spotConfig$alg.roi[ ,"lower"];
	upperBound <-  spotConfig$alg.roi[ ,"upper"];
	
	A <- t(rbind(t(lowerBound), t(upperBound)))
		 
	M<-FrF2(nfactors=length(pNames), resolution=3)
	if (nrow(M)<noDesPoints){ # JZ test must be made here, because now lines are added to M 
		stop("Number of initial design points (init.design.size) too large for this design")		
	}
	M<-add.center(M, ncenter =1)	
	M<-as.matrix(M)	#MZ: This is broken or useless, since M (as defined here) will not be used again
	
	d1<-ccd.design(nfactors=length(pNames), ncenter=1, default.levels=c(-1,1),blocks=1)
	M<-as.matrix(unique(cbind(d1$X1,d1$X2))) #MZ: M is redefined here, limited to 2 variables... 
		
	for (i in 1:nrow(M)){
		for (j in 1: ncol(M)){
			M[i,j] <- A[j,1] + (M[i,j] +1)/2 * (A[j,2] - A[j,1])
		}
	}
	colnames(M) <- pNames
	x <- 1:nrow(M)
	# reduces to the number requestet
	M <- M[ sample(x,replace=FALSE)[1:noDesPoints], ]	
	spotWriteLines(spotConfig$io.verbosity,2,"  Leaving spotCreateDesignDoe.R::spotCreateDesignDoe");
	return(M);		
}