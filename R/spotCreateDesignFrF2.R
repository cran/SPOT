####################################################################################
## create a factorial design : chosen if the switch in spot::stepInitial is set to doe
## spotConfig$design.type="spotDesignFrF2"
####################################################################################
#' Create a resolution III design with center point 
#' based on the number of dimensions and the number of design points
#' 
#' Uses the function FrF2 from the FrF2-package to generate a (fractional) factorial design
#'
#' The dimension is determined from the number of rows of the .roi - file 
#' (each row in the roi file defines a variable).
#'  
#'
#' @param spotConfig list of spotConfiguration
#' @param noDesPoints number of design points, default is NaN
#' @param repeats number of repeats, default is NaN
#'
#' @return matrix \code{M} \cr
#' - \code{M} has \code{dimension} columns and \code{noDesPoints} rows
####################################################################################
spotCreateDesignFrF2 <- function(spotConfig, noDesPoints = NaN, repeats=NaN){	
	spotWriteLines(spotConfig$io.verbosity,2,"  Entering spotCreateDesignFrF2.R::spotCreateDesignFrF2()");
	spotInstAndLoadPackages(c('FrF2',  'DoE.wrapper'))
	#require(DoE.wrapper)#allready required in spotInstAndLoadPackages function
	
	if (spotConfig$seq.useAdaptiveRoi){ 
		if(spotConfig$spot.fileMode){ 
			roiConfig <- spotReadAroi(spotConfig)	
		}else{
			roiConfig <- spotConfig$alg.aroi
		}
	}	
	else{
		roiConfig <- spotConfig$alg.roi
	}	
	pNames <- row.names(roiConfig);
	lowerBound <-  roiConfig[ ,"low"];
	upperBound <-  roiConfig[ ,"high"];
	
	A <- t(rbind(t(lowerBound), t(upperBound)))
	
	# design: range from -1 to +1
	M<-FrF2(nfactors=length(pNames), resolution=3)
	
	## add center point:
	## M<-add.center(M, ncenter =1)
	## or add center point plus stars:	
	M <- ccd.augment(M, ncenter = 1, columns="all",
			alpha = sqrt(2)/2, randomize=TRUE)
    ## remove block information
	M <- M[,-1]
    ##delete replicates
	M <- unique(M)
	###############
	
	M<-as.matrix(M)	
		
	for (i in 1:nrow(M)){
		for (j in 1: ncol(M)){
			M[i,j] <- A[j,1] + (M[i,j] +1)/2 * (A[j,2] - A[j,1])
		}
	}		
	colnames(M) <- pNames			
	spotWriteLines(spotConfig$io.verbosity,2,"  Leaving spotCreateDesignFrF2.R::spotCreateDesignFrF2");
	return(M);		
}