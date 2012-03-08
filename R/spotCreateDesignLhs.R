####################################################################################
## create a LHD via Latin hypercube sampling:
####################################################################################
#' Create a resolution III design with center point 
#' based on the number of dimensions and the number of design points
#' 
#' Uses the function maxminLHS from the lhs to generate a  Latin Hypercube Design. 
#' This function attempts to optimize the sample  
#' by maximizing the minium distance between design points (maximin criteria).
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
#' with entries corresponding to the region of interest.
#' @export
####################################################################################
spotCreateDesignLhs <- function(spotConfig, noDesPoints = NaN, repeats=NaN){	
	spotWriteLines(spotConfig$io.verbosity,2,"  Entering spotCreateDesignLhs.R::spotCreateDesignLhs()");
	spotInstAndLoadPackages("lhs")	
	
	## use roi or aroi:
	if (spotConfig$seq.useAdaptiveRoi){ 
		if(spotConfig$spot.fileMode){
			roiConfig <- spotReadRoi(spotConfig$io.aroiFileName,spotConfig$io.columnSep,spotConfig$io.verbosity)
		}else{
			roiConfig <- spotConfig$alg.aroi
		}
	}
	else{
		roiConfig <- spotConfig$alg.roi
	}
	pNames <- row.names(roiConfig);
	lowerBound <-  roiConfig[ ,"lower"];
	upperBound <-  roiConfig[ ,"upper"];
		
	## old version
	#pNames <- row.names(spotConfig$alg.roi);
	#lowerBound <-  spotConfig$alg.roi[ ,"lower"];
	#upperBound <-  spotConfig$alg.roi[ ,"upper"];
	
	A <- t(rbind(t(lowerBound), t(upperBound)))
			
        ## modified Jan, 31 2011 TBB:
        ## M<-as.matrix(maximinLHS(noDesPoints, length(pNames), dup=2))
        M<-as.matrix(improvedLHS(noDesPoints, length(pNames), dup=2))
	## M has entries in the range from 0 to 1, so we 
	## transform these values into the regions of interest:
	for (i in 1:nrow(M)){
		for (j in 1: ncol(M)){
			M[i,j] <- A[j,1] + M[i,j] * (A[j,2] - A[j,1])
		}
	}
	M <- as.data.frame(M)
	colnames(M) <- pNames	
	spotWriteLines(spotConfig$io.verbosity,2,"  Leaving spotCreateDesignLhs.R::spotCreateDesignLhs");
	return(M);		
}
