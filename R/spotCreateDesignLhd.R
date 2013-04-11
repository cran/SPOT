## file renamed from createDesign.R
####################################################################################
#' spotNormDesign
#'
#' Produces a normalized design and calculates the minimal distance 
#' if required.
#' A design  is a matrix with \code{dim} columns and \code{size} rows.
#' 
#' @param dim number, dimension of the problem (will be no. of columns of the result matrix)
#' @param size number of points with that dimension needed. (will be no. of rows of the result matrix)
#' @param calcMinDistance Boolean to indicate whether a minimal distance should be calculated
#'
#' @seealso This function is used as a basis for \code{\link{spotCreateDesignLhd}}.
#'
#' @return list \code{L}  \cr
#' - \code{L} consists of a matrix and nd (if required) a minimal distance 
#' @export
#' @keywords internal
#' @author Christian Lasarczyk
####################################################################################
spotNormDesign <- function(dim,size, calcMinDistance=FALSE){
	step <- 1/size;
	design <- replicate(dim, sample(0:(size-1),size) * step + runif(size) * step);
	
	if (calcMinDistance)
		minDistance <- min(dist(design))
	else
		minDistance <- NA;
	
	list(  design=design
			, minDistance=minDistance); 
}


####################################################################################
#' spotCreateDesignLhd
#'
#' Create a design based on the number of dimensions and the number of design points 
#' The dimension is driven from the number of rows of the .roi - file 
#' (each row in the roi file defines a variable, 
#' In case of a missing number of design points a value is calcuated from the dimension 
#'
#' @param spotConfig list of spotConfiguration
#' @param noDesPoints number of design points, default is NaN
#' @param retries number of retries, which is the number of trials to find a design with the lowest distance, default is 1
#'
#' @return matrix \code{design} \cr
#' - \code{design} has \code{dimension} columns and \code{noDesPoints} rows
#' with entries corresponding to the region of interest.
#' @export
#' @author Christian Lasarczyk
#' @seealso \code{\link{spotCreateDesignBasicDoe}}, \code{\link{spotCreateDesignFrF2}}, 
#' \code{\link{spotCreateDesignLhs}}, \code{\link{spotCreateDesignLhsOpt}}
####################################################################################
spotCreateDesignLhd <- function(spotConfig, noDesPoints = NaN, retries= 1) {
	spotWriteLines(spotConfig$io.verbosity,2,"  Entering spotCreateDesignLhd");
	#noDesPoints <- spotConfig$init.design.size
	## retries is the number of trials to find a 
	## design with the greates minimal distance, (default is 1)
	## Calculate required points for initial design
	
	## use roi or aroi:
	if(spotConfig$spot.fileMode){
		if(file.exists(spotConfig$io.aroiFileName))
			roiConfig <- spotReadRoi(spotConfig$io.aroiFileName,spotConfig$io.columnSep,spotConfig$io.verbosity)
		else
			roiConfig <- spotReadRoi(spotConfig$io.roiFileName,spotConfig$io.columnSep,spotConfig$io.verbosity)
	}else{
		roiConfig <- spotConfig$alg.aroi
		if(is.null(roiConfig)) roiConfig <- spotConfig$alg.roi
	}
	
	if(is.na(noDesPoints)) {
		dim <- nrow(roiConfig);
		## Im letzten Term ist das zweite +1 für die Crossvalidierung nötig
		noDesPoints <- max(11*dim,1 + 3 * dim + dim * (dim - 1) / 2 + 1);
		## dim higher than 16, max will take the second arguent, else the first		
	}
	
	## Bei einer Wiederholung muss die Distanz nicht berechnet werden
	best <- spotNormDesign(nrow(roiConfig),noDesPoints,calcMinDistance=retries>1);
	
	if (retries>1) {
		for (i in 1:(retries-1)) {
			tmpDes <- spotNormDesign(nrow(roiConfig),noDesPoints,calcMinDistance=TRUE);
			## maximize minimal distance
			if (tmpDes$minDistance > best$minDistance)
				best <- tmpDes;
		}
	}
	
	design <- as.data.frame(best$design);
	colnames(design) <- row.names(roiConfig);  
	
	for (param in row.names(roiConfig)){
		lowerBound <-  spotConfig$alg.roi[param,"lower"];
		upperBound <-  spotConfig$alg.roi[param,"upper"];
		
		## Bei x.5 wird zum nächsten GERADEN Wert gerundet (nach IEEE), so wird aus 2.2500 2.2 und aus 2.3500 2.4
		if (roiConfig[param,"type"]  == "INT" || roiConfig[param,"type"]  == "FACTOR"){
			## print( c(param, spotConfig$alg.roi[param,"type"], lowerBound))
			lowerBound <- lowerBound - 0.5;
			upperBound <- upperBound + 0.4999999999999;
		}
		design[param] <- lowerBound + design[param] * (upperBound-lowerBound);
	}
	## Integers (and Factor) runden
	###if (any(spotConfig$alg.roi[["type"]] == "INT") || any(spotConfig$alg.roi[param,"type"]  == "FACTOR"))
	design[roiConfig[["type"]]  == "INT"] <- floor(design[roiConfig[["type"]]  == "INT"]+0.5)
	design[roiConfig[["type"]]  == "FACTOR"] <- floor(design[roiConfig[["type"]]  == "FACTOR"]+0.5)
	
	spotWriteLines(spotConfig$io.verbosity,2,"  Leaving spotCreateDesignLhd")
	design
}
