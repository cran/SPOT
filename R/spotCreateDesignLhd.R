## file renamed from createDesign.R
####################################################################################
#' spotNormDesign() 
#'
#' Produces a normalized design and calculates the minimal distance 
#' if required
#' A design  is a matrix with \code{dim} columns and \code{size} rows
#' 
#' @param dim number, dimension of the problem (will be no. of columns of the result matrix)
#' @param size number of points with that dimension needed. (will be no. of rows of the result matrix)
#' @param calcMinDistance Boolean to indicate whether a minimal distance should be calculated
#'
#' this function is only the basis with a normalized design, that is 
#' used as a basis function to spotDesignLhd()
#'
#' @return list \code{L}  \cr
#' - \code{L} consists of a matrix and nd (if required) a minimal distance 
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
## spotDesignLhd : chosen if the switch in spot::stepInitial is set to lhd
## spotConfig$design.type="spotDesignLhd"
####################################################################################
#' Create a design based on the number of dimensions and the number of design points 
#'
#' The dimension is driven from the number of rows of the .roi - file 
#' (each row in the roi file defines a variable, 
#' In case of a missing number of design points a value is calcuated from the dimension 
#'
#' @param spotConfig list of spotConfiguration
#' @param noDesPoints number of design points, default is NaN
#' @param retries number of retries, which is the number of trials to find a design with the lowest distance, default is NaN
#'
#' @return matrix \code{design} \cr
#' - \code{design} has \code{dimension} columns and \code{noDesPoints} rows
#' with entries corresponding to the region of interest.
####################################################################################

spotCreateDesignLhd <- function(spotConfig, noDesPoints = NaN, retries=NaN) {
	spotWriteLines(spotConfig$io.verbosity,2,"  Entering spotDesignLhd");
	#noDesPoints <- spotConfig$init.design.size
	## retries is the number of trials to find a 
	## design with the greates minimal distance, (default is 1)
	## Calculate required points for initial design
	if(is.na(noDesPoints)) {
		dim <- nrow(spotConfig$alg.roi);
		## Im letzten Term ist das zweite +1 für die Crossvalidierung nötig
		noDesPoints <- max(11*dim,1 + 3 * dim + dim * (dim - 1) / 2 + 1);
		## dim higher than 16, max will take the second arguent, else the first
		## TBB 23 2 2009:
		#noDesPoints <- 10;    
	}
	
	## Bei einer Wiederholung muss die Distanz nicht berechnet werden
	best <- spotNormDesign(nrow(spotConfig$alg.roi),noDesPoints,calcMinDistance=retries>1);
	
	if (retries>1) {
		for (i in 1:(retries-1)) {
			tmpDes <- spotNormDesign(nrow(spotConfig$alg.roi),noDesPoints,calcMinDistance=TRUE);
			## maximize minimal distance
			if (tmpDes$minDistance > best$minDistance)
				best <- tmpDes;
		}
	}
	#browser()
	design <- as.data.frame(best$design);
	colnames(design) <- row.names(spotConfig$alg.roi);  
	print(spotConfig$alg.roi)
	
	for (param in row.names(spotConfig$alg.roi)){
		lowerBound <-  spotConfig$alg.roi[param,"low"];
		upperBound <-  spotConfig$alg.roi[param,"high"];
		
		## Bei x.5 wird zum nächsten GERADEN Wert gerundet (nach IEEE)
		if (spotConfig$alg.roi[param,"type"]  == "INT" || spotConfig$alg.roi[param,"type"]  == "FACTOR"){
			## print( c(param, spotConfig$alg.roi[param,"type"], lowerBound))
			lowerBound <- lowerBound - 0.5;
			upperBound <- upperBound + 0.4999999999999;
		}
		design[param] <- lowerBound + design[param] * (upperBound-lowerBound);
	}
	## Integers (and Factor) Runden
	if (any(spotConfig$alg.roi[["type"]] == "INT") || any(spotConfig$alg.roi[param,"type"]  == "FACTOR"))
		design[spotConfig$alg.roi[["type"]]  == "INT"] <- floor(design[spotConfig$alg.roi[["type"]]  == "INT"]+0.5);
	design[spotConfig$alg.roi[["type"]]  == "FACTOR"] <- floor(design[spotConfig$alg.roi[["type"]]  == "FACTOR"]+0.5);	
	
	if (!is.na(spotConfig$design.paramSignif))
		## Auf den signifikanten Anteil  reduzieren
		design <- signif(design,spotConfig$design.paramSignif);
	##
	spotWriteLines(spotConfig$io.verbosity,2,"  Leaving spotDesignLhd");	
	return(design);
}
