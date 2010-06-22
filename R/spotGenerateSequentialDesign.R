############################################################################################
#' function spotGenerateSequentialDesign() 
#' 
#' Creates a new design. Design points are determined with respect to the current result file. 
#' 
#' Uses the functions \link{spotPrepareData},\link{spotGetRawDataMatrixB},\link{spotGetMergedDataMatrixB},\link{spotWriteLines}
#' \link{spotWriteBest},\link{spotPlotBst},\link{spotSafelyAddSource}
#' returns a sequential design to be written to the file <xxx>.des (will be determined from calling function)
#' 
#' @param spotConfig the list of all parameters is given, but the used ones are: 
#'   spotConfig$io.resFileName: is checked for existence is not, function fails with error
#'   spotConfig$algSourceSrcPath: needed for the error message 
#'   spotConfig$userConfFileName: needed for the error message
#'   spotConfig$io.verbosity:
#' 
#' @return data.frame \code{design} \cr
#' - \code{design} contains one or more new design points to be calculated 
############################################################################################
spotGenerateSequentialDesign <- function(spotConfig) {
	spotWriteLines(spotConfig,2,"Entering generateSequentialDesign");	
	rawB <- spotGetRawDataMatrixB(spotConfig);
	mergedData <- spotPrepareData(spotConfig)
	mergedB <- spotGetMergedDataMatrixB(mergedData, spotConfig);
# browser()
# Example data: 
#	"rawB:"
#	          y VARX1 VARX2
#	1  10.96089  10.0   0.0
#	2  17.50830  -5.0  15.0
#	3  24.12996   2.5   7.5
#	4 145.87219  10.0  15.0
#	5 308.12910  -5.0   0.0
#	
#	"mergedData:"
#	$x
#	VARX1 VARX2
#	1  10.0  15.0
#	2  -5.0   0.0
#	3  -5.0  15.0
#	4  10.0   0.0
#	5   2.5   7.5
#	$mergedY
#	1         2         3         4         5 
#	145.87219 308.12910  17.50830  10.96089  24.12996 
#	$count
#	1 2 3 4 5 
#	1 1 1 1 1 
#	$CONFIG
#	1 2 3 4 5 
#	1 2 3 4 5 
#	$pNames
#	[1] "VARX1" "VARX2"
#		$step.last
#	[1] 0
#	
#	"mergedB:"
#	y VARX1 VARX2
#	4  10.96089  10.0   0.0
#	3  17.50830  -5.0  15.0
#	5  24.12996   2.5   7.5
#	1 145.87219  10.0  15.0
#	2 308.12910  -5.0   0.0	
#	
	spotWriteBest(mergedData, spotConfig);
	if(spotConfig$io.verbosity>2){
		spotPlotBst(spotConfig)
	}
	#####################################################
	## (1) Here it is most important to cover a broad area of 
	## the search space, so Latin hypercube designs are preferred to factorial designs.
	## The user can specify what ever he wants...
	spotSafelyAddSource(spotConfig$seq.design.path,
			spotConfig$seq.design.func,
			spotConfig)
	largeDesign <- (eval(call(spotConfig$seq.design.func, 
								spotConfig, 
								spotConfig$seq.design.size, 
								spotConfig$seq.design.retries)));
	#print(largeDesign)
	#####################################################
	### (2) Fit the prediction model and generate new sample points:
	### x contains input, y output values
	### now calling the seq.predictionModel.func specified in spotConfigure
	### the prediction model is build with the values from the resfiles
	spotSafelyAddSource(spotConfig$seq.predictionModel.path,
			spotConfig$seq.predictionModel.func, spotConfig)
	largeDesignEvaluated <-eval(call(spotConfig$seq.predictionModel.func,
					rawB, 
					mergedB, 
					largeDesign, 
					spotConfig))	
	## print(largeDesignEvaluated)	
	##################################################
    ## (3) Adaptation of the number of repeats and
    ## (4) Combination of old (which should be re-evaluated)  and new design points 
	selection <- order(mergedData$mergedY)[1:spotConfig$seq.design.oldBest.size];
	lastConfigNr<-max(mergedData$CONFIG)
	oldD <- cbind(mergedData$x[selection,]
			, CONFIG = mergedData$CONFIG[selection]           
			, repeatsInternal = mergedData$count[selection]
			, repeatsLastConfig = mergedData$count[lastConfigNr] # holds the number of repeats used for the last configuration of the last step...
	)
	#	      	

## new, increased number of experiments
	## definable increase function is used - see  seq.design.increase.func in spotGetOptions 
	spotSafelyAddSource(spotConfig$seq.design.increase.path,
			spotConfig$seq.design.increase.func,spotConfig)
	totalWanted<-(eval(call(spotConfig$seq.design.increase.func, 
								max(oldD$repeatsLastConfig))));
	
	## seq.design.maxRepeats is the upper bound, so the increasing repeats are limited to that maximum 
	totalWanted <- min(totalWanted,
			spotConfig$seq.design.maxRepeats, 
			na.rm=TRUE);	
	## now calculate the number of repeats for those configurations, that were 
	## already evaluated before - but perhaps not with less repeats, so some repeats are 
	## to be done NOW (but not the same as for the new configurations)
	oldD$repeatsInternal <- totalWanted - oldD$repeatsInternal; 
	
	## The following might cause problems for the aroi configurations, so continue at 
	## label [BUXFIX1].
	##
	## Handling of:
	## spotConfig$seq.design.new.size > nrow(largeDesignEvaluated)
	## This problem might occur if
	## the meta model predicts less candidate points than
	## spotConfig$seq.design.new.size
	## additionalConfigNumbers <- min(spotConfig$seq.design.new.size, nrow(largeDesignEvaluated))
	##
	## [BUGFIX1]
	additionalConfigNumbers <- nrow(largeDesignEvaluated)
	newCONFIG <- max(mergedData$CONFIG) + 1:additionalConfigNumbers;
	newD <- cbind(  largeDesignEvaluated
			, CONFIG = newCONFIG
			, repeatsInternal = totalWanted
			, repeatsLastConfig= totalWanted);
	## if old design points have to be evaluated:
	if (oldD$repeatsInternal > 0){
		design <- rbind(oldD,newD);
	}
	## otherwise take the new design points only:
	else{
		design <- newD
	}
	## now replace the internal identifier with the correct one from spotConfig
	colnames(design)[colnames(design)=="repeatsInternal"] <-
		spotConfig$io.colname.repeats;
	## append column with current step
	if (!is.na(spotConfig$io.colname.step)) {
		design <- cbind(design,mergedData$step.last + 1);
		colnames(design)[ncol(design)] <- spotConfig$io.colname.step;
	}
	## all configurations start with the same seed, automatically increased for each repeat
	## the OLD configurations that are to be calculated again, but with only the missing numbers 
	## of repeats are starting with 
	## alg.seed + <numberOfRepeatsAlreadyEvaluatedForThisConfiguration>
	## or as stated below: alg.seed PLUS (totalWanted MINUS missingRepeatsForThisConfiguration)
	## SEED<-spotConfig$alg.seed+totalWanted-design[spotConfig$io.colname.repeats]
	## [BUGFIX2]
	SEED<-spotConfig$alg.seed+totalWanted-design[,spotConfig$io.colname.repeats]
	design <- cbind(design,SEED);
	## is the following necessary?
	colnames(design)[ncol(design)] <- "SEED";
	#
	spotWriteLines(spotConfig,2,"  Leaving generateSequentialDesign");
	return(design);
}
