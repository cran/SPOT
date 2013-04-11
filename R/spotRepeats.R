##################################################################################
#' Select repeats of design points based on OCBA
#'
#'
#' @param spotConfig List, containing all settings of SPOT
#' @param mergedData merged data from runs of the target function
#' @param largeDesignEvaluated sorted large design in sequential SPOT step
#' 
#' @seealso  \code{\link{spotGenerateSequentialDesign}} \code{\link{spotOcba}} \code{\link{spotRepeats}}
#' @keywords internal
###################################################################################
spotRepeatsOcba <- function(spotConfig,mergedData,largeDesignEvaluated){
	lastConfigNr <- max(mergedData$CONFIG)
	lastStepNr <- mergedData$step.last
	colnames(largeDesignEvaluated)= row.names(spotConfig$alg.roi); 
	### We select only #seq.design.oldBest.size design points. These design points will be considered for re-evaluation:
	if (spotConfig$seq.design.oldBest.size <= 1) warning("spotRepeatsOcba.R: Increase spotConfig$seq.design.oldBest.size in your conf file, OCBA will not work if it is smaller than 2.");
	selection <- order(mergedData$mergedY)[1:min(nrow(mergedData$x), spotConfig$seq.design.oldBest.size)];
	ocbaData <- cbind(  mergedData$x[selection,]
					, mergedY = data.frame(mergedData$mergedY)[selection,]
					, varY = data.frame(mergedData$varY)[selection,]
					, count = data.frame(mergedData$count)[selection,]
					, CONFIG = data.frame(mergedData$CONFIG)[selection,]
					, STEP = data.frame(mergedData$STEP)[selection,]
					, SEED = data.frame(mergedData$SEED)[selection,]
					) ;
	### Based on OCBA, the budget is distributed among this subset:
	REPEATS <- spotOcba(ocbaData$mergedY, ocbaData$varY, ocbaData$count, spotConfig$seq.ocba.budget, iz=NA, verbose=spotConfig$io.verbosity)
	spotPrint(spotConfig$io.verbosity,1,REPEATS)
	oldD <- cbind(ocbaData, REPEATS=data.frame(REPEATS))                  
	oldD <- oldD[oldD$REPEATS>0,];
	oldD$SEED <- oldD$SEED + 1;
	oldD$mergedY <- NULL
	oldD$varY <- NULL
	oldD$count <- NULL
	### 
	additionalConfigNumbers <- nrow(largeDesignEvaluated)
	CONFIG <- lastConfigNr + 1:additionalConfigNumbers;
	REPEATS <- rep(spotConfig$init.design.repeats, additionalConfigNumbers)
	SEED <- spotConfig$alg.seed
	STEP <- lastStepNr +1
	newD <- cbind(largeDesignEvaluated
					  , CONFIG
					  , REPEATS
					  , STEP
					  , SEED)
	### Combination of old (which should be re-evaluated)  and new design 
	rbind(newD, oldD)
}

##################################################################################
#' Select repeats of design points, linearly increasing
#'
#'
#' @param spotConfig List, containing all settings of SPOT
#' @param mergedData merged data from runs of the target function
#' @param largeDesignEvaluated sorted large design in sequential SPOT step
#' 
#' @seealso  \code{\link{spotGenerateSequentialDesign}} \code{\link{spotOcba}} \code{\link{spotRepeatsOcba}}
#' @keywords internal
###################################################################################
spotRepeats <- function(spotConfig,mergedData,largeDesignEvaluated){
	selection <- order(mergedData$mergedY)[1:spotConfig$seq.design.oldBest.size];
	selectedData=as.data.frame(mergedData$x[selection,]) #MZ: Bugfix for 1 dimensional optimization
	colnames(selectedData)= row.names(spotConfig$alg.roi); #MZ: Bugfix for 1 dimensional optimization
	colnames(largeDesignEvaluated)= row.names(spotConfig$alg.roi); #MZ: Bugfix for 1 dimensional optimization
	oldD <- cbind(selectedData #MZ: Bugfix for 1 dimensional optimization
			, CONFIG = mergedData$CONFIG[selection]           
			, repeatsInternal = mergedData$count[selection]
			, repeatsLastConfig = mergedData$count[max(mergedData$CONFIG)] # holds the number of repeats used for the last configuration of the last step...
	)
	## new, increased number of experiments
	## definable increase function is used - see  seq.design.increase.func in spotGetOptions 
	if(!exists(spotConfig$seq.design.increase.func))stop(paste("The design increase function name", spotConfig$seq.design.increase.func, "is not found in the workspace \n
		Please make sure to load the design increase function in the workspace, or specify the correct function in spotConfig$seq.design.increase.func" ))
	totalWanted<-(eval(call(spotConfig$seq.design.increase.func, 
								max(oldD$repeatsLastConfig))));		
	## seq.design.maxRepeats is the upper bound, so the increasing repeats are limited to that maximum 
	totalWanted <- min(totalWanted,
			spotConfig$seq.design.maxRepeats, 
			na.rm=TRUE);
	## now calculate the number of repeats for those configurations, that were 
	## already evaluated before
	oldD$repeatsInternal <- totalWanted - oldD$repeatsInternal; 
	oldD = oldD[oldD$repeatsInternal>0,]	#remove those with zero repeats (i.e. reached max-repeats)
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
	#additionalConfigNumbers <- nrow(largeDesignEvaluated)
	newCONFIG <- max(mergedData$CONFIG) + 1:nrow(largeDesignEvaluated);
	newD <- cbind(  largeDesignEvaluated
			, CONFIG = newCONFIG
			, repeatsInternal = totalWanted
			, repeatsLastConfig= totalWanted);
	## if old design points have to be evaluated:
	if (sum(oldD$repeatsInternal,na.rm=TRUE) > 0){
		design <- rbind(oldD,newD)}
	## otherwise take the new design points only:
	else{
		design <- newD}
	## now replace the internal identifier with the correct one from spotConfig
	colnames(design)[colnames(design)=="repeatsInternal"] <-"REPEATS";
	## append column with current step
	design <- cbind(design,mergedData$step.last + 1);
	colnames(design)[ncol(design)] <- "STEP";
	## all configurations start with the same seed, automatically increased for each repeat
	## the OLD configurations that are to be calculated again, but with only the missing numbers 
	## of repeats are starting with 
	## alg.seed + <numberOfRepeatsAlreadyEvaluatedForThisConfiguration>
	## or as stated below: alg.seed PLUS (totalWanted MINUS missingRepeatsForThisConfiguration)
	## SEED<-spotConfig$alg.seed+totalWanted-design["REPEATS"]
	## [BUGFIX2]
	SEED<-spotConfig$alg.seed+totalWanted-design[,"REPEATS"]
	design <- cbind(design,SEED);
	## is the following necessary?
	colnames(design)[ncol(design)] <- "SEED";
	design
}