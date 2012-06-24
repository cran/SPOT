## Experimental research in evolutionary computation
## author: thomas.bartz-beielstein@fh-koeln.de
## http://www.springer.com/3-540-32026-1
##
## Copyright (C) 2004-2011  T. Bartz-Beielstein, C. Lasarczyk
## This program is free software;
## you can redistribute it and/or modify it under the terms of the GNU 
## General Public License as published by the Free Software Foundation;
## either version 3 of the License,
## or (at your option) any later version.
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of 
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
## See the GNU General Public License for more details.
## You should have received a copy of the GNU General Public License along 
## with this program; if not, see <http://www.gnu.org/licenses/>.
############################################################################################
#' Generate Design for next sequential evaluation with OCBA
#' 
#' Creates a new design. Design points are determined with respect to the current result file. 
#' Number of repeats are adapted according to the OCBA approach. 
#' Uses the functions \link{spotPrepareData},\link{spotGetRawDataMatrixB},\link{spotGetMergedDataMatrixB},\link{spotWriteLines}
#' \link{spotWriteBest},\link{spotPlotBst},\link{spotOcba}
#' returns a sequential design to be written to the file <xxx>.des (will be determined from calling function)
#' 
#' @param spotConfig the list of all parameters is given, but the used ones are: \cr
#'   spotConfig$io.resFileName: is checked for existence. If not found, function fails with error\cr
#'   spotConfig$algSourceSrcPath: needed for the error message \cr
#'   spotConfig$userConfFileName: needed for the error message \cr
#'   spotConfig$io.verbosity: needed for command window output \cr
#' 
#' @return data.frame \code{design} \cr
#' - \code{design} contains one or more new design points to be calculated 
#' @export
############################################################################################
spotGenerateSequentialDesignOcba <- function(spotConfig) {
	spotWriteLines(spotConfig$io.verbosity,2,"Entering generateSequentialDesign");	
	rawB <- spotGetRawDataMatrixB(spotConfig);
        ## merged data, unsorted:
	mergedData <- spotPrepareData(spotConfig)
	mergedB <- spotGetMergedDataMatrixB(mergedData, spotConfig);
	
	#bugfix, for continuing runs without saving results in spotConfig:
	if(is.null(spotConfig$alg.currentResult))spotConfig$alg.currentResult<- spotGetRawResData(spotConfig)$rawD;
	
# Example data: 
#	"rawB:"
#	          y VARX1 VARX2
#	1  10.96089  10.0   0.0
#	2  17.50830  -5.0  15.0
#	3  24.12996   2.5   7.5
#	4 145.87219  10.0  15.0
#	5 308.12910  -5.0   0.0
#
# "mergedData":
# $x
#        TEMP TMAX
# 1  35.608154   21
# 2   3.030746   31
# 3  35.043096   12
# 4  18.713224   50
# 5  13.996489   35
# 6  26.265450   26
# 7  24.104926    4
# 8   7.341344   25
# 9  49.035177   43
# 10 42.543436    7
# $mergedY
#         1          2          3          4          5          6          7 
# 11.1678372  0.4078694  7.4144527  2.4989917  0.4077170  3.1585922  1.0490716 
#         8          9         10 
# 0.5910223  7.3349334  3.1669658 
# $varY
#            1            2            3            4            5            6 
# 4.465517e+01 1.654812e-04 7.644061e+01 8.417478e+00 9.182110e-05 2.943528e+00 
#           7            8            9           10 
# 7.281525e-01 5.336887e-02 7.418998e+01 8.794129e+00 
# $count
# 1  2  3  4  5  6  7  8  9 10 
# 2  2  2  2  2  2  2  2  2  2 
# $CONFIG
# 1  2  3  4  5  6  7  8  9 10 
# 1  2  3  4  5  6  7  8  9 10 
# $pNames
# [1] "TEMP" "TMAX"
# $step.last
# [1] 0
# $STEP
#  1  2  3  4  5  6  7  8  9 10 
#  0  0  0  0  0  0  0  0  0  0 
#	
# "mergedB:"
# y VARX1 VARX2
# 4  10.96089  10.0   0.0
# 3  17.50830  -5.0  15.0
# 5  24.12996   2.5   7.5
# 1 145.87219  10.0  15.0
# 2 308.12910  -5.0   0.0	
#	
	spotConfig=spotWriteBest(mergedData, spotConfig);
	if(spotConfig$io.verbosity>2){
		spotPlotBst(spotConfig)
	}
### Store the config number of the last configuration used so far. The first new configuration will receive the
### configuration number lastConfigNr + 1:
        lastConfigNr <- max(mergedData$CONFIG)
        lastStepNr <- mergedData$step.last
### We select only #seq.design.oldBest.size design points. These design points will be considered for re-evaluation:
        if (spotConfig$seq.design.oldBest.size <= 1) warning("spotGenerateSequentialDesignOcba.R: Increase spotConfig$seq.design.oldBest.size in your conf file, OCBA will not work if it is smaller than 2.");
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
### Now we have constructed the first part of the des file, i.e., how many repeats should be distributed among existing design points.
### Next, we have to determine new design points based on the meta model (prediction):
	if(!exists(spotConfig$seq.design.func))stop(paste("The design function name", spotConfig$seq.design.func, "is not found in the workspace \n
		Please make sure to load the design function in the workspace, or specify the correct function in spotConfig$seq.design.func" ))
	largeDesign <- (eval(call(spotConfig$seq.design.func
                                  , spotConfig
                                  , spotConfig$seq.design.size
                                  , spotConfig$seq.design.retries))
                        );
#####################################################
### Fit the prediction model and generate new sample points:
### x contains input, y output values
### now calling the seq.predictionModel.func specified in spotConfigure
	if(!exists(spotConfig$seq.predictionModel.func))stop(paste("The prediction model function name", spotConfig$seq.predictionModel.func, "is not found in the workspace \n
		Please make sure to load the prediction model function in the workspace, or specify the correct function in spotConfig$seq.predictionModel.func" ))	
	spotConfig <- eval(call(spotConfig$seq.predictionModel.func
                                        , rawB
                                        , mergedB
                                        , largeDesign
                                         , spotConfig));		 

	##################################################
    ## (2b) If desired, optimize fit returned by prediction model
	if (!is.na(spotConfig$seq.predictionOpt.func)){
		spotConfig <- eval(call(spotConfig$seq.predictionOpt.func
											, largeDesign #start point of optimization	
											, spotConfig));
		largeDesign <-  as.data.frame(rbind(spotConfig$optDesign, as.matrix(largeDesign)),row.names=FALSE); 
		spotConfig$seq.largeDesignY <-  as.data.frame(rbind(spotConfig$optDesignY, as.matrix(spotConfig$seq.largeDesignY)),row.names=FALSE);  #TODO: check where largeDesignY is used, and if it is used correctly!
		spotConfig$optDesignY<-NULL
		spotConfig$optDesign<-NULL
	}
	names(largeDesign)<- row.names(spotConfig$alg.roi);
	#names(spotConfig$seq.largeDesignY)<-"y"
	
	#now sort the largeDesign, to select only best points for next evaluation
	if(length(spotConfig$alg.resultColumn)>1){ #in case of multi criteria spot: "sort" large design by hypervolume contribution nds rank
		#if(spotConfig$seq.mco.infill=="sort"){
		#	largeDesign <- spotMcoSort(largeDesign,spotConfig$seq.largeDesignY,spotConfig$seq.design.new.size)} #TODO only largedesign not largedesignY is sorted. For future applications might be needed to sort both
		#if(spotConfig$seq.mco.infill=="fill"){
			mergY <-  eval(call(spotConfig$seq.predictionModel.func, NULL, NULL  #reevaluate known points on model, to be in the same scale as the largeDesignY
								, mergedData$x
								, spotConfig
                                , spotConfig$seq.modelFit 
								))$seq.largeDesignY
			largeDesign <- spotMcoInfill(largeDesign,spotConfig$seq.largeDesignY,spotConfig$seq.design.new.size, mergedData$x,mergY)
		#}
	}else{ #in case of single criteria spot: sort large design by criteria value
		largeDesign <-  as.data.frame(largeDesign[order(spotConfig$seq.largeDesignY,decreasing=FALSE),]);
		#spotConfig$seq.largeDesignY <-  as.data.frame(spotConfig$seq.largeDesignY[order(spotConfig$seq.largeDesignY,decreasing=FALSE),]);
	}		
	
	largeDesignEvaluated <- as.data.frame(largeDesign[1:spotConfig$seq.design.new.size,]); #limit to set design size
	#####################################################
    spotPrint(spotConfig$io.verbosity,1,"largeDesignEvaluated:")
	spotPrint(spotConfig$io.verbosity,1,largeDesignEvaluated)
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
        design <- rbind(newD, oldD)
        spotPrint(spotConfig$io.verbosity,1,"design:")
        spotPrint(spotConfig$io.verbosity,1,design)
	spotWriteLines(spotConfig$io.verbosity,2,"  Leaving generateSequentialDesign");
	## write the design to the .des-file	
	if (spotConfig$spot.fileMode){
		spotWriteDes(design,spotConfig$io.verbosity,spotConfig$io.columnSep,spotConfig$io.desFileName)	
	}#else{
	spotConfig$alg.currentDesign<-design;		
	#}
	return(spotConfig);
}
