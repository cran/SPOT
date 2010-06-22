###################################################################################
#' Get Raw Result Data 
#'
#' spotGetRawResData is based on \code{\link{spotPrepareData}}  
#'
#' The result (.res) file is read and the data is returned as is 
#' 
#' @param spotConfig the list of all parameters is given, used ones are: 
#' 		\code{spotConfig$io.resFileName:} result file where data are read from
#' 		\code{spotConfig$io.columnSep:} column separator as determined by getConfig
#' parameter to the function call \code{\link{spotPrepareData}} 
#' 
#'
#' @return data.frame \code{rawResData}  \cr
#' - \code{rawResData} contains values from the result file 
#'
#' @references  \code{\link{spotPrepareData}} 
####################################################################################

spotGetRawResData<- function(spotConfig){
	spotWriteLines(spotConfig,2,"  Entering spotGetRawResData");
	## Load .res-file data, the result of the alg run
	spotWriteLines(spotConfig
					, 2
					, paste("Loading result data from::", spotConfig$io.resFileName)
					, con=stderr());
	
	rawResData <- read.table(spotConfig$io.resFileName
			, sep=spotConfig$io.columnSep
			, header = TRUE	
			, stringsAsFactors = TRUE
			);
	spotWriteLines(spotConfig,2,"  Leaving spotGetRawResData");
	return(rawResData)
}

###################################################################################
#' Get Raw Data Matrix B  
#'
#' The result (.res) file is read and the data are prepared for further procesing
#' results in "Matrix B" that consits of the y-vector of the results and the x-Matrix
#' of inputs that are bound together to a matrix B
#' 
#' @param spotConfig the list of all parameters is given, also used as input
#' parameter to the function call spotGetRawResData(spotConfig) the other ones in use are: 
#' 		\code{spotConfig$alg.roi:} the roi data provided as matrix
#' 		\code{spotConfig$alg.resultColumn:} name of the result column 
#'
#' @return data.frame \code{B}  \cr
#' - \code{B} holds a column "y" with the results
#' and all columns with column-names derived from .roi file (should be the parameters
#' of the algorithm. Values are sorted with respect to the y values (increasing) 
#'
#' @references \code{\link{spotPrepareData}} 
####################################################################################
spotGetRawDataMatrixB <- function(spotConfig){
	## read data frame from res file
	rawData <- spotGetRawResData(spotConfig)
	## extract parameter names
	pNames <- row.names(spotConfig$alg.roi);
	y <- rawData[,spotConfig$alg.resultColumn]
	## data frame of parameter values
	x <- as.matrix(rawData[,pNames]);
	A <- cbind(y,x)        
	B <-  data.frame(A[order(y,decreasing=FALSE),]);
	return(B)
}

###################################################################################
#' Get Merged Data Matrix B
#'
#' The merged data that are the result of \code{\link{spotPrepareData}}must be the first input parameter
#' These data prepared and results in "Matrix B" that consits of the y-vector 
#' of the results and the x-Matrix of inputs that are bound together to a matrix B
#' 
#' @param mergedData the Data prepared as done with \code{\link{spotPrepareData}}
#' @param spotConfig the list of all parameters is given, but used only as input
#' parameter to the function call of code{\link{spotPrepareData}} 
#' 
#' @return data.frame \code{B}  \cr
#' - \code{B} holds a column "y" with the results
#' and all columns with column-names derived from .roi file (should be the parameters
#' of the algorithm. Values are sorted with respect to the y values (increasing) 
#'
#' @references  \code{\link{spotPrepareData}} 
####################################################################################
spotGetMergedDataMatrixB <- function(mergedData, spotConfig){
	## Note: requires pre-processing via spotPrepareData()
	##       does not work on raw data	
	## extract parameter names
	pNames <- row.names(spotConfig$alg.roi);
	y <- mergedData$mergedY
	## data frame of parameter values	
	x <- as.matrix(mergedData$x);
	A <- cbind(y,x)        
	B <-  data.frame(A[order(y,decreasing=FALSE),]);
	return(B)
}

###################################################################################
#' Prepare Data for SPOT
#' 
#' spotPrepareData prepares the data from .res-file  
#'
#' The result (.res) file is read and the data are prepared for further procesing
#' results in a list of several values
#' 
#' @param spotConfig the list of all parameters is given, used parameters are: 
#' 		\code{spotConfig$io.resFileName}
#' 		\code{spotConfig$io.columnSep}
#' 		\code{spotConfig$alg.roi}
#' 		\code{spotConfig$io.colname.step}
#' 		\code{spotConfig$alg.resultColumn}
#' 		\code{spotConfig$seq.transformation.func()}
#' 		\code{spotConfig$seq.merge.func()}
#'
#' @return list \code{resultList}  \cr
#' - \code{resultList} is a list of values (not sorted in increasing order!)
#' 		\code{x}: vector of vector (=matrix) of input values, the "points"
#' 		\code{mergedY}: vector of merged fitness values, the result column (each row of x has a corresponding Y-value)
#'		\code{count}: the number of repeats
#' 		\code{CONFIG}: the unique CONFIG number for each unique design point (sdev=NA if count=1)
#' 		\code{pNames}: names of the parameters as derived from roi-file
#' 		\code{step.last}: the maximum number of the step-column of the result-file
#' 		\code{STEP}: SPOT STEP when design point was created  
#'
#' @references  \code{\link{SPOT}} \code{\link{spot}}
####################################################################################
spotPrepareData <- function(spotConfig){
    spotWriteLines(spotConfig,2,"  Entering spotPrepareData");
	rawData <- spotGetRawResData(spotConfig)
	if (!any(names(rawData)=="CONFIG"))
  	   stop("Error: Result file is missing the required column CONFIG!")
    ## extract parameter names
    pNames <- row.names(spotConfig$alg.roi);
	step.last <- NA;
    if (!is.na(spotConfig$io.colname.step)) {
		if (any(names(rawData)==spotConfig$io.colname.step)) #=="STEP"
      		step.last <- max(rawData[spotConfig$io.colname.step])
		else{
			spotWriteLines(spotConfig,2,"Warning: user algorithm does not write STEP column to result file, 0-vector added")
			step.last=0
			rawData$STEP<-rep(0, length(rawData$CONFIG)) 
		}
    }
	z <- split(rawData[,spotConfig$alg.resultColumn], rawData$CONFIG);
	fs <- function(xs) { spotConfig$seq.transformation.func(spotConfig$seq.merge.func(xs)) }
	mergedY <- sapply(z,fs)	
	count <- sapply(z,length) 	
	mergedCONFIG <- sapply(split(rawData$CONFIG, rawData$CONFIG),min)
	mergedSTEP <- sapply(split(rawData$STEP, rawData$CONFIG),min)
	x <- as.data.frame(t(sapply(split(rawData[,pNames], rawData$CONFIG),mean)));	
	resultList<-list( x = x
         , mergedY = mergedY
         , count = count          
         , CONFIG = mergedCONFIG 
         , pNames = pNames
         , step.last = step.last
		 , STEP = mergedSTEP
         )
	spotWriteLines(spotConfig,2,"  Leaving spotPrepareData");
	return(resultList)
}


###################################################################################
#' Spot Prepare Data As Matrix C
#'
#' The result (.res) file is read and the data are prepared for further procesing
#' results in "Matrix C" that consits of the y-vector of the results and the x-Matrix
#' of inputs plus the columns "count", "sdev" and "CONFIG", that are bound together 
#' to a matrix C
#' 
#' @param spotConfig the list of all parameters is given, but used only as input
#' parameter to the function call of \code{\link{spotPrepareData}}  
#'
#' @return Matrix \code{C} \cr
#' - \code{C} holdsg a column "y" with the results
#' and all columns with column-names derived from .roi file (should be the parameters
#' of the algorithm) plus the columns "count", "sdev" and "CONFIG"
#
#' @references  \code{\link{spotPrepareData}} 
####################################################################################
spotPrepareDataAsMatrixC <- function(spotConfig){
	algResults<-spotPrepareData(spotConfig)
	x <- as.matrix(algResults$x);
	y <- algResults$Y;
	A <- cbind(y,x,count=algResults$count,CONFIG=algResults$CONFIG)       
	C <-  data.frame(A[order(y,decreasing=FALSE),]);	
	return(C)
}
