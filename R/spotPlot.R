##################################################################################
# plot function spotPlot() 
# collection of plotting procedures
###################################################################################

###################################################################################
#' Read Bst File
#' This function reads the .bst file of the current project. The data is used
#' for plots and reports.
#' 
#' @param spotConfig the list of all parameters is given, used here 
#' 		\code{spotReadBstFile}: the name pf the .bst file to be read
#' 		\code{spotConfig$io.columnSep}: Separator for the columns 
#'
#' @return data.frame \code{bstData} \cr
#' - \code{bstData} holds a column "y" with the results and all columns with column-names 
#' derived from .roi file (should be the parameters of the algorithm
#'
#' @references  \code{\link{SPOT}}
####################################################################################
spotReadBstFile<-function(spotConfig){
	bstData <- read.table(spotConfig$io.bstFileName
			, sep=spotConfig$io.columnSep
			, header = TRUE	
			, stringsAsFactors = TRUE
	);
	return(bstData)
}

###################################################################################
#' Plot Best
#'
#' Function  used to continuosly plot the actually retrieved best value 
#' throughout a SPOT run. The number of variables shown is limited to twelve -
#' make sure the relevant variables belong to the first lines of your .roi-file.
#' 
#' @param spotConfig the list of all parameters is given, used here 
#' 		\code{alg.roi}: the region of interest reduced to a matrix
####################################################################################
spotPlotBst <- function(spotConfig){	
	rawB <- spotGetRawDataMatrixB(spotConfig);
	nEvals <- nrow(rawB)
	pNames <- row.names(spotConfig$alg.roi);
	b<-spotReadBstFile(spotConfig)	
	Y = b$Y
	n <- length(Y)
	step<-1:n
	# do not try to show more than 11 variables or the graphic window will crash 
	numPlots<-min(length(pNames),11,spotConfig$report.io.maxPlots)
	if(numPlots==0) 
		return # if there is nothing to do, do not try to 
	if(numPlots>=4){ # more than 3 variables are not nice to plot, but possible: 
		nRows <- ceiling(sqrt(1+numPlots))
		nCols <- ceiling(numPlots/sqrt(1+numPlots))
 	} else {	## if we have just 3 variables, we can use a simple version here:
		nRows <- 1+numPlots
		nCols <- 1
	}
	par(mfrow=c(nRows, nCols), xpd=NA)
	plot(step,Y, type="b", ylab = "Y", main = paste("Eval: ",as.character(nEvals), ", Y: ", as.character(Y[length(Y)])))	
	for (i in 1:numPlots){
		plot(step,t(b[pNames[i]]), type="b", ylab = as.character(pNames[i]))	
	}
	## uncomment the following to - but reduce the mximum of variables to 10...
	## plot a simple tree
	##	rawB <- spotGetRawDataMatrixB(spotConfig);	
	##	fit.tree <- rpart(y ~ ., data= rawB)
	##	if (!is.null(fit.tree$splits)){		
	##		draw.tree(clip.rpart(fit.tree, best=5 ), digits=2, print.levels=FALSE)
	##	}	
}


