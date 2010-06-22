###################################################################################################
#'  Default Report Function
#' 
#' Function to generate a simple report.
#'
#' This function is used when no report function is requested by the user. It creates some text output
#' and also draws a tree, printing it to screen or pdf. If the \code{report.io.pdf} setting is TRUE
#' the graphic is printed to a pdf file (usually named like your .conf file, and placed in the same folder)
#' if \code{report.io.screen} is set TRUE the graphic is printed to the screen. Both can be FALSE or TRUE
#' at the same time. If the user doesnt specify those values, the defaults will be used as shown in
#' \code{\link{spotGetOptions}}, which means there will be only screen output, and no pdf.
#' 
#' @param spotConfig the configuration list of all spot parameters
#' @references  \code{\link{SPOT}} \code{\link{spot}} \code{\link{spotStepReport}} 
###################################################################################################
spotReportDefault <- function(spotConfig) {		
	spotWriteLines(spotConfig,2,"  Entering spotReportDefault");	
	rawB <- spotGetRawDataMatrixB(spotConfig);
	print(summary(rawB));
	mergedData <- spotPrepareData(spotConfig)
	mergedB <- spotGetMergedDataMatrixB(mergedData, spotConfig);	
	C1 = spotWriteBest(mergedData, spotConfig);
	C1 = C1[C1$COUNT==max(C1$COUNT),];    # choose only among the solutions with high repeat
	cat(sprintf("\n Best solution found with %d evaluations:\n",nrow(rawB)));
	print(C1[1,]);
		
	fit.tree <- rpart(y ~ ., data= rawB)	
	
	if (!is.null(fit.tree$splits)){
		if(spotConfig$report.io.pdf==TRUE){ #if pdf should be created
			pdf(spotConfig$io.pdfFileName) #start pdf creation
			spotPlotBst(spotConfig)
			par(mfrow=c(1,1))
			draw.tree(fit.tree, digits=4)	
			dev.off() #close pdf device
		}
		if(spotConfig$report.io.screen==TRUE) #if graphic should be on screen
		{
			x11()
			par(mfrow=c(1,1), xpd=NA)
			draw.tree(fit.tree, digits=4)	
		}
	}	
}
