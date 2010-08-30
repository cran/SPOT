###################################################################################################
#'  Default Report Function for Meta Runs
#' 
#' Function to generate a simple report for meta runs.
#'
#' This function draws a scatterplot matrix (based on car), printing it to screen or pdf. 
#' If the \code{report.io.pdf} setting is TRUE
#' the graphic is printed to a pdf file (usually named like your .conf file, and placed in the same folder)
#' if \code{report.io.screen} is set TRUE the graphic is printed to the screen. Both can be FALSE or TRUE
#' at the same time. If the user doesnt specify those values, the defaults will be used as shown in
#' \code{\link{spotGetOptions}}, which means there will be only screen output, and no pdf.
#' 
#' @param spotConfig the configuration list of all spot parameters
#' @references  \code{\link{SPOT}} \code{\link{spot}} \code{\link{spotStepReport}} 
###################################################################################################
spotReportMetaDefault <- function(spotConfig) {		
	spotWriteLines(spotConfig$io.verbosity,2,"  Entering spotReportMetaDefault");
	### generate formula for scatterplots:
	pNames <- spotMetaGetMetaVarNames(spotConfig)
	## number of parameters
	nParam <- length(pNames)
	##
	fmla <- "~Y"
	for (i in 1:nParam) {
		fmla <- paste(fmla,pNames[i],sep="+")
	}
	### Now that we have the formula, we read the data from the fbs file
	fbs.df <- read.table(spotConfig$io.fbsFileName			
			, header = TRUE
			, as.is=TRUE
	);
	
	### ToDo: Library handling:
	library(car)
	
  plotFormula <- as.formula(fmla)
	if(spotConfig$report.io.pdf==TRUE){ #if pdf should be created
		pdf(spotConfig$io.pdfFileName) #start pdf creation
		scatterplot.matrix(plotFormula, reg.line=lm, smooth=TRUE, span=0.5, 
				diagonal = 'density', data=fbs.df)
		dev.off() #close pdf device
	}
	if(spotConfig$report.io.screen==TRUE) #if graphic should be on screen
	{
		x11()
		#par(mfrow=c(1,1), xpd=NA)
		scatterplot.matrix(plotFormula, reg.line=lm, smooth=TRUE, span=0.5, 
				diagonal = 'density', data=fbs.df)			
	}
}	

