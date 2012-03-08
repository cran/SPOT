###################################################################################################
#' 3d Plot of Meta Model - Report Function
#' 
#' Function to generate a 3d surface plot of the predicted meta model.
#'
#' This report function uses the parameter spotConfig$seq.modelFit to plot the predicted model.
#' If spotConfig$seq.modelFit is NULL, the model is generated, based on spotConfig$seq.predictionModel.func.
#' It is not recommended to use this function at the end of an "auto" run of SPOT, make sure to save results first. 
#' By default, twiddler will be used to let the user specify which of the parameters should be varied in the plot.
#' Values not varied in the graph are fixed to their "best" value according to the current Results.
#'
#' @param spotConfig the configuration list of all spot parameters. \cr
#'				The parameter spotConfig$report.interactive=TRUE will be set as default if not contained in the list.
#'				That means, by default the user will be asked to specify which parameters will be varied when the report is started. This is done in a small twiddler gui. 
#'				If the user wants to specify which parameters should be plotted against each other, before starting the report, he can 
#'				set the parameter spotConfig$report.aIndex and spotConfig$report.bIndex. They should be two different integer numbers. They will only be used if 
#'				spotConfig$report.interactive is FALSE. By default they will be set to 1 and 2, so the first two parameters in the ROI will be plotted.
#' @export
###################################################################################################
spotReport3d <- function(spotConfig) {
	#set unset defaults
	if(is.null(spotConfig$report.interactive)){spotConfig$report.interactive=TRUE}
	if(is.null(spotConfig$report.main)){spotConfig$report.main="predicted Model"}
	#load packages
	spotInstAndLoadPackages("rgl")
	spotInstAndLoadPackages("twiddler")		
	spotWriteLines(spotConfig$io.verbosity,2,"  Entering spotReportDefault");	
	rawB <- spotGetRawDataMatrixB(spotConfig);
	spotPrint(spotConfig$io.verbosity,1,summary(rawB));
	mergedData <- spotPrepareData(spotConfig)
	mergedB <- spotGetMergedDataMatrixB(mergedData, spotConfig);	
	spotConfig=spotWriteBest(mergedData, spotConfig);
	C1=spotConfig$alg.currentBest[nrow(spotConfig$alg.currentBest),]
	spotWriteLines(spotConfig$io.verbosity,1," ");
	spotPrint(spotConfig$io.verbosity,1,paste("Best solution found with ",nrow(rawB)," evaluations:",sep=""));
	spotPrint(spotConfig$io.verbosity,1,C1);	
	a=1;
	b=2;
	plotFn <- function(aIndex, bIndex) {
		if((aIndex==bIndex)){#here: add more errors
			warning("Plot index problem: both chosen indices are the same") 
		}
		xlim = c(spotConfig$alg.roi$lower[aIndex],spotConfig$alg.roi$upper[aIndex]);
		ylim = c(spotConfig$alg.roi$lower[bIndex],spotConfig$alg.roi$upper[bIndex]);

		fn <- function(a,b){
			x<-matrix(t(C1[2:(1+nrow(spotConfig$alg.roi))]),nrow(spotConfig$alg.roi),length(a))
			x<-t(x)
			x[,aIndex]=a
			x[,bIndex]=b
			colnames(x) <- row.names(spotConfig$alg.aroi);	
			if(is.null(spotConfig$seq.modelFit)){
				spotConfig1 <- eval(call(spotConfig$seq.predictionModel.func
                                        , rawB
                                        , mergedB
                                        , as.data.frame(x)
                                         ,spotConfig));
			}else{
				spotConfig1 <- eval(call(spotConfig$seq.predictionModel.func
                                , NULL 
								, NULL
								, as.data.frame(x)
								, spotConfig
                                , spotConfig$seq.modelFit #external fit is used, model is only evaluated not build
								));
			}
			ldY <-  spotConfig1$seq.largeDesignY[[1]];
		}
		main = spotConfig$report.main
		xsteps = 100
		ysteps = xsteps
		x <- seq(xlim[1], xlim[2], length = xsteps)
		y <- seq(ylim[1], ylim[2], length = ysteps) 
		z <- outer(x, y, fn)	
		zlim <- range(z)
		zlen <- zlim[2] - zlim[1] + 1
		colorlut <- topo.colors(zlen)
		col <- colorlut[ z-zlim[1]+1 ]
		open3d()
		persp3d(x, y, z, color=col, xlab=row.names(spotConfig$alg.roi)[aIndex],ylab=row.names(spotConfig$alg.roi)[bIndex], main = main)
		datx<-mergedB[,rownames(spotConfig$alg.roi)]
		dat<-data.frame(datx[,c(aIndex,bIndex)],mergedB$y)
		rgl.points(dat,color="black",size=4)
	}
	if(spotConfig$report.interactive==TRUE){
		twiddle(plotFn(a, b), eval = FALSE, a = knob(c(1, nrow(spotConfig$alg.roi)), res = 1),
			b = knob(c(1, nrow(spotConfig$alg.roi)), res = 1))
	}
	else{
		if(is.null(spotConfig$report.aIndex)){spotConfig$report.aIndex=1}
		if(is.null(spotConfig$report.bIndex)){spotConfig$report.bIndex=2}
		plotFn(spotConfig$report.aIndex,spotConfig$report.bIndex);
	}	
	return(spotConfig)
}


###################################################################################################
#' Model Contour Plot -  Report Function
#' 
#' Function to generate a contour plot of the predicted meta model.
#'
#' This report function uses the parameter spotConfig$seq.modelFit to plot the predicted model.
#' If spotConfig$seq.modelFit is NULL, the model is generated, based on spotConfig$seq.predictionModel.func.
#' It is not recommended to use this function at the end of an "auto" run of SPOT, make sure to save results first. 
#' By default, twiddler will be used to let the user specify which of the parameters should be varied in the plot.
#' Values not varied in the graph are fixed to their "best" value according to the current Results.
#'
#' @param spotConfig the configuration list of all spot parameters. \cr
#'				The parameter spotConfig$report.interactive=TRUE will be set as default if not contained in the list.
#'				That means, by default the user will be asked to specify which parameters will be varied when the report is started. This is done in a small twiddler gui. 
#'				If the user wants to specify which parameters should be plotted against each other, before starting the report, he can 
#'				set the parameter spotConfig$report.aIndex and spotConfig$report.bIndex. They should be two different integer numbers. They will only be used if 
#'				spotConfig$report.interactive is FALSE. By default they will be set to 1 and 2, so the first two parameters in the ROI will be plotted.
#' @export
###################################################################################################
spotReportContour <- function(spotConfig) {	
	#set unset defaults
	if(is.null(spotConfig$report.interactive)){spotConfig$report.interactive=TRUE}
	if(is.null(spotConfig$report.main)){spotConfig$report.main="predicted Model"}
	#load packages
	spotInstAndLoadPackages("twiddler")		
	spotWriteLines(spotConfig$io.verbosity,2,"  Entering spotReportDefault");	
	rawB <- spotGetRawDataMatrixB(spotConfig);
	spotPrint(spotConfig$io.verbosity,1,summary(rawB));
	mergedData <- spotPrepareData(spotConfig)
	mergedB <- spotGetMergedDataMatrixB(mergedData, spotConfig);	
	spotConfig=spotWriteBest(mergedData, spotConfig);
	C1=spotConfig$alg.currentBest[nrow(spotConfig$alg.currentBest),]
	spotWriteLines(spotConfig$io.verbosity,1," ");
	spotPrint(spotConfig$io.verbosity,1,paste("Best solution found with ",nrow(rawB)," evaluations:",sep=""));
	spotPrint(spotConfig$io.verbosity,1,C1);	
	a=1;
	b=2;
	plotFn <- function(aIndex, bIndex) {
		if((aIndex==bIndex)){#here: add more errors
			warning("Plot index problem: both chosen indices are the same") 
		}
		xlim = c(spotConfig$alg.roi$lower[aIndex],spotConfig$alg.roi$upper[aIndex]);
		ylim = c(spotConfig$alg.roi$lower[bIndex],spotConfig$alg.roi$upper[bIndex]);  

		fn <- function(a,b){
			x<-matrix(t(C1[2:(1+nrow(spotConfig$alg.roi))]),nrow(spotConfig$alg.roi),length(a))
			x<-t(x)
			x[,aIndex]=a
			x[,bIndex]=b
			colnames(x) <- row.names(spotConfig$alg.aroi);	
			if(is.null(spotConfig$seq.modelFit)){
				spotConfig1 <- eval(call(spotConfig$seq.predictionModel.func
                                        , rawB
                                        , mergedB
                                        , as.data.frame(x)
                                         ,spotConfig));
			}else{
				spotConfig1 <- eval(call(spotConfig$seq.predictionModel.func
                                , NULL 
								, NULL
								, as.data.frame(x)
								, spotConfig
                                , spotConfig$seq.modelFit #external fit is used, model is only evaluated not build
								));
			}
			ldY <-  spotConfig1$seq.largeDesignY[[1]];
		}
		main = spotConfig$report.main
		xsteps = 100
		ysteps = xsteps
		x <- seq(xlim[1], xlim[2], length = xsteps)
		y <- seq(ylim[1], ylim[2], length = ysteps) 
		z <- outer(x, y, fn)	
		####		
		zlim <- range(z)
		zlen <- zlim[2] - zlim[1] + 1
		filled.contour(x, y, z, color.palette=heat.colors,
			xlab=row.names(spotConfig$alg.roi)[aIndex],
			ylab=row.names(spotConfig$alg.roi)[bIndex],
			plot.axes = { points(C1[row.names(spotConfig$alg.roi)[aIndex]],C1[row.names(spotConfig$alg.roi)[bIndex]],pch=19); axis(1); axis(2) })
	}
	if(spotConfig$report.interactive==TRUE){
		twiddle(plotFn(a, b), eval = FALSE, a = knob(c(1, nrow(spotConfig$alg.roi)), res = 1),
			b = knob(c(1, nrow(spotConfig$alg.roi)), res = 1))
	}
	else{
		if(is.null(spotConfig$report.aIndex)){spotConfig$report.aIndex=1}
		if(is.null(spotConfig$report.bIndex)){spotConfig$report.bIndex=2}	
		plotFn(spotConfig$report.aIndex,spotConfig$report.bIndex);
	}	
	return(spotConfig)
}
