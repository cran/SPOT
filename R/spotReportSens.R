###################################################################################################
#'  Sensivity Report Help Function
#' 
#' Helper function for the sensivity report
#'
#' @param B best solution column
#' @param fit random forest fit
#' @param roi internal parameter for the initial region of interest, or: \code{spotConfig$alg.roi}
#' @param nsens number of parameters estimated with sensitivity
#' @references  \code{\link{SPOT}} \code{\link{spot}} \code{\link{spotStepReport}} \code{\link{spotReportSens}} 
################################################################################################### 
spotReportSensY <- function(B,fit,roi,nsens) {	
  	Y <- NULL;
  	for (k in 1:length(B)) {
    	BV <- B;
    	BV[,k] <- seq(roi[k,"low"],roi[k,"high"],length.out=nsens)
      Y <- data.frame(cbind(Y,predict(fit,BV)))
      names(Y)[length(Y)] <- names(B)[k];
    }	
    Y;
}

###################################################################################################
#'  Sensitivity Report Function
#' 
#' Function to generate a report with sensitivity plot.
#'
#' The sensitivity curves are based on a metamodel which is a random forest with 100 trees 
#' fitted to the result points from RES-file. The plot contains: 
#'    x-axis:   ROI for each parameter normalized to [-1,1]
#'    y-axis: 
#'
#' @param spotConfig the configuration list of all spot parameters
#' @references  \code{\link{SPOT}} \code{\link{spot}} \code{\link{spotStepReport}} 
################################################################################################### 
spotReportSens <- function(spotConfig) {		
	spotWriteLines(spotConfig$io.verbosity,2,"  Entering spotReportSens");
	spotInstAndLoadPackages("randomForest")	
	rawB <- spotGetRawDataMatrixB(spotConfig);
	print(summary(rawB));
	mergedData <- spotPrepareData(spotConfig)
	mergedB <- spotGetMergedDataMatrixB(mergedData, spotConfig);	
	C1 = spotWriteBest(mergedData, spotConfig);
	C1 = C1[C1$COUNT==max(C1$COUNT),];    # choose only among the solutions with highest repeat
	xNames <- setdiff(names(rawB),c(spotConfig$alg.resultColumn,"y"))
	B <- NULL 
	nsens=20             # number of points along the normalized ROI range
	for (i in 1:nsens) {
	   # replicate best solution (row 1), but cut away 1st column (Y):   
	   B <- rbind(B,data.frame(C1[1,xNames]));
	}
	fit <- randomForest(rawB[,xNames], rawB$y, ntree=100)			
#	fit <- rpart(y ~ ., data= rawB)

  rwb <- cbind(spotConfig$alg.roi,t(B[1,]));      # rwb: roi with 'BEST' column
  names(rwb)[length(rwb)] <- "BEST";
  Y <- spotReportSensY(B,fit,spotConfig$alg.roi,nsens);
  # scale each ROI to the normalized ROI range [-1,+1]:
  X=seq(-1,1,length.out=nsens)
  matplot(X,Y,type="l",lwd=rep(2,ncol(Y)),col=1:ncol(Y),xlab="normalized ROI") #,sub=palette(rainbow(6)));
  # XP is the location of the SPOT best point for each parameter in the normalized ROI range
  XP = (rwb$BEST-rwb$low)/(rwb$high-rwb$low)*2-1; 
  XP=rbind(XP,XP);   # matpoints needs at least two rows to plot each column in a different color
  YP = min(Y);  YP=rbind(YP,YP);
  matpoints(XP,YP,pch=rep(21,ncol(Y)),bg=1:ncol(Y),cex=2)
  legend("topleft",legend=names(Y),lwd=rep(2,ncol(Y)),lty=1:ncol(Y),col=1:ncol(Y),text.col=1:ncol(Y));
  #palette("default")
  cat(sprintf("\n Sensitivity plot for this ROI:\n"));
  print(rwb);
	cat(sprintf("\n Best solution found with %d evaluations:\n",nrow(rawB)));
	print(C1[1,]);
	spotWriteLines(spotConfig$io.verbosity,2,"\n Leaving spotReportSens");
}

