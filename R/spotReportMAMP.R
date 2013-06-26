###################################################################################################
#' MAMP Model Report
#' 
#' This function does a random effects model analysis of a SPOT run, using the lme4 package. It is supposed to be used
#' for the case of Multiple Algorithms Multiple Problems. The SPOT demo 22 provides a simple example for mixed model analysis.
#' Call for the demo is: \code{demo(spotDemo22MixedModelMAMP,ask=F)}.
#'
#' @param spotConfig the configuration list of all spot parameters
#' @return list spotConfig with changed values
#' @seealso  \code{\link{spotReportSAMP}}  \code{\link{spotStepReport}} 
#' @export
###################################################################################################
spotReportMAMP <- function(spotConfig) {	
	spotWriteLines(spotConfig$io.verbosity,2,"  Entering spotReportSAMP");	
	rawB <- spotGetRawDataMatrixB(spotConfig);
	spotPrint(spotConfig$io.verbosity,1,summary(rawB));
	mergedData <- spotPrepareData(spotConfig)	
	mergedB <- spotGetMergedDataMatrixB(mergedData, spotConfig);	
	spotConfig=spotWriteBest(mergedData, spotConfig);
	C1=spotConfig$alg.currentBest[nrow(spotConfig$alg.currentBest),]
	#cat(sprintf("\n Best solution found with %d evaluations:\n",nrow(rawB)));
	spotWriteLines(spotConfig$io.verbosity,1," ");
	spotPrint(spotConfig$io.verbosity,1,paste("Best solution found with ",nrow(rawB)," evaluations:",sep=""));
	spotPrint(spotConfig$io.verbosity,1,C1[1,]);
	
	######
	spotInstAndLoadPackages(c("lme4","ggplot2","lattice"))
	############################################################################	
	pNames <- row.names(spotConfig$alg.roi); # names of fixed factors
	yNames <- spotConfig$alg.resultColumn # names of observations
	rawB <- spotConfig$alg.currentResult

	###
	#browser()
	paramString <- paste(pNames,collapse=",") 
	if(any(rawB$Y < 0))	
		yLog= log(rawB$Y-min(rawB$Y)+1)
	else
		yLog= log(rawB$Y)
	mamp.df <- data.frame(cbind(y=rawB$Y, yLog=yLog, algSeed = rawB$SEED, fSeed=rawB$PINST))
	mamp.df[pNames] <- rawB[pNames]
	mamp.df$algSeed <- factor(mamp.df$algSeed)
	mamp.df$fSeed <- factor(mamp.df$fSeed)
	for(i in 1:length(pNames))
		mamp.df[[pNames[i]]] <- factor(mamp.df[[pNames[i]]])
	#par(mfrow=c(2,1))
	#qqnorm(mamp.df$y,main="(a)")
	#qqline(mamp.df$y)
	#qqnorm(mamp.df$yLog,main="(b) log. transformed")
	#qqline(mamp.df$yLog)
	str(mamp.df)
	  
	#pdf(file="glgxy2.pdf")
	  
	### erster plot
	#library(lattice)
	dev.new()
	print(xyplot(as.formula(paste("y ~", pNames[1]," | fSeed",sep="")), data=mamp.df, 				#Frage: welchen Sinn machen die linien bei ungeordneten faktoren?
	             main="",ylab="y",xlab=pNames[1],
	             panel=function(x, y){
	               m <- sort.list(x)
	               panel.grid(h=-1,v=-1,lty=2)
	               panel.xyplot(x[m], y[m])
	               panel.loess(x[m], y[m], span=2, lty=1)
	               panel.lmline(x[m], y[m], lty=2)
	             }))		
	dev.new()
	print(xyplot(as.formula(paste("yLog ~", pNames[1]," | fSeed",sep="")), data=mamp.df, 				#Frage: welchen Sinn machen die linien bei ungeordneten faktoren?
	             main="",ylab="yLog",xlab=pNames[1],
	             panel=function(x, y){
	               m <- sort.list(x)
	               panel.grid(h=-1,v=-1,lty=2)
	               panel.xyplot(x[m], y[m])
	               panel.loess(x[m], y[m], span=2, lty=1)
	               panel.lmline(x[m], y[m], lty=2)
	             }))	
	# print(xyplot(as.formula(paste("yLog ~ ", pNames[1]," | fSeed * algSeed",sep="")), data=mamp.df, 				#Frage: welchen Sinn machen die linien bei ungeordneten faktoren?
	             # main="",ylab="y",xlab=pNames[1],
	             # panel=function(x, y){
	               # m <- sort.list(x)
	               # panel.grid(h=-1,v=-1,lty=2)
	               # panel.xyplot(x[m], y[m])
	               # panel.loess(x[m], y[m], span=2, lty=1)
	               # panel.lmline(x[m], y[m], lty=2)
	             # }))
	# dev.off()

 	h <- nlevels(mamp.df[[pNames[1]]])												#Frage: wie wird das mit mehreren Faktoren?
 	q <- nlevels(mamp.df$fSeed)
 	r <- nlevels(mamp.df$algSeed)
	
	###
	op <- options(contrasts=c("contr.sum","contr.poly"))
	frml.log <- as.formula(paste("yLog ~", pNames[1]," + (1|fSeed) + (1|fSeed:",pNames[1],")",sep=""))
	frml <- as.formula(paste("y ~", pNames[1]," + (1|fSeed) + (1|fSeed:",pNames[1],")",sep=""))
	mamp.lmer.log <- lmer(frml.log, data=mamp.df)
	mamp.lmer <- lmer(frml, data=mamp.df)
	
	###############################################################################
	### NEW: Check Model Adequacy:
	# checking that e_i all have the same variance:
	dev.new()
	plot(resid(mamp.lmer) ~ fitted(mamp.lmer),main="residual plot")
	abline(h=0)
  
	dev.new()
	# checking the normality of residuals e_ij:
	qqnorm(resid(mamp.lmer), main="Q-Q plot for residuals")
	qqline(resid(mamp.lmer))
	
	dev.new()
	plot(resid(mamp.lmer.log) ~ fitted(mamp.lmer.log),main="residual plot (log.)")
	abline(h=0)
	
	dev.new()
	# checking the normality of residuals e_ij:
	qqnorm(resid(mamp.lmer.log), main="Q-Q plot for residuals (log.)")
	qqline(resid(mamp.lmer.log))
	###############################################################################

	spotPrint(spotConfig$io.verbosity,1,paste("Summary of the mixed model produced by lmer: ",sep=""));
	spotPrint(spotConfig$io.verbosity,1,mamp.lmer)	
	spotPrint(spotConfig$io.verbosity,1,paste("Summary of the mixed model produced by lmer (log.-transformed): ",sep=""));
	spotPrint(spotConfig$io.verbosity,1,mamp.lmer.log)
	
	###
	VC<-VarCorr(mamp.lmer)
	sigma.gamma<-as.numeric(attr(VC[[paste("fSeed:",pNames[1],sep="")]],"stddev"))
	sigma<-as.numeric(attr(VC,"sc"))
	MSAB <- sigma^2 + r * sigma.gamma^2 
	##
	Y.j. <- aggregate(mamp.df$y,list(alg=mamp.df[[pNames[1]]]),mean)
	s <- sqrt(2)*sqrt(MSAB/(q*r))
	T <- qtukey(1-0.05,h,(h-1)*(q-1))/sqrt(2)
	Y.j.$lower <- Y.j.$x - 0.5 * T * s
	Y.j.$upper <- Y.j.$x + 0.5 * T * s	
	###
	VC.log<-VarCorr(mamp.lmer.log)
	sigma.gamma.log<-as.numeric(attr(VC.log[[paste("fSeed:",pNames[1],sep="")]],"stddev"))
	sigma.log<-as.numeric(attr(VC.log,"sc"))
	MSAB <- sigma.log^2 + r * sigma.gamma.log^2 
	##
	Y.j.log <- aggregate(mamp.df$yLog,list(alg=mamp.df[[pNames[1]]]),mean)
	s <- sqrt(2)*sqrt(MSAB/(q*r))
	T <- qtukey(1-0.05,h,(h-1)*(q-1))/sqrt(2)
	Y.j.log$lower <- Y.j.log$x - 0.5 * T * s
	Y.j.log$upper <- Y.j.log$x + 0.5 * T * s
	#if(any(rawB$Y < 0)){	
	#	Y.j.log$lower= exp(Y.j.log$lower)+(min(rawB$Y)-1)
	#	Y.j.log$upper= exp(Y.j.log$upper)+(min(rawB$Y)-1)
	#	Y.j.log$x= exp(Y.j.log$x)+(min(rawB$Y)-1)
	#}else{	
	#	Y.j.log$lower= exp(Y.j.log$lower)
	#	Y.j.log$upper= exp(Y.j.log$upper)
	#	Y.j.log$x= exp(Y.j.log$x)
	#}
	#Y.j.
	###
	r <- length(unique(mamp.df$algSeed))
	intervals <- function(formula,IN,...) {
		prepanel.ci <- function(x, y, lx, ux, subscripts, ...){
			x <- as.numeric(x)
			lx <- as.numeric(lx[subscripts])
			ux <- as.numeric(ux[subscripts])
			list(xlim = range(x, ux, lx, finite = TRUE))
		}
		panel.ci <- function(x, y, lx, ux, subscripts, ar.len=0.06, pch = 16, ...){
			x <- as.numeric(x)
			y <- as.numeric(y)
			lx <- as.numeric(lx[subscripts])
			ux <- as.numeric(ux[subscripts])
			panel.abline(h = unique(y), col = "grey")
			panel.arrows(lx, y, ux, y, col = 'black',
						 length = ar.len, ##unit = "native",
						 angle = 90, code = 3)
			panel.xyplot(x, y, pch = pch, ...)
		}
		dotplot(formula,data=IN,
			  lx = IN[["lower"]], ux = IN[["upper"]],
			  prepanel = prepanel.ci,
			  panel = panel.ci, ...)
	}  
	# pdf(file="glg2intLog.pdf")  
	### abschließender plot:
	dev.new()
	print(intervals(alg~x, Y.j.,xlab="y",ylab=pNames[1]))
	dev.new()
	print(intervals(alg~x, Y.j.log,xlab="yLog",ylab=pNames[1]))	
	spotConfig
}
