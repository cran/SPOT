###################################################################################################
#' SAMP Model Report
#' 
#' This function does a random effects model analysis of a SPOT run, using the lme4 package. It is supposed to be used
#' for the case of Single Algorithm Multiple Problems. The SPOT demo 21 provides a simple example for mixed model analysis.
#' Call for the demo is: \code{demo(spotDemo21MixedModelSAMP,ask=F)}.
#'
#' @param spotConfig the configuration list of all spot parameters
#' @return list spotConfig with changed values
#' @seealso  \code{\link{spotReportMAMP}}  \code{\link{spotStepReport}} 
#' @export
###################################################################################################
spotReportSAMP <- function(spotConfig) {	
	spotWriteLines(spotConfig$io.verbosity,2,"  Entering spotReportSAMP");	
	rawB <- spotGetRawDataMatrixB(spotConfig);
	spotPrint(spotConfig$io.verbosity,1,summary(rawB));
	mergedData <- spotPrepareData(spotConfig)	
	mergedB <- spotGetMergedDataMatrixB(mergedData, spotConfig);	
	spotConfig=spotWriteBest(mergedData, spotConfig);
	C1=spotConfig$alg.currentBest[nrow(spotConfig$alg.currentBest),]
	## this is superfluous, since this is SAMP SINGLE algorithm
	#spotWriteLines(spotConfig$io.verbosity,1," ");
	#spotPrint(spotConfig$io.verbosity,1,paste("Best solution found with ",nrow(rawB)," evaluations:",sep=""));
	#spotPrint(spotConfig$io.verbosity,1,C1[1,]);
	
	######
	spotInstAndLoadPackages(c("lme4","ggplot2"))	#TODO in suggest		
	pNames <- row.names(spotConfig$alg.roi); # names of fixed factors
	yNames <- spotConfig$alg.resultColumn # names of observations
	rawB <- spotConfig$alg.currentResult
	###
	#paramString <- paste(pNames,collapse=",") 
    ###
	samp.df <- data.frame(cbind(y=rawB$Y, yLog=log(rawB$Y), algSeed = rawB$SEED, fSeed=rawB$PINST))
	samp.df$algSeed <- factor(samp.df$algSeed)
	samp.df$fSeed <- factor(samp.df$fSeed)
	#pdf(file="qq1.pdf")
	par(mfrow=c(2,1))
	qqnorm(samp.df$y,main="(a)")
	qqline(samp.df$y)
	qqnorm(samp.df$yLog,main="(b) log. transformed")
	qqline(samp.df$yLog)
	#dev.off()
	# 	samp.aov <- aov(yLog ~ fSeed, data=samp.df)
	# 	(M1 <- anova(samp.aov))
	# 	(MSA <- M1[1,3])
	# 	(MSE <- M1[2,3])
	# 	r <-length(unique(samp.df$algSeed))
	# 	q <- nlevels(samp.df$fSeed)
	#   print("1st variance component (treatment):")
	# 	(var.A <- (MSA - MSE)/(r))
	#   print("2nd variance component (error):")
	# 	(var.E <- MSE)
	# 	var.A + var.E
	# 	coef(samp.aov)[1]
	# 	1-pf(MSA/MSE,q-1,q*(r-1))
	# 	MSA.anova <- MSA
	###	
	samp.lmer <- lmer(yLog~ 1 +(1|fSeed),data=samp.df)
	spotPrint(spotConfig$io.verbosity,1,paste("Summary of the mixed model: ",sep=""));
	spotPrint(spotConfig$io.verbosity,1,samp.lmer)
	####
	VC <- VarCorr(samp.lmer)
	sigma.tau <- as.numeric(attr(VC$fSeed,"stddev"))
	sigma <- as.numeric(attr(VC,"sc"))
	q <- nlevels(samp.df$fSeed)
	r <- length(unique(samp.df$algSeed))
	MSA <- sigma^2+r*sigma.tau^2
	MSE <- sigma^2
	### diesen wert als p-wert ausgeben:
	pvalue=1-pf(MSA/MSE,q-1,q*(r-1))
	spotPrint(spotConfig$io.verbosity,1,paste("P-value: ",pvalue,sep=""));
	###	
	s <- sqrt(MSA/(q*r))
	Ydotdot <- mean(samp.df$yLog)
	qsr <- qt(1-0.025,r)
	### conf intervall ausgeben:
	confInt=c( exp(Ydotdot - qsr * s), exp(Ydotdot + qsr * s))
	spotPrint(spotConfig$io.verbosity,1,paste("Confidence Interval: ",confInt[1]," to ", confInt[2], sep=""));
	### same analysis based on anova
	# 	s <- sqrt(MSA.anova/(q*r))
	# 	Ydotdot <- mean(samp.df$yLog)
	# 	qsr <- qt(1-0.025,r)
	# 	c( exp(Ydotdot - qsr * s), exp(Ydotdot + qsr * s))
	#### plots  
	# 	ggplot(samp.df, aes(x = y, y = fSeed, colour = algSeed)) +
	# 	  geom_point() + opts(title = "Performance")
	#   pdf(file="gg1Log.pdf")  
	### nebeneinander plotten:
	dev.new()
	plt1 <- ggplot(samp.df, aes_string(x = "yLog", y = "fSeed")) + geom_point() + ggtitle("Performance")
	print(plt1)
	dev.new()
	plt2 <- ggplot(samp.df, aes_string(x = "y", y = "fSeed")) + geom_point() + ggtitle("Performance")
	print(plt2)
	#dev.off()
}
