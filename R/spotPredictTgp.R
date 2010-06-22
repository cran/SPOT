###################################################################################
#' SPOT Predictor Tgp
#'
#' This function implements a  model for prediction, based on Mat's tgp 
#' 
#' @param rawB unmerged data
#' @param mergedB merged data
#' @param lhd new design points which should be predicted
#' @param spotConfig global list of all options, needed to provide data for calling functions
#' 
#' @return data.frame \code{lhd} \cr
#' - \code{lhd} is a sorted (with respect to fitness, i.e., smallest estimated function value) largeDesign. 
#' Best is first  
#'
#' @references \code{\link{SPOT}}
###################################################################################
spotPredictTgp <- function(rawB,mergedB,lhd,spotConfig){
	writeLines("spotPredictTgp started");
	spotInstAndLoadPackages("tgp")
	
	xNames <- setdiff(names(rawB),"y")
	x <- rawB[,xNames]	
	y <- rawB$y
	fit <- btgpllm(X=x,Z=y,XX=lhd,improv=c(1,spotConfig$seq.design.new.size));	
	##pdf(file=spotConfig$io.pdfFileName)
	# plot(fit, main = "btgpllm", as="improv")
	##tgp.trees(fit)
	##sf <- sens( X=x, Z=y, nn.lhs=100, model = btgpllm, verb=0)
	##plot(sf, layout="sens", legendloc="topleft")
	# dev.off()
	lhd<-cbind(fit$improv,lhd)[fit$improv$rank<=spotConfig$seq.design.new.size,]
	lhd<-lhd[,-c(1,2)]	
  	writeLines("spotPredictTgp finished");
	return(lhd)
  	}
