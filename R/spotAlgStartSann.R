###################################################################################################
#' Branin function
#'
#' This is the implementation of the Branin function used by some SPOT demos
#'
#' This Branin function does not use noise
#'
#' @param x	two dimensional vector that will be evaluated by the Branin function
#' 
#' @return number \code{y} \cr
#' - \code{y} is the value of the corresponding \code{x} vector
#' 
#' @references  \code{\link{SPOT}} \code{\link{spot}} \code{\link{demo}} \code{\link{spotNoisyBraninFunction}}
#' \code{\link{spotFuncStartBranin}} 
###################################################################################################
spotFuncStartBraninSann <- function (x) {
	x1 <- x[1] 
	x2 <- x[2] 
	(x2 - 5.1/(4 * pi^2) * (x1^2) + 5/pi * x1 - 6)^2 + 10 * (1 - 1/(8 * pi)) * cos(x1) + 10 
}

###################################################################################################
#' SANN function call for SPOT
#'
#' SPOT uses this function for some demos to call the \code{\link{optim}} function with the SANN
#' method, which means Simulated Annealing. The SANN uses \code{\link{spotFuncStartBranin}} as 
#' a target function.
#'
#' @param io.apdFileName name of the apd file
#' @param io.desFileName name of the des file
#' @param io.resFileName name of the res file
#' @references  \code{\link{SPOT}} \code{\link{spot}} \code{\link{demo}} \code{\link{optim}}
#' \code{\link{spotFuncStartBranin}}
###################################################################################################
#seed    	random seed (e.g. 12345)
#maxit		stopping criterion, maximum number of iterations
#parscale	scaling vector
#tmax  		number of function evaluations at each temperature 
#temp		starting temperature of the SANN algorithm
spotAlgStartSann <- function(io.apdFileName, io.desFileName, io.resFileName){
	writeLines(paste("Loading design file data from::",  io.desFileName), con=stderr());
	x0<-NULL
	maxit<-NULL
	parscale<-NULL
	f<-NULL
	n<-NULL
	source( io.apdFileName,local=TRUE)
	des <- read.table( io.desFileName, sep=" ", header = TRUE);	 
	pNames <- names(des);	
	config<-nrow(des);	
	for (k in 1:config){
		for (i in 1:des$REPEATS[k]){
			if (is.element("TEMP", pNames)){
				temp <- des$TEMP[k]
			}
			if (is.element("TMAX", pNames)){
				tmax <- round(des$TMAX[k])
			}
			conf <- k
			if (is.element("CONFIG", pNames)){
				conf <- des$CONFIG[k]
			}
			spotStep<-NA
			if (is.element("STEP", pNames)){
				spotStep <- des$STEP[k]
			}			
			seed <- des$SEED[k]+i-1	
			set.seed(seed)
			y <- optim(x0, spotFuncStartBraninSann, method="SANN",
					control=list(maxit=maxit, temp=temp, tmax=tmax, parscale=parscale))	
			res <- NULL
			res <- list(Y=y$value, TEMP=temp, TMAX=tmax, FUNCTION=f, DIM=n, SEED=seed, CONFIG=conf)
			if (is.element("STEP", pNames)){
				res=c(res,STEP=spotStep)
			} 
			res <-data.frame(res)
			colNames = TRUE
			if (file.exists(io.resFileName)){
				colNames = FALSE
			}
			write.table(res, file = io.resFileName, row.names = FALSE, 
					col.names = colNames, sep = " ", append = !colNames, quote = FALSE);			
			colNames = FALSE
		}
	}	
}