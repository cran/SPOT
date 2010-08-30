###################################################################################################
#' SANN function call for SPOT
#'
#' SPOT uses this function for some demos to call the \code{\link{optim}} function with the SANN
#' method, which means Simulated Annealing. The SANN uses \code{\link{spotFuncStartBranin}} as 
#' a target function.
#' This function is needed as an interface, to ensure the right information
#' are passed from SPOT to the target algorithm(e.g. the SANN) and vice versa.
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
			y <- optim(x0, spotNoisyBraninFunction, method="SANN",
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