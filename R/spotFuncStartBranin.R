
###################################################################################################
#' noisy Branin function
#'
#' This is the noisy implementation of the Branin function used by some SPOT demos
#'
#' @param x	two dimensional vector that will be evauluated by the branin function
#' @param noise	this number is multiplied with \code{rnorm(1)}, the result is added to the function value.
#'
#' @return number \code{y} \cr
#' - \code{y} is the function value of the corresponding vector \code{x}
#'
#' @references  \code{\link{SPOT}} \code{\link{spot}} \code{\link{demo}} \code{\link{spotFuncStartBranin}}
#' \code{\link{spotFuncStartBranin}}  \code{\link{rnorm}} \code{\link{spotAlgEsF}}
###################################################################################################
spotNoisyBraninFunction <- function (x, noise=0.0) {
	x1 <- x[1] 
	x2 <- x[2] 
	(x2 - 5.1/(4 * pi^2) * (x1^2) + 5/pi * x1 - 6)^2 + 10 * (1 - 1/(8 * pi)) * cos(x1) + 10 + noise*rnorm(1)
}


###################################################################################################
#' Branin function call for SPOT
#'
#' SPOT uses this function for some demos to call the \code{\link{spotNoisyBraninFunction}} function 
#' 
#' @param pdFile name of the apd file
#' @param desFileName name of the des file
#' @param resFileName name of the res file
#' @references  \code{\link{SPOT}} \code{\link{spot}} \code{\link{demo}} \code{\link{optim}}
#' \code{\link{spotNoisyBraninFunction}}
###################################################################################################

spotFuncStartBranin <- function(pdFile, desFileName, resFileName){
	writeLines(paste("Loading design file data from::", desFileName), con=stderr());
	writeLines("ES run...", con=stderr());
	print(pdFile)
	noise<-NULL
	f<-NULL
	n<-NULL
	## read default problem design
	source(pdFile,local=TRUE)
	
	## read doe/dace etc settings:
	print(desFileName)
	des <- read.table(desFileName
			, sep=" "
			, header = TRUE
	);
	print(summary(des));
	
	##  VARX1 VARX2 REPEATS SEED
	config<-nrow(des);
	print(config);
	attach(des)
	
	for (k in 1:config){
		for (i in 1:des$REPEATS[k]){
			##
			if (exists("VARX1")){
				x1 <- des$VARX1[k]
			}
			if (exists("VARX2")){
				x2 <- des$VARX2[k]
			}
			conf <- k
			if (exists("CONFIG")){
				conf <- des$CONFIG[k]
			}
			if (exists("STEP")){
				step <- des$STEP[k]
			}
			seed <- des$SEED[k]+i-1			
			print(c("Config:",k ," Repeat:",i))
			y <- spotNoisyBraninFunction(c(x1,x2), noise)
			print(y)
			res <- NULL
			res <- list(Y=y,					
					VARX1=x1,
					VARX2=x2,
					Function=f,					
					DIM=n,
					STEP=step,
					SEED=seed,
					CONFIG=conf
			)
			res <-data.frame(res)
			colNames = TRUE
			if (file.exists(resFileName)){
				colNames = FALSE
			}
			
			## quote = false is required for JAVA
			write.table(res
					, file = resFileName
					, row.names = FALSE
					, col.names = colNames
					, sep = " "              
					, append = !colNames
					, quote = FALSE
			);			
		}			
	}	
	detach(des)
}


