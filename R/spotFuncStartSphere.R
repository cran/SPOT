
###################################################################################################
#' 3-dim sphere function
#'
#' This is the noisy implementation of the Sphere function used by some SPOT demos
#'
#' @param x	two dimensional vector that will be evauluated by the sphere function
#' @param noise	this number is added to the function value y as noise
#'
#' @return number \code{y} \cr
#' - \code{y} is the value of the corresponding \code{x} vector
#'
#' @references  \code{\link{SPOT}} \code{\link{spot}} \code{\link{demo}} \code{\link{spotFuncStartSphere}}
#' \code{\link{spotFuncStartSphere}} 
###################################################################################################
spotNoisySphereFunction <- function (x, noise) {
	x1 <- x[1] 
	x2 <- x[2] 
	x3 <- x[3]
	return(x1*x1+x2*x2+x3*x3+noise*rnorm(1))	
}


###################################################################################################
#' Sphere function call for SPOT
#'
#' SPOT uses this function for some demos to call the \code{\link{spotNoisySphereFunction}} function 
#' 
#' @param pdFile name of the apd file
#' @param desFileName name of the des file
#' @param resFileName name of the res file
#' @references  \code{\link{SPOT}} \code{\link{spot}} \code{\link{demo}} \code{\link{optim}}
#' \code{\link{spotNoisySphereFunction}}
###################################################################################################

spotFuncStartSphere <- function(pdFile, desFileName, resFileName){
	writeLines(paste("Loading design file data from::", desFileName), con=stderr());	
	noise<-NULL
	f<-NULL
	n<-NULL
	## read default problem design
	source(pdFile,local=TRUE)
	## read doe/dace etc settings:
	des <- read.table(desFileName
			, sep=" "
			, header = TRUE
	);	
	##  VARX1 VARX2 VARX3 REPEATS SEED
	config<-nrow(des);	
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
			if (exists("VARX3")){
				x3 <- des$VARX3[k]
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
			y <- spotNoisySphereFunction(c(x1,x2,x3), noise)
			print(y)
			res <- NULL
			res <- list(Y=y,					
					VARX1=x1,
					VARX2=x2,
					VARX3=x3,
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


