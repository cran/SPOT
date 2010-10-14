
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
#' @param spotConfig Contains the list of spot configurations, results of the algorithm can be passed to this list instead of the .res file.
#'		  spotConfig defaults to "NA", and will only be passed to the Algorithm if spotConfig$spot.fileMode=FALSE. See also: \code{\link{spotGetOptions}}
#'			Items used are: \cr \cr
#'			alg.currentDesign: data frame holding the design points that will be evaluated \cr
#'			io.apdFileName: name of the apd file \cr
#'			io.desFileName: name of the des file \cr
#'			io.resFileName: name of the res file, for logging results (if spotConfig$spot.fileMode==TRUE)\cr
#'			spot.fileMode: boolean, if selected with true the results will also be written to the res file, otherwise it will only be saved in the spotConfig returned by this function\cr
#' @return this function returns the \code{spotConfig} list with the results in spotConfig$alg.currentResult
#' @references  \code{\link{SPOT}} \code{\link{spot}} \code{\link{demo}} \code{\link{optim}}
#' \code{\link{spotNoisySphereFunction}}
###################################################################################################
spotFuncStartSphere <- function(spotConfig){
	pdFile=spotConfig$io.apdFileName;
	resFileName=spotConfig$io.resFileName;	
	desFileName=spotConfig$io.desFileName;	
	
	if (spotConfig$spot.fileMode){ ##Check if spotConfig was passed to the algorithm, if yes the spot.fileMode is chosen with False wich means results have to be passed to spotConfig and not to res file.
		writeLines(paste("Loading design file data from::",  desFileName), con=stderr());
		## read doe/dace etc settings:
		des <- read.table(desFileName, sep=" ", header = TRUE);	
	}else{
		des <- spotConfig$alg.currentDesign; ##The if/else should not be necessary anymore, since des will always be written into the spotConfig
	}	
	noise<-NULL
	f<-NULL
	n<-NULL
	## read default problem design
	source(pdFile,local=TRUE)
	## read doe/dace etc settings:
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
			if (spotConfig$spot.fileMode){ ##Log the result in the .res file, only if user didnt set fileMode==FALSE
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
			spotConfig$alg.currentResult=rbind(spotConfig$alg.currentResult,res);					
		}			
	}	
	detach(des)
	return(spotConfig)
}


