###################################################################################################
#' SANN function call for SPOT
#'
#' SPOT uses this function for some demos to call the \code{\link{optim}} function with the SANN
#' method, which means Simulated Annealing. The SANN uses \code{\link{spotFuncStartBranin}} as 
#' a target function.
#' This function is needed as an interface, to ensure the right information
#' are passed from SPOT to the target algorithm(e.g. the SANN) and vice versa.
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
#' \code{\link{spotFuncStartBranin}}
###################################################################################################
spotAlgStartSann <- function(spotConfig){
	io.apdFileName=spotConfig$io.apdFileName;
	io.desFileName=spotConfig$io.desFileName;
	io.resFileName=spotConfig$io.resFileName;	
	#default Values that can be changed with apd file
	x0<-c(10,10);
	maxit<-250;
	parscale<-c(1,1);
	f<-"Branin0.0";
	n<-2;
	## read problem design file
	if(file.exists(io.apdFileName)){
		source(io.apdFileName,local=TRUE)
	}
	else{
		spotWriteLines(spotConfig$io.verbosity,1,"apd File not found, defaults used");
	}
	if (spotConfig$spot.fileMode){ ##Check if spotConfig was passed to the algorithm, if yes the spot.fileMode is chosen with False wich means results have to be passed to spotConfig and not to res file.
		spotWriteLines(spotConfig$io.verbosity,1,paste("Loading design file data from::",  io.desFileName), con=stderr());
		## read doe/dace etc settings:
		des <- read.table( io.desFileName, sep=" ", header = TRUE);	
	}else{
		des <- spotConfig$alg.currentDesign; ##The if/else should not be necessary anymore, since des will always be written into the spotConfig
	}			
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
			y <- optim(x0, spotBraninFunction, method="SANN",
					control=list(maxit=maxit, temp=temp, tmax=tmax, parscale=parscale))	
			res <- NULL
			res <- list(Y=y$value, TEMP=temp, TMAX=tmax, FUNCTION=f, DIM=n, SEED=seed, CONFIG=conf)
			if (is.element("STEP", pNames)){
				res=c(res,STEP=spotStep)
			} 
			res <-data.frame(res)
			if (spotConfig$spot.fileMode){ ##Log the result in the .res file, only if user didnt set fileMode==FALSE
				colNames = TRUE
				if (file.exists(io.resFileName)){
					colNames = FALSE
				}				
				write.table(res, file = io.resFileName, row.names = FALSE, 
					col.names = colNames, sep = " ", append = !colNames, quote = FALSE);
				colNames = FALSE					
			}
			spotConfig$alg.currentResult=rbind(spotConfig$alg.currentResult,res);#always log the results in spotConfig			
		}
	}	
	return(spotConfig)
}