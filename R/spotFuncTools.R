###################################################################################
#' Calculate Noise
#'
#' Calculates Noise for a given Function-Value (y) dependant on the
#' noise intensity (noise) and the way of calculating noise (noise.type)
#'
#' @param y the function value where noise will be added to
#' @param noise noise magnitude (absolute or weighted)
#' @param noise.type type of noise can either be "weighted" or "constant"
#' @param spot.noise.minimum.at.value Voice magnitude at minimum
#' 
#' @return numeric \cr
#' holding the noise Value
#' @export
#' @keywords internal
####################################################################################
spotCalcNoise <- function(y, noise=0.0, noise.type="weighted", spot.noise.minimum.at.value=0.0){
	
	noiseValue <- 0	
	if (noise == 0)return (noiseValue)

	if (noise.type=="weighted"){
		noiseValue <- (y - spot.noise.minimum.at.value)*noise*rnorm(1)/100
	}else if (noise.type=="constant"){
		noiseValue <- noise*rnorm(1)
	}
	
	noiseValue
}

###################################################################################################
#' Interface for Target Functions
#'
#' SPOT uses this function to call functions passed to \code{\link{spotOptim}} or \code{\link{spot}} like they would be passed to optim().
#' That means, it will be used whenever an actual function is passed instead of a string. When a string is passed
#' the string itself will contain the interface to use.
#' This function is needed as an interface, to ensure the right information
#' are passed from SPOT to the target function. It can handle single and multi criteria target functions, e.g. functions that return numerics or vectors of numerics.
#'
#' @param spotConfig Contains the list of spot configurations, results of the algorithm can be passed to this list instead of the .res file.
#'		  spotConfig defaults to "NA", and will only be passed to the Algorithm if spotConfig$spot.fileMode=FALSE. See also: \code{\link{spotGetOptions}}
#'			Items used are: \cr \cr
#'			alg.currentDesign: data frame holding the design points that will be evaluated \cr
#'			io.apdFileName: name of the apd file \cr
#'			io.desFileName: name of the des file \cr
#'			io.resFileName: name of the res file, for logging results (if spotConfig$spot.fileMode==TRUE)\cr
#'			spot.fileMode: boolean, if selected with true the results will also be written to the res file, otherwise it will only be saved in the spotConfig returned by this function\cr
#'			spotConfig$alg.tar.func target function of type y=f(x,...)
#' @param ... additional parameters to be passed on to target function: spotConfig$alg.tar.func
#' @return this function returns the \code{spotConfig} list with the results in spotConfig$alg.currentResult
#' @seealso  \code{\link{SPOT}} \code{\link{spot}} \code{\link{demo}} \code{\link{optim}}
#' \code{\link{spotOptim}}
#' @export
###################################################################################################
#TODO: Frage: Wie sollen initial defaults gehandelt werden. Voorallem: Noise, OCBA, init/seq Repeats.
spotOptimInterface <- function(spotConfig,...){
	if(exists(as.character(substitute(.Random.seed))))
		SAVESEED<-.Random.seed
	else
		SAVESEED=NULL
	
	pdFile=spotConfig$io.apdFileName
	desFileName=spotConfig$io.desFileName		
	if (spotConfig$spot.fileMode){ 
		spotWriteLines(spotConfig$io.verbosity,1,paste("Loading design file data from::",  desFileName), con=stderr())
		## read doe/dace etc settings:
		des <- read.table(desFileName, sep=" ", header = TRUE)
	}else{
		des <- spotConfig$alg.currentDesign; 
	}
	spotPrint(spotConfig$io.verbosity,1,summary(des))
	spotWriteLines(spotConfig$io.verbosity,1,"spotOptimInterface...", con=stderr())
	f<-"UserSuppliedFunction"
	## read problem design file
	if(file.exists(pdFile)){
		source(pdFile,local=TRUE)
	}
	config<-nrow(des)
	spotPrint(spotConfig$io.verbosity,1,config)
	for (k in 1:config){
		for (i in 1:des$REPEATS[k]){
			dimcounter <- 1
			x <- NULL			
			while (!is.null(des[[paste("VARX", dimcounter, sep="")]])){
				name <- paste("VARX", dimcounter, sep="")
				varHelper  <- des[name]
				x <- cbind(x,varHelper[k,1])
				dimcounter <- dimcounter+1
			}
			
			x <- t(x)
			n <- dimcounter - 1
			conf <- k
			if (!is.null(des$CONFIG)){
				conf <- des$CONFIG[k]
			}
			if (!is.null(des$STEP)){
				step <- des$STEP[k]
			}
			if(!is.na(spotConfig$alg.seed)){ #only use seed if seed is desired (not for deterministic target functions)
				seed <- des$SEED[k]+i-1	
				set.seed(seed)	
			}
			else{seed=NA}
			spotPrint(spotConfig$io.verbosity,1,c("Config:",k ," Repeat:",i))
			if(!is.function(spotConfig$alg.tar.func)){stop("spotConfig$alg.tar.func is not a function. \n Please specify a function for this variable if you use spotOptimInterface.\n Else use your own custom interface")}
			y <-  spotConfig$alg.tar.func(x,...)#spotConfig$alg.func.tar(x)#, noise=noise, noise.type=noise.type, spot.noise.minimum.at.value=spot.noise.minimum.at.value)
			#No external noise is used, the user has to take care of that himself	
			spotPrint(spotConfig$io.verbosity,1,y)
			res <- NULL
			#browser()
			res <- data.frame(Function=f,XDIM=n,YDIM=length(y),STEP=step,SEED=seed,CONFIG=conf)
			for (counter in 1:(dimcounter-1)){
				res[paste("VARX", counter, sep="")] <- x[counter]
			}	
			if(length(y)>1){
				for (counter in 1:length(y)){
					if(length(spotConfig$alg.resultColumn)>1){
						res[spotConfig$alg.resultColumn[counter]] <- y[counter]
					}
					else{res[paste(spotConfig$alg.resultColumn,".", counter, sep="")] <- y[counter]}
				}	#TODO: Wenn mehr als 9 result columns: 01, 02, 03
				#now reset result columns for later reading.
				spotConfig$alg.resultColumn <- setdiff(names(res),c(c("Function","XDIM","YDIM","STEP","SEED","CONFIG"),paste("VARX", 1:n, sep="")))
			}
			else{res[spotConfig$alg.resultColumn]<-y}
			if (spotConfig$spot.fileMode){ ##Log the result in the .res file, only if user didnt set fileMode==FALSE
				colNames = TRUE
				if (file.exists(spotConfig$io.resFileName)){
					colNames = FALSE
				}				
				## quote = false is required for JAVA
				write.table(res
						, file = spotConfig$io.resFileName
						, row.names = FALSE
						, col.names = colNames
						, sep = " "    
						, append = !colNames
						, quote = FALSE)	
			}
			spotConfig$alg.currentResult=rbind(spotConfig$alg.currentResult,res)						
		}			
	}	
	if(!is.null(SAVESEED))
		assign(".Random.seed", SAVESEED, envir=globalenv())
	spotConfig
}
