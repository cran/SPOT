###################################################################################################
#' noisy Mexican Hat function
#'
#' This is the noisy implementation of the Mexican Hat function used by some SPOT demos
#'
#' @param x	two dimensional vector that will be evauluated by the branin function
#'
#' @return number \code{y} \cr
#' - \code{y} is the function value of the corresponding vector \code{x}
#'
#' @references  \code{\link{SPOT}} \code{\link{spot}} \code{\link{demo}} \code{\link{spotFuncStartMexicanHat}}
#' \code{\link{spotFuncStartMexicanHat}}  \code{\link{rnorm}} \code{\link{spotAlgEsF}}
###################################################################################################
spotMexicanHatFunction <- function (x) {	
	x1 <- x[1] 
	x2 <- x[2] 
	
	distance <- sqrt(x1^2 + x2^2)
	if (distance == 0) # stetig ergaenzen
		{y<-1}
	else
		{y<- sin(distance) / distance}
		
	return(y)       
}

###################################################################################################
#' Mexican Hat function call for SPOT
#'
#' SPOT uses this function for some demos to call the \code{\link{spotMexicanHatFunction}} function 
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
#' \code{\link{spotMexicanHatFunction}}
###################################################################################################

spotFuncStartMexicanHat <- function(spotConfig){
	pdFile=spotConfig$io.apdFileName;
	resFileName=spotConfig$io.resFileName;	
	desFileName=spotConfig$io.desFileName;	
	
	if (spotConfig$spot.fileMode){ ##Check if spotConfig was passed to the algorithm, if yes the spot.fileMode is chosen with False wich means results have to be passed to spotConfig and not to res file.
		spotWriteLines(spotConfig$io.verbosity,1,paste("Loading design file data from::",  desFileName), con=stderr());
		## read doe/dace etc settings:
		des <- read.table(desFileName, sep=" ", header = TRUE);	
	}else{
		des <- spotConfig$alg.currentDesign; ##The if/else should not be necessary anymore, since des will always be written into the spotConfig
	}
	spotPrint(spotConfig$io.verbosity,1,summary(des));
	spotWriteLines(spotConfig$io.verbosity,1,"Mexican Hat run...", con=stderr());
	spotPrint(spotConfig$io.verbosity,1,pdFile)
	#default Values that can be changed with apd file
    noise<-spotConfig$spot.noise;
	noise.type <- spotConfig$spot.noise.type;
	spot.noise.minimum.at.value <- spotConfig$spot.noise.minimum.at.value;
	f<-"MexicanHat"
	n<-2;
	## read problem design file
	if(file.exists(pdFile)){
		source(pdFile,local=TRUE)
	}
	##  VARX1 VARX2 REPEATS SEED
	config<-nrow(des);
	spotPrint(spotConfig$io.verbosity,1,config);
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
			spotPrint(spotConfig$io.verbosity,1,c("Config:",k ," Repeat:",i))
			y <- spotMexicanHatFunction(c(x1,x2))
			## add noise
			y <- y + spotCalcNoise(y, noise=noise, noise.type=noise.type, spot.noise.minimum.at.value=spot.noise.minimum.at.value);
			
			spotPrint(spotConfig$io.verbosity,1,y)
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
			spotConfig$alg.currentResult=rbind(spotConfig$alg.currentResult,res);	#always log the results in spotConfig				
		}			
	}	
	detach(des)
	return(spotConfig);
}
