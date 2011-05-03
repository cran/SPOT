### Feb, 1 2011: spot.noise added
### Jan, 29th 2010: alg.aroi added

###################################################################################################
#' spotOptim: optim like spot interface
#'
#' Besides \code{\link{spot}} this is one of the main interfaces for using the SPOT package. It is build
#' like the \code{\link{optim}} interface.
#' 
#' It is of major importance to understand that spot by default expects to optimize noisy functions. That means, the default settings of spot,
#' which are also used in spotOptim, include repeats of the initial and sequentially created design points. Also, as a default OCBA
#' is used to spread the design points for optimal usage of the function evaluation budget. OCBA will not work when there is no variance in the data.
#' So if the user wants to optimize non-noisy functions, the following settings should be used:\cr
#' \code{control$spot.ocba <- FALSE}\cr
#' \code{control$seq.design.maxRepeats <- 1}\cr
#' \code{control$init.design.repeats <- 1}\cr
#'
#' A call to a noisy function could look like this: \cr
#' \code{objFunction<-function(x){y=(x[1]+2)^2*(x[2]-4)^2+runif(1)}} \cr
#' \code{spotOptim(par=c(1,1),fn<-objFunction,lower=c(-10,-10),upper=c(10,10),method="spotPredictRandomForest",control=list(maxit=50))}
#'
#' A call to a non-noisy function could look like this: \cr
#' \code{objFunction<-function(x){y=(x[1]+2)^2*(x[2]-4)^2}} \cr
#' \code{spotOptim(par=c(1,1),fn<-objFunction,lower=c(-10,-10),upper=c(10,10),method="spotPredictRandomForest",control=list(maxit=50,spot.ocba=F,seq.design.maxRepeats=1,init.design.repeats=1))}
#' 
#' @param par is a point in search intervall (defines dimension)
#' @param fn is the target function (it can also be a string with the name of a spot interface function, like "\code{\link{spotFuncStartBranin}}")
#' @param gr gradient function, not implemented yet
#' @param lower is a vector that defines the lower boundary of search space
#' @param upper is a vector that defines the upper boundary of search space
#' @param method is a string that describes which method is to be used.
#' @param control is a list of additional settings. \code{maxit} is the number of function evaluations,
#' all the other settings will simply be passed to SPOT (see \code{\link{spotGetOptions}} for details) 
#'
#' @return This function returns a list with:\cr
#'	\code{par} parameters of the found solution\cr
#'	\code{value} target function value of the found solution\cr
#	\code{convergence} inidicates successfull completion when \code{0}\cr
#	\code{message}\cr
#	\code{hessian}\cr
#
#' @references  \code{\link{SPOT}} \code{\link{spot}}
#'
###################################################################################################
spotOptim <- function(par=NULL
                      , fn
                      , gr=NULL
                      , lower=-Inf
                      , upper=Inf
                      , method
                      ,control=list()){					  
	if (length(par)==0) stop("dimension of par is null")
	con<-list(maxit=100 #CON: Internal List with defaults for control
             , seed=1
             , spot.fileMode = FALSE);
	con[(namc <- names(control))] <- control;
	control<-con;
	
#	# funktion fn in String wandeln
#	if (class(fn)=="character"){
#		control$alg.func<-fn;
#		control$alg.func.tar<-NA;
#	}
#	else if(is.function(fn))opti{
#		control$alg.func<-"spotOptimInterface"
#		control$alg.func.tar<-fn;
#	}
#	else{
#		stop("The optimization target function is neither a character string, nor a function handle")
#	}
	control$alg.func<-fn; #will  be checked for string or function in spotGetOptions, here it is only passed
	control$seq.predictionModel.func<- method;
	control$auto.loop.nevals<-control$maxit;
	control$spot.seed<- control$seed;


	# transform parameters for SPOT	
	helpFx <- function(x){paste("VARX",x,sep="")}
	rowNames <- helpFx(c(1:length(par)))
        ### now we have VARX1, VARX2 etc as list of colnames (=rownames)
		
	### lower & upper 
	if (length(lower)==1){ lower=rep(lower,length(par))}
	if (length(upper)==1){ upper=rep(upper,length(par))}
	control$alg.roi<-data.frame(low=lower
            ,high=upper
            ,type=rep("FLOAT",length(par))
            ,row.names=rowNames)
	control$alg.aroi<-control$alg.roi;
	#ConfigList<-list(alg.func=AlgInterfaceFunction,
	#		alg.func.tar = AlgTargetFunction,
	#		seq.predictionModel.func= method,
	#		auto.loop.nevals=control$maxit,
	#		spot.seed = control$seed,
	#		spot.fileMode = control$spot.fileMode,
	#		io.verbosity = control$io.verbosity, 			
     #                    init.design.size = control$init.design.size,
      #                   init.design.repeats = control$init.design.repeats,
		#				 init.design.func = control$init.design.func,
         #                seq.design.func = "spotCreateDesignLhd",
          #               spot.ocba = control$spot.ocba,
           #              spot.noise = control$spot.noise,
			#			 spot.noise.type = control$spot.noise.type,
			#			 spot.noise.minimum.at.value = control$spot.noise.minimum.at.value,
             #            seq.ocba.budget = control$seq.ocba.budget,
              #           seq.design.oldBest.size = control$seq.design.oldBest.size,
               #          seq.mlegp.constantMean = control$seq.mlegp.constantMean,
                #         seq.design.size = control$seq.design.size,
                 #        seq.design.new.size = control$seq.design.new.size,
					#	 seq.design.maxRepeats = control$seq.design.maxRepeats,
					#	 seq.predict.subMethod = control$seq.predict.subMethod,
                     #    alg.roi=data.frame(low=lower
                      #    ,high=upper
                       #   ,type=rep("FLOAT",length(par))
                        #  ,row.names=rowNames)
                         #,alg.aroi=data.frame(low=lower
                          #,high=upper
                          #,type=rep("FLOAT",length(par))
                          #,row.names=rowNames))

	##### call SPOT	###############################			
	spotResult<-spot(spotConfig=control)
	
	##### prepare Results	###############################		
	result <- list()
	pNames <- row.names(spotResult$alg.roi);
	result$par <- as.numeric(spotResult$alg.currentBest[nrow(spotResult$alg.currentBest),pNames])
	result$value <- spotResult$alg.currentBest[nrow(spotResult$alg.currentBest),1]
	#result$counts <-
	result$convergence <- 0 # 0 indicates successful completion
	result$message <- NULL
	result$hessian <- NULL	
	return(result)
}


###################################################################################################
#' function call for spotOptim
#'
#' SPOT uses this function to call functions passed to \code{\link{spotOptim}} or \code{\link{spot}} like they would be passed to optim().
#' That means, it will be used whenever an actual function is passed instead of a string. When a string is passed
#' the string itself will contain the interface to use.
#' This function is needed as an interface, to ensure the right information
#' are passed from SPOT to the target function.
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
spotOptimInterface <- function(spotConfig){
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
	spotWriteLines(spotConfig$io.verbosity,1,"spotOptimInterface...", con=stderr());
	f<-"UserSuppliedFunction"
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
			dimcounter <- 1
			x <- NULL			
			while (exists(paste("VARX", dimcounter, sep=""))){
				name <- paste("VARX", dimcounter, sep="")
				varHelper  <- des[name]
				x <- cbind(x,varHelper[k,1])
				dimcounter <- dimcounter+1
			}
			
			x <- t(x)
			n <- dimcounter - 1
			conf <- k
			if (exists("CONFIG")){
				conf <- des$CONFIG[k]
			}
			if (exists("STEP")){
				step <- des$STEP[k]
			}
			seed <- des$SEED[k]+i-1			
			spotPrint(spotConfig$io.verbosity,1,c("Config:",k ," Repeat:",i))
			y <- spotConfig$alg.func.tar(x)#, noise=noise, noise.type=noise.type, spot.noise.minimum.at.value=spot.noise.minimum.at.value)
			#No external noise is used, the user has to take care of that himself	
			spotPrint(spotConfig$io.verbosity,1,y)
			res <- NULL
			res <- list(Y=y,					
					Function=f,					
					DIM=n,
					STEP=step,
					SEED=seed,
					CONFIG=conf
			)
			
			for (counter in 1:(dimcounter-1)){
				res[paste("VARX", counter, sep="")] <- x[counter]
			}
			
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
