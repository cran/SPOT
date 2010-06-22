###################################################################################################
#' noisy Six Hump function
#'
#' Example for SPOT optimizing the 6-hump camel back function, see 
#'   http://www.it.lut.fi/ip/evo/functions/node26.html
#'
#'
#' @param x	two dimensional vector that will be evauluated by the branin function
#' @param noise	this number is added to the function value y as noise
#'
#' @return number \code{y} \cr
#' - \code{y} is the value of the corresponding \code{x} vector
#'
#' @references  \code{\link{SPOT}} \code{\link{spot}} \code{\link{demo}} \code{\link{spotFuncStartBranin}}
#' \code{\link{spotFuncStartBranin}} \code{\link{spotFuncStartSixHump}}
###################################################################################################
spotNoisySixHumpFunction <- function (x, noise=0.0) {
	x1 <- x[1] 
	x2 <- x[2] 
	y<-(4-2.1*x1^2+x1^4/3)*x1^2+x1*x2+(-4+4*x2^2)*x2^2  + noise*rnorm(1)
	return(y)
}

###################################################################################################
#' Six Hump function call for SPOT
#'
#' SPOT uses this function for some demos to call the \code{\link{spotNoisySixHumpFunction}} function 
#' 
#' @param pdFile name of the apd file
#' @param desFileName name of the des file
#' @param resFileName name of the res file
#'
#' @references  \code{\link{SPOT}} \code{\link{spot}} \code{\link{demo}} \code{\link{spotFuncStartBranin}}
#' \code{\link{spotFuncStartBranin}} \code{\link{spotNoisySixHumpFunction}}
###################################################################################################
spotFuncStartSixHump <- function(pdFile, desFileName, resFileName){
	writeLines(paste("Loading design file data from::", desFileName), con=stderr());
	writeLines("spotFuncStartSixHump...", con=stderr());
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
			y <- spotNoisySixHumpFunction(c(x1,x2), noise)
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
#
#	
#	
#	
#	
#	config<-nrow(des);
#	print(config);
#	if (is.null(des$CONFIG))
#	   stop("Design file is missing the required column CONFIG!")	   
#	for (k in 1:config){	 
#		for (i in 1:des$REPEATS[k]){
#			x1=des$x1[k]
#			x2=des$x2[k]			
#  		conf <- des$CONFIG[k]
#			seed <- des$SEED[k]+i			
#			cat(sprintf("Config: %5d,   Repeat: %5d\n",conf,i))		
#			browser()
#			y <- spotNoisySixHumpFunction(x1,x2,noise)
#			print(y)
#			res <- NULL
#			res <- list(Y=y
#					     ,x1=x1
#					     ,x2=x2
#					     ,SEED=seed
#					     ,CONFIG=conf                  
#					     ,REP=i
#			)
#			res <-data.frame(res)
#			colNames = TRUE
#			if (file.exists(resFileName)){
#				colNames = FALSE
#			}		
#			
#			write.table(res
#					, file = resFileName
#					, row.names = FALSE
#					, col.names = colNames
#					, sep = " "              
#					, append = !colNames               
#					, quote = FALSE
#			);			
#		}	# for (i)	 
#	}	# for (k)	
#}
#
#
