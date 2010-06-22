#
#seed    random seed (e.g. 12345)
#steps   maximum number of evolution steps (e.g. 10000)
#target  objective function threshold for priliminary evolution end (e.g. 0.0001)
#f       objective function class name (e.g. TODO)
#n       problem dimension (e.g. 12)
#sigma0  initial step size (e.g. 1.0)
#a       step size muliplier (e.g. 1.2239)
#g       history length ( e.g. 12 == n)

spotAlgStartOnePlusOneEsJava <- function(io.apdFileName, io.desFileName, io.resFileName){
	writeLines(paste("Loading design file data from::", io.desFileName), con=stderr());
	writeLines("ES run...", con=stderr());
	print(io.apdFileName)
	## read default problem design
	source(io.apdFileName,local=TRUE)
	## read doe/dace etc settings:
	print(io.desFileName)
	des <- read.table(io.desFileName
			, sep=" "
			, header = TRUE
	);
	print(summary(des));
	
	##  SIGMANULL VARA VARG REPEATS SEED
	config<-nrow(des);
	print(config);
	attach(des)
	
	for (k in 1:config){
		if(des$REPEATS[k]>=1){
			for (i in 1:des$REPEATS[k]){
				##
				if (exists("SIGMANULL")){
					sigma0 <- des$SIGMANULL[k]
				}
				if (exists("VARA")){
					a <- des$VARA[k]
				}
				if (exists("VARG")){
					g <- round(des$VARG[k])
				}
				conf <- k
				if (exists("CONFIG")){
					conf <- des$CONFIG[k]
				}
				spotStep<-NA
				if (exists("STEP")){
					spotStep <- des$STEP[k]
				}
				seed <- des$SEED[k]+i			
				print(c("Config:",k ," Repeat:",i))
				callString <- paste("java -jar bin/simpleOnePlusOneES.jar", seed, steps, target, f, n, xp0, sigma0, a, g, px, py, sep = " ")
				print(callString)
				y <-system(callString, intern= TRUE)
				print(y)
				res <- NULL
				res <- list(Y=y, 
						SIGMANULL=sigma0,
						VARA=a,
						VARG=g,
						Function=f,
						MAXITER=steps,
						DIM=n,
						TARGET=target,
						SEED=seed,
						CONFIG=conf
				)
				if (exists("STEP")){
					res=c(res,STEP=spotStep)
				} 
				res <-data.frame(res)
				colNames = TRUE
				if (file.exists(io.resFileName)){
					colNames = FALSE
				}
				
				## quote = false is required for JAVA
				write.table(res
						, file = io.resFileName
						, row.names = FALSE
						, col.names = colNames
						, sep = " "              
						, append = !colNames
						, quote = FALSE
				);		
				colNames = FALSE
			} # end for i
		} # end if(des$REPEATS[k]>=1)
	}	#end for k
	detach(des)
}


