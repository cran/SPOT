##################################################################################
#' Spot Safely Add Source
#'
#' spotSafelyAddSource is a call frame for all user written 
#' R-functions 
#'
#' All these functions to be sourced in any case, MUST follow this rule: 
#' The name of the file MUST be THE SAME as the function that should be  
#' included (plus extension of the two characters ".R" to indicate the R-File) 
#' This interface may be used for adding
#' - spotCreateDesign<xxx> functions
#' - spotPredictor<xxx> functions
#' - spotReport<xxx> functions
#' - but also for the algorithm, where the srcPath cannot be \code{spotConfig$srcPath}
#' Result/Effects: sources external, user written function or does nothing if the 
#' function alreadey exists
#' returns Error if 
#' 1) file is not found or 
#' 2) function is not part of the existing file 
#' after including a function you can use it by 
#' eval(call(functionName, <functionParamList>))
#'
#' @param srcPath location where to find the R-file to be sourced (mostly: \code{spotConfig$srcPath}) 
#' @param functionName the name of a R-function that MUST be part of a sourceFile
#' with the same name, mostly
#' @param set.io.verbosity needed only for transfer to \code{\link{spotWriteLines}}, mostly \code{spotConfig$io.verbosity}
#' @return boolean \cr 
#' - The boolean tells if adding the source was successfull (TRUE) or not (FALSE)
###################################################################################

spotSafelyAddSource <- function(srcPath,functionName,set.io.verbosity){	
	if (exists(functionName)){
		return(TRUE)
	} else { # function does not exist, so we try to source the file...
		fileToAdd<-paste(srcPath,"/",functionName,".R",sep="")
		if(file.exists(fileToAdd)){
			source(fileToAdd,local=FALSE)
			if(exists(functionName)){
				return(TRUE)
			}else{
				spotWriteLines(set.io.verbosity,0,paste("Error: spot.R::spotSafelyAddSource::",functionName,".R"," added. The following function is not available: ",functionName,sep="" ))
				return(FALSE)
			}
		}else{
			spotWriteLines(set.io.verbosity,0,paste("Error: spot.R::spotSafelyAddSource::",functionName,".R"," does not exist in ",srcPath,sep="" ))
			spotWriteLines(set.io.verbosity,0,paste(fileToAdd," was identified, but not found - something wrong in building the string?"))
			return(FALSE)
		}
	}
}


##################################################################################
#' Spot Write Best
#' 
#' Help function that simply writes data to the .bst-file 
#' (appending or creating - depends on the existance of the .bst-file)
#' 
#' Result/Effects: 
#' adds  one row to the best file 
#'
#' @param B matrix 
#' @param spotConfig all parameters, the only one of interest is the name of the
#' file for the best-data to be stored in: \code{spotConfig$io.bstFileName}
#'
###################################################################################
spotWriteBest <- function(B, spotConfig){
	x <- as.matrix(B$x);
	Y <- B$mergedY;
	A <- cbind(Y,x,COUNT=B$count,CONFIG=B$CONFIG)        
	C <-  data.frame(A[order(Y,decreasing=FALSE),]);
	#TODOMZ: Implement the following line right? Before only in report, but that resulted into differences between report/.bst file
	C = C[C$COUNT==max(C$COUNT),];    # choose only among the solutions with highest repeat	
	## col.names should be written only once:	
	if(spotConfig$spot.fileMode){
		colNames = TRUE
		if (file.exists(spotConfig$io.bstFileName)){
			colNames = FALSE
		}
		write.table(C[1,]
				, file = spotConfig$io.bstFileName
				, col.names= colNames
				, row.name = FALSE
				, append = !colNames         ## /WK/
				, sep = " ",
				, quote = FALSE
				, eol = "\n"
		);
	}
	spotConfig$alg.currentBest=rbind(spotConfig$alg.currentBest,C[1,]); #Add the best to the bst list in spotConfig, if spot.fileMode is false
	return(spotConfig); 
}


##################################################################################
#'Spot Write Design
#'
#' help function that simply writes Data to the .des-file
#'
#' Result/Effects: 
#' rewrites the design-file for the next call of the \code{\link{spotStepRunAlg}}
#'
#' @param spotConfig all parameters, only two are used:
#'		\code{spotConfig$io.columnSep}: the column separator should not be empty for writing table to .des-file
#'		\code{spotConfig$io.desFileName}: the filename the design should be written to
#' @param des design provided by any spotCreateDesignXXX()-function 
#'
#' @references  \code{\link{spotStepRunAlg}}
###################################################################################
spotWriteDes<-function(spotConfig,des){
	
	## empty separator is only required by input, because then it can distinguish all whitespaces, 
	## but output must be separated with a well defined separator, so the empty separator is changed to a space " "
	outsep <- spotConfig$io.columnSep;
	if(outsep=="")
		outsep <- " ";
	spotWriteLines(spotConfig$io.verbosity,2,paste(" design written to::", spotConfig$io.desFileName), con=stderr());
		write.table(des
			, file = spotConfig$io.desFileName
			, row.names = FALSE
			, sep = outsep
			, quote = FALSE
			, append = FALSE
			, col.names=TRUE
	);	
}

##################################################################################
#' Spot Write Aroi
#'
#' help function spotWriteAroi writes actual region of interest to the .aroi-file
#'		
#' Result/Effects: 
#' rewrites the actual region of interest-file 
#'
#' @param spotConfig all parameters, only two are used:
#'		\code{spotConfig$io.columnSep}: the column separator should not be empty for writing table to .des-file
#'		\code{spotConfig$io.aroiFileName}: the filename the design should be written to
#' @param aroi data frame.  
###################################################################################
spotWriteAroi<-function(spotConfig,aroi){
	## Colnames have to be supplied by the user, e.g., colnames(aroi) <- c("name", "low", "high")
	## Empty separator is only required by input, because then it can distinguish all whitespaces, 
	## but output must be separated with a well defined separator, so the empty separator is changed to a space " "
	outsep <- spotConfig$io.columnSep;
	if(outsep=="")
		outsep <- " ";
	spotWriteLines(spotConfig$io.verbosity,2,paste(" aroi written to::", spotConfig$io.aroiFileName), con=stderr());
	write.table(aroi
			, file = spotConfig$io.aroiFileName
			, row.names = FALSE
			, sep = outsep
			, quote = FALSE
			, append = FALSE
			, col.names=TRUE
	);	
}

##################################################################################
#' Spot Read Aroi
#'
#' help function spotReadAroi reads actual region of interest from the .aroi-file
#'
#' @param spotConfig all parameters, only two are used: \cr
#'		\code{spotConfig$io.columnSep}: the column separator should not be empty for writing table to .des-file \cr
#'		\code{spotConfig$io.aroiFileName}: the filename the design should be read from
#'
#' @return data.frame \code{aroi} \cr
#' - \code{aroi} contains the data from the aroi file
#'		
###################################################################################
spotReadAroi<-function(spotConfig){
	writeLines(paste("Load actual algorithm design (AROI): ", spotConfig$io.aroiFileName, collapse="")
			, con=stderr());
	aroi.df <- read.table( spotConfig$io.aroiFileName		
			, header = TRUE
			, as.is=TRUE
			, row.names = 1 #Parameter als Zeilennamen
	);
	return(aroi.df)
}


##################################################################################
#' Spot Write Lines
#'
#' This help function writes the string given in "myString" 
#' only if user gives the io.verbosity to do so 
#'
#' @param set.io.verbosity	\code{spotConfig$io.verbosity} should be passed here. Global flag to drive the \code{io.verbosity} of the programm
#' @param io.verbosity \code{io.verbosity} for this specified output string
#' @param myString the string to be written to stdout
#' @param con defines the output stream, defaults to \code{stderr()}
#' 
#' @references  \code{\link{SPOT}} \code{\link{writeLines}}
###################################################################################
spotWriteLines<-function(set.io.verbosity,io.verbosity,myString,con=stderr()){	
	if(set.io.verbosity>=io.verbosity){
		writeLines(myString)
	}	
}

###################################################################################
#' Spot install and load required packages
#'
#' Help function that installs and loads the packages that are given by a list
#' new predictors should use this function to install their packages, to make sure
#' that they are only installed if necessary
#' Use it like this:
#' spotInstAndLoadPackages("rsm")
#' spotInstAndLoadPackages(c('FrF2',  'DoE.wrapper'))
#' spotInstAndLoadPackages("rsm","http://cran.r-project.org")
#' 
#' @param packageList a list of strings holding the names of the packages that should 
#' 		  installed if necessary and then loaded for use
#' @param reposLoc ["http://cran.r-project.org"] a string of the location,from where 
#' 		the package is to be downloaded - the default is the cran R-Project page, but 
#' 		if special packages are needed from other locations this can be set here too 
#' 
####################################################################################
spotInstAndLoadPackages <- function(packageList,reposLoc="http://cran.r-project.org"){
	installed = packageList %in% installed.packages()[, 'Package'];
	if (length(packageList[!installed]) >=1){
		writeLines("SPOT detected packages that needs installation: ")
		print(packageList)
		install.packages(packageList[!installed], repos=reposLoc);
	}
	for (i in 1:length(packageList)){
		require(packageList[i],character.only=TRUE,quietly = TRUE)
	}
}

###################################################################################
#' Spot Version
#'
#' Help function that returns the version of SPOT 
#' provided for convinience
#'
#' @return string \cr
#' holding the installed version of SPOT
####################################################################################
spotVersion <- function(){

	writeLines("Initial version: 0.1.888")
	writeLines("")
	writeLines("Changes from version 0.1.888 to version 0.1.1016:")
	writeLines("1. New function: spotGui()")
	writeLines("\t	Starts a java gui to work with SPOT")
	writeLines("2. New parameters for the \"meta\" task:")
	writeLines("\t	- spotConfig$report.meta.func")
	writeLines("\t	- spotConfig$report.meta.path")
	writeLines("\t	Define name and path of a report that summarizes a meta experiment")
	writeLines("3. Fixed Documentation")
	writeLines("")	
	writeLines("Changes from version 0.1.1016 to version NEWVERSION")
	writeLines("1. New parameters in spotConfig:")
	writeLines("\t	a.\t	spot.fileMode: Boolean, defines if ")
	writeLines("\t\t		created data is logged in res/des/bst/aroi files")
	writeLines("\t\t		If TRUE (default) files will be written, else")
	writeLines("\t\t		the results will be written to spotConfig (see")
	writeLines("\t\t		the followind parameters)")
	writeLines("\t	b.\t	alg.currentResult: Holds the results of the")
	writeLines("\t\t		target algorithm or function that is optimized ")
	writeLines("\t\t		by spot, as a data frame. (earlier only in .res file)")
	writeLines("\t	c.\t	alg.aroi: this holds the aroi (earlier only in .aroi ")
	writeLines("\t\t		file)")
	writeLines("\t	d.\t	alg.currentBest: This holds a data frame with the best")
	writeLines("\t\t		results (earlier only in .bst files)")
	writeLines("\t	e.\t	alg.currentDesign: This holds a data frame with the ")	
	writeLines("\t\t		design to be evaluated by the next \"run\" task.(earlier")
	writeLines("\t\t		only in .des files)")
	writeLines("")
	writeLines("2. Changes to userdefined functions:")
	writeLines("")
	writeLines("\t	spotConfig$alg.func:")
	writeLines("\t	This function now recieves and returns only spotConfig.")
	writeLines("\t	This means resFileName, apdFileName, and desFileName are no")
	writeLines("\t	input arguments anymore, they need to be read from 	spotConfig.")
	writeLines("\t	The function needs to be changed to write the \"result\" data")
	writeLines("\t	frame to the spotConfig$alg.currentResult.")
	writeLines("\t	It can also be written to .res file, if wanted by the user.")
	writeLines("\t	(check for spotConfig$spot.fileMode if needed)")
	writeLines("")
	writeLines("\t	spotConfig$report.func")
	writeLines("\t	The function now also returns spotConfig.")
	writeLines("\t	To get the best value the following lines are be used now in the default report: ")
	writeLines("\t	#################################	")
	writeLines("\t	spotConfig=spotWriteBest(mergedData, spotConfig);")
	writeLines("\t	C1=spotConfig$alg.currentBest[nrow(spotConfig$alg.currentBest),]")
	writeLines("\t	#################################")
	writeLines("")
	writeLines("\t	In general some spot functions you might use in your")
	writeLines("\t	userdefined functions will now return spotConfig.")
	writeLines("")
	writeLines("4. spot() can now be called with a list of settings, e.g. spotConfig.")
	writeLines("\t	example: spot(\"example.conf\",\"auto\",spotConfig=list(auto.loop.nevals=150)).")
	writeLines("\t	spot() will also return the spotConfig. This can then be feed into the next spot() run.")
	writeLines("")
	writeLines("4. Changes to the spotGui:")
	writeLines("\t	a.\t	The spotGui layout was changed, a menu bar added.")
	writeLines("\t	b.\t	New feature: Create your own test functions with")
	writeLines("\t\t	the function generator")
	writeLines("")
	writeLines("")
	return(packageDescription("SPOT")$Version)
}

###################################################################################
#' rsm Model 
#'
#' Help function creating a response-surface Regression
#' this function will be replaced when rsm-package has fixed a bug 
#' @param formula must be a formula 
#' @param data parameter holding the data for the formula  
#' @return model usd by predict \cr
#' fix of rsm - Model 
####################################################################################
spotRsm <- function(formula, data) 
{
	spotInstAndLoadPackages("rsm")
	CALL = match.call(lm)
	CALL[[1]] = as.name("lm")
	oc = as.character(deparse(formula))
	nc = sub("SO\\(([a-zA-Z0-9, ._]+)\\)", "FO\\(\\1\\) + TWI\\(\\1\\) + PQ\\(\\1\\)", 
			oc)
	nc = sub("TWI\\([a-zA-Z0-9 ._]+\\)", "", nc)
	CALL$formula = formula(nc)
	CALL$data = data
	LM = eval(CALL)

	LM$call[[1]] = as.name("rsm")
	LM$call$formula = formula(oc)
	nm = names(LM$coef)
	i.fo = grep("FO\\(", nm)
	if (length(i.fo) == 0) {
		warning("No FO() terms in model; cannot use RSM methods\nAn 'lm' object has been returned.")
		return(LM)
	}
	k = length(i.fo)
	LM$b = LM$coef[i.fo]
	LM$order = 1
	foterm = as.list(LM$terms[LM$assign[min(i.fo)]][[3]])
	fonm = names(LM$b) = sapply(foterm, as.character)[-1]
	LM$labels = list(FO = list(idx = i.fo, lab = fonm))
	names(LM$coef)[i.fo] = LM$labels
	i.twi = grep("TWI\\(", nm)
	if ((k > 1) & (length(i.twi) == k * (k - 1)/2)) {
		btwi = LM$coef[i.twi]
		LM$order = 1.5
		LM$B = diag(rep(0, k))
		col = 1
		twi.lab = rep("", length(i.twi))
		for (i in 1:(k - 1)) {
			rng = 1:(k - i)
			LM$B[i, rng + i] = LM$B[rng + i, i] = 0.5 * btwi[col + 
							rng - 1]
			twi.lab[col + rng - 1] = paste(fonm[i], fonm[rng + 
									i], sep = ":")
			col = col + k - i
		}
		dimnames(LM$B) = list(fonm, fonm)
		LM$labels$TWI = list(idx = i.twi, lab = twi.lab)
	}
	else if (length(i.twi) > 0) 
		warning(paste("TWI() term not usable because it has", 
						length(i.twi), "d.f. instead of", k * (k - 1)/2))
	i.pq = grep("PQ\\(", nm)
	if (length(i.pq) == k) {
		LM$order = 2
		if (is.null(LM$B)) {
			if (k > 1) 
				LM$B = diag(LM$coef[i.pq])
			else LM$B = matrix(LM$coef[i.pq], nrow = 1)
			dimnames(LM$B) = list(fonm, fonm)
		}
		else diag(LM$B) = LM$coef[i.pq]
		LM$labels$PQ = list(idx = i.pq, lab = paste(fonm, 2, 
						sep = "^"))
	}
	else if (length(i.pq) > 0) 
		warning(paste("PQ() term not usable because it has", 
						length(i.pq), "d.f. instead of", k))
	if (LM$order == 1) 
		aliased = any(is.na(LM$b))
	else aliased = any(is.na(cbind(LM$B, LM$b)))
	if (aliased) 
		warning("Some coefficients are aliased - cannot use 'rsm' methods.\n  Returning an 'lm' object.")
	else {
		if (!missing(data)) 
			if (inherits(data, "coded.data")) 
				LM$coding = attr(data, "codings")
		class(LM) = c("rsm", "lm")
	}
	LM
}

