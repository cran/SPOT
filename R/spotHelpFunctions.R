##################################################################################
#' Spot Write Best
#' 
#' Helper function that simply writes data to the .bst-file 
#' (appending or creating - depends on the existance of the .bst-file)
#' 
#' Result/Effects: 
#' adds one row to the best file and the alg.currentBest data frame.
#'
#' @param B matrix containing the merged result data of the current SPOT run
#' @param spotConfig all parameters, the ones of interest are:\cr
#' the name of the file for the best-data to be stored in: \code{spotConfig$io.bstFileName}\cr
#' the name string of the result column: \code{spotConfig$alg.resultColumn}\cr
#' the boolean that specifies if files are actually used or not: \code{spot.fileMode}\cr
#' the data frame that will be extended with the current best: \code{alg.currentBest}\cr
#'
#' @export
###################################################################################
spotWriteBest <- function(B, spotConfig){
	x <- as.matrix(B$x);
	y <- B$mergedY;
	A <- cbind(y,x,COUNT=B$count,CONFIG=B$CONFIG)        
	#C <-  data.frame(A[order(y,decreasing=FALSE),]);
	colnames(A)[1:length(spotConfig$alg.resultColumn)]<-spotConfig$alg.resultColumn
	if(!is.null(dim(y))){C <- data.frame(A[order(y[,1],y[,2],decreasing=FALSE),]);}  #TODO this means the best will only be determined by first objective, second objective breaking ties
	else{C <- data.frame(A[order(y,decreasing=FALSE),]);}
	### commented the following line (ocba):
        ### C = C[C$COUNT==max(C$COUNT),];    # choose only among the solutions with highest repeat	
	## col.names should be written only once:	
	best <-  C[1,]
	n <- max(spotConfig$alg.currentResult$STEP)+1
	best$STEP<-n
	if(spotConfig$spot.fileMode){
		colNames = TRUE
		if (file.exists(spotConfig$io.bstFileName)){
			colNames = FALSE
		}
		write.table(as.matrix(best) #MZ bugfix: added as.matrix, because elements are sometimes lists for some reason??
				, file = spotConfig$io.bstFileName
				, col.names= colNames
				, row.names = FALSE
				, append = !colNames         ## /WK/
				, sep = " ",
				, quote = FALSE
				, eol = "\n"
		);
	}
	spotConfig$alg.currentBest=rbind(spotConfig$alg.currentBest,best); #Add the best to the bst list in spotConfig, if spot.fileMode is false
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

#' @param des design provided by any spotCreateDesignXXX()-function 
#' @param verbosity for values greater than two, a message is given
#' @param sep the column separator (should not be empty for writing table to .des-file)
#' @param filename the filename the design should be written to
#'
#' @seealso \code{\link{spotStepRunAlg}}
#' @export
###################################################################################
spotWriteDes<-function(des, verbosity, sep, filename){	
	## empty separator is only required by input, because then it can distinguish all whitespaces, 
	## but output must be separated with a well defined separator, so the empty separator is changed to a space " "
	outsep <- sep;
	if(outsep=="")
		outsep <- " ";
	spotWriteLines(verbosity,3,paste(" design written to::", filename), con=stderr());
		write.table(des
			, file = filename
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
#' @param aroi data frame.  
#' @param verbosity for values greater than two, a message is given
#' @param sep the column separator used when writing the table to .aroi-file
#' @param filename the filename the aroi should be written to
#'
#' @export
###################################################################################
spotWriteAroi<-function(aroi, verbosity, sep, filename){	
	colnames(aroi) <- c("name", "lower", "upper", "type")	
	## Empty separator is only required by input, because then it can distinguish all whitespaces, 
	## but output must be separated with a well defined separator, so the empty separator is changed to a space " "
	outsep <- sep;
	if(outsep=="")
		outsep <- " ";
	spotWriteLines(verbosity,3,paste(" aroi written to::", filename), con=stderr());
	write.table(aroi
			, file = filename
			, row.names = FALSE
			, sep = outsep
			, quote = FALSE
			, append = FALSE
			, col.names=TRUE
	);	
}


##################################################################################
# Spot Read Aroi
#
# Helper function spotReadAroi reads actual region of interest from the .aroi-file
#
# @param filename the filename the AROI should be read from
# @param verbosity for values greater than two, a message is given
#
# @return data.frame \code{aroi} \cr
# - \code{aroi} contains the data from the aroi file
#		
# @export
###################################################################################
#read aroi is deprecated, replaced by spotReadRoi
# spotReadAroi<-function(filename,verbosity){
	# spotWriteLines(verbosity,3,paste("Load actual algorithm design (AROI): ", filename, collapse=""));
	# aroi.df <- read.table( filename		
			# , header = TRUE
			# , as.is=TRUE
			# , row.names = 1 #Parameter als Zeilennamen
	# );
	# return(aroi.df)
# }


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
#' @seealso  \code{\link{SPOT}} \code{\link{writeLines}}
#' @keywords internal
###################################################################################
spotWriteLines<-function(set.io.verbosity,io.verbosity,myString,con=stderr()){	
	if(set.io.verbosity>=io.verbosity){
		writeLines(myString,con)
	}	
}

##################################################################################
#' Spot Print
#'
#' This help function prints the given argument "myArg" 
#' only if user gives the io.verbosity to do so 
#'
#' @param set.io.verbosity	\code{spotConfig$io.verbosity} should be passed here. Global flag to drive the \code{io.verbosity} of the programm
#' @param io.verbosity \code{io.verbosity} for this specified output string
#' @param myArg the argument to be printed
#' 
#' @seealso  \code{\link{SPOT}} \code{\link{writeLines}}
#' @keywords internal
###################################################################################
spotPrint<-function(set.io.verbosity,io.verbosity,myArg){	
	if(set.io.verbosity>=io.verbosity){
		print(myArg)
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
#' @export
#' @keywords internal
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
#' @export
#' @keywords internal
####################################################################################
spotVersion <- function(){
	return(packageDescription("SPOT")$Version)
}


#todo this should not be here?
###################################################################################
#' rsm Model 
#'
#' Help function creating a response-surface Regression
#' this function will be replaced when rsm-package has fixed a bug 
#' @param formula must be a formula 
#' @param data parameter holding the data for the formula  
#' @return model usd by predict \cr
#' fix of rsm - Model 
#' @export
####################################################################################
spotRsm <- function(formula, data) 
{
	spotInstAndLoadPackages("rsm")
	CALL = match.call(lm)
	CALL[[1]] = as.name("lm")
	oc = as.character(deparse(formula))
	nc = sub("SO\\(([a-zA-Z0-9, ._]+)\\)", "FO\\(\\1\\) + TWI\\(\\1\\) + PQ\\(\\1\\)",oc)
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
		warning(paste("TWI() term not usable because it has",length(i.twi), "d.f. instead of", k * (k - 1)/2))
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
		LM$labels$PQ = list(idx = i.pq, lab = paste(fonm, 2,sep = "^"))
	}
	else if (length(i.pq) > 0) 
		warning(paste("PQ() term not usable because it has",length(i.pq), "d.f. instead of", k))
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

###################################################################################
#' write ROI for repeats
#'
#' @param workingDir Working directory as string
#' @param bstFile best file
#' @param roiInFile ROI in file
#' @param roiOutFile ROI out file
#' @export
#' @keywords internal
####################################################################################
spotWriteRoiFileForRepeats <- function(workingDir, bstFile, roiInFile, roiOutFile){
setwd(workingDir)
best.df <- read.table(bstFile, header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)
## Since the names are in the first column of the roi file, we use "row.names=1" in the following command:
roi.df <- read.table(roiInFile, header=TRUE, row.names=1)
pNames <- row.names(roi.df)
best <- (best.df[,pNames])[nrow(best.df),]
ncol(best)
A <- rep(as.matrix(best[1]),2)
for( i in 2:ncol(best)){
  A <- rbind(A,  rep(as.matrix(best[i]),2))
}
A <- cbind(pNames, A)
rownames(A) <- pNames
typeCol <- rep("FLOAT", ncol(best))
A <- cbind(A, typeCol)
colnames(A) <- c("name", "lower", "upper", "type")	
write.table(A
			, file = roiOutFile
			, row.names = FALSE
			, sep = " "
			, quote = FALSE
			, append = FALSE
			, col.names=TRUE
	)	
}




###################################################################################################
#' Region Of Interest Constructor
#'
#' This function can be used to construct a region of interest, to be passed on to spot().
#'
#' @param lower vector or a single number, specifiying lower boundary of ROI variables
#' @param upper vector or a single number, specifiying upper boundary of ROI variables
#' @param type vector of strings or single string, specifiying the data type of the variables. Can be: "FLOAT", "INT", "FACTOR"
#' @param varnames vector or NULL, telling the name of each variable. Can be NULL, so that default variable names will be used.
#' @param dimROI defines the number of variables. If dimROI is set (not NULL), the other vectors should have length=dimROI, or length=1.
#' @examples
#' ## without varnames or dimROI
#' alg.roi <- spotROI(c(0,0),c(1,1),c("FLOAT","FLOAT"))
#' ## with varnames
#' alg.roi <- spotROI(c(0,0),c(1,1),c("FLOAT","FLOAT"),c("VARX1","VARX3"))
#' ## lower and upper only
#' alg.roi <- spotROI(c(0,1,-2),10)
#' alg.roi <- spotROI(c(0,1,-2),c(2,3,100))
#' ## with dimROI
#' alg.roi <- spotROI(-10,10,"FLOAT",dimROI=4)
#' ## with varnames and dimROI
#' alg.roi <- spotROI(-10,10,"FLOAT",c("x1","x2","x3","x4"),dimROI=4)
#' @export
###################################################################################################
spotROI<-function(lower,upper,type="FLOAT",varnames=NULL,dimROI=NULL){	
	#dim is either the length of varnames, or of lower boundary
	if(is.null(dimROI)){
		if(length(lower)>1)dimROI=length(lower)
		else if(length(upper)>1)dimROI=length(upper)
		else if(length(type)>1)dimROI=length(type)
		else if(length(varnames)>1)dimROI=length(varnames)
		else dimROI=1
	}
	#If length of any vector is one, repeat it to match dimension. 
	if(length(upper)==1)upper=rep(upper,dimROI)
	if(length(lower)==1)lower=rep(lower,dimROI)
	if(length(type)==1)type=rep(type,dimROI)
	#determine varnames if not specified by user
	if(is.null(varnames)){
		helpFx <- function(x){paste("VARX",x,sep="")}
		varnames = helpFx(c(1:length(lower)))		
	}
	#check if all vector lengths are consistent
	if(length(lower)!=length(upper))stop("'upper' and 'lower' boundaries of region of interest have not the same length.");
	if(length(lower)!=length(type))stop("The 'lower' vector and the 'type' vector for the region of interest have not the same length");
	if(length(lower)!=length(varnames))stop("The 'lower' vector and the 'varnames' vector for the region of interest have not the same length");
	#determine varnames if not specified by user
	if(is.null(varnames)){
		helpFx <- function(x){paste("VARX",x,sep="")}
		varnames = helpFx(c(1:length(lower)))		
	}
	#check data format
	for(i in 1:dimROI){
		if((type[i]=="FLOAT")&&(!(is.numeric(upper[i])&&is.numeric(lower[i]))))stop("upper and lower values should be numerics if type == 'INT' ")
	}
	#check each variable for consistence
	for(i in 1:dimROI){
		#check if lower is lower than upper
		if(!(lower[i]<upper[i]))stop("A value in the lower boundary of the 
		ROI is not smaller than the value of the upper boundary. 
		Check your 'upper' and 'lower' vectors or the respective .roi file")
		#TODO check for valid data type
		if(!(type=="FLOAT"||type=="INT"||type=="FACTOR"))stop("
		The data type in the Region Of Interest is not valid.
		Check your 'type' vectors or the respective .roi file.
		Possible data types are 'FLOAT', 'INT' or 'FACTOR'")
	}
	#check for equal varnames
	if( any(duplicated(varnames))){
		stop(paste("Not all variables in the region of interest have unique names.\n Check the variable names column vector.\n Duplicated variable names are: ", paste(varnames[duplicated(varnames)],collapse=" ")))
	}	
	alg.roi=data.frame(lower=lower,upper=upper,type=type,row.names=varnames)
}


##################################################################################
#' Spot Read ROI
#'
#' help function spotReadRoi reads region of interest from the .roi-file
#'
#' @param roiFile this file contains the roi
#' @param sep this is used as a column separator
#' @param verbosity \code{io.verbosity} for this specified output string
#'
#' @return data.frame \code{aroi} \cr
#' - \code{roi} contains the data from the roi file
#'		
#' @export
###################################################################################
spotReadRoi<-function(roiFile,sep,verbosity=0){
	spotWriteLines(verbosity,3,paste("Load actual algorithm design (AROI): ", roiFile, collapse=""));
	alg.roi <- read.table(roiFile
			, sep = sep
			, header = TRUE
			, as.is=TRUE
			, row.names = 1 #Parameter als Zeilennamen
		);
	#checking format of file	
	if(colnames(alg.roi)[1]!="low" && colnames(alg.roi)[1]!="lower") stop("Second Column in the .roi file should be named 'lower' ('low' works, too, but is deprecated). Check if the header is wrong or missing in this file.")
	if(colnames(alg.roi)[2]!="high" && colnames(alg.roi)[2]!="upper") stop("Third Column in the .roi file should be named 'upper' ('high' works, too, but is deprecated). Check if the header is wrong or missing in this file.")
	if(colnames(alg.roi)[3]!="type") stop("Fourth column in the .roi file should be named 'type'. Check if the header is wrong or missing in this file.")
	#use the roi constructor to check for validity and convert to correct column names
	alg.roi <- spotROI(alg.roi[,1],alg.roi[,2],alg.roi[,3],rownames(alg.roi))
	return(alg.roi)
}