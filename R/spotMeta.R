# reads the Meta file, holding lines like :
## n=c(2,4,8,16,22)
## x0=list("uniform", "gaussian", c(-10,0,10,-10,0,10,-10,0,10,-10,0,10,-10,0,10,-10))
## steps=(100,200,500,1000,5000)
## colours=c("red","green")

##################################################################################
#' Spot Meta Read
#' 
#' Help function that simply reads the .metafile and returns the values in a list
#'
#' @param metaFile file that holds the meta - variables that should be 
#' 		added full factorial to the apd-files 
#'
#' @return \code{myList} \cr
#' - \code{myList} is holding a list of all variables defined in the .meta file 
###################################################################################
spotMetaRead <- function(metaFile){
	.lsBefore <- NA
	.lsBefore <- ls()
	source(metaFile,local=TRUE)
	.lsAfter<-ls()
	.lsDiff<-setdiff(.lsAfter,.lsBefore)
	myList<-sapply(.lsDiff, function (.lsDiff) { get(.lsDiff)}, USE.NAMES=TRUE)
	return(myList)
}
##################################################################################
#' Spot Meta Get Assignment Operator
#' 
#' Help function that simply searches for the assignment operator in the 
#' .meta-file and returns the value or stops
#'
#' @param metaFile file that holds the meta - variables that should be 
#' 		added full factorial to the apd-files 
#'
#' @return character \cr
#' - holding either "=" or "<-" respectively
###################################################################################
spotMetaGetAssOp<- function(metaFile){
	myVarEq<-ncol(read.table(metaFile,sep="="))
	myVarArrow<-ncol(read.table(metaFile,sep="<"))
	if (myVarEq==2 && myVarArrow==1){ # file is build with "=" as assignment operator
		return("=")
	}
	if (myVarEq==1 && myVarArrow==2){ # file is build with "<-" as assignment operator
		return("<-")
	}
	writeLines("the .meta file must have assignments like \"=\" or \"<-\" in it to work correctly")
	stop()
	return(FALSE) # meta file must be corrupt
}
##################################################################################
#' Spot Meta Creat Sub Project
#' 
#' Help function that simply creates a project- sub-directory 
#' for one line of the factorial design for the .meta-file and
#' copies the .roi, the .apd and the .conf file to that subDir.
#' It is only called from \code{\link{spotStepMetaOpt}}
#'
#' @param spotConfig spotConfig$io.apdFilename, spotConfig$io.roiFileName 
#'  and spotConfig$configFileName are used for copying
#' @param projectName name derived from the factors of ONE row of the factorial design
#'  holding all the parameters for the Meta calls 
#' @param apdLines a vector of lines to be added to the apdFile that is copied here
#' 
#' @return string \cr
#' - holding the directory created
###################################################################################
spotMetaCreateSubProject<-function(spotConfig,projectName,apdLines){
	conFilePrefix <-  unlist(strsplit(basename(spotConfig$configFileName), ".", fixed = TRUE))[1]
	myProject<-paste(conFilePrefix,projectName,sep="_")
	## TODO: JZ only create when not existing, - more sofisticated errorhandling is needed
	if (!file.exists(myProject)) 
		dir.create(myProject)
	
	myConfigFileName<-paste( unlist(strsplit(basename(spotConfig$configFileName), ".", fixed = TRUE))[1],"_",projectName,".conf",sep="")
	file.copy(spotConfig$configFileName,paste(myProject,"/",myConfigFileName,sep=""))
	spotConfig$configFileName<-myConfigFileName;

	myRoiFileName<-paste(unlist(strsplit(basename(spotConfig$io.roiFileName), ".", fixed = TRUE))[1],"_",projectName,".roi",sep="")
	file.copy(spotConfig$io.roiFileName,paste(myProject,"/",myRoiFileName,sep=""))
	spotConfig$io.roiFileName<-myRoiFileName
	
	myApdFileName<-paste(unlist(strsplit(basename(spotConfig$io.apdFileName), ".", fixed = TRUE))[1],"_",projectName,".apd",sep="")
	file.copy(spotConfig$io.apdFileName,paste(myProject,"/",myApdFileName,sep=""))
	spotConfig$io.apdFileName<-myApdFileName
	
	setwd(myProject)
	write.table(apdLines,file=myApdFileName,quote=FALSE, col.names=FALSE,row.names=FALSE,append=TRUE)
	
	
	myAroiFileName<-paste(unlist(strsplit(basename(spotConfig$io.aroiFileName), ".", fixed = TRUE))[1],"_",projectName,".aroi",sep="")
	spotConfig$io.aroiFileName<-myAroiFileName
	
	myBstFileName<-paste(unlist(strsplit(basename(spotConfig$io.bstFileName), ".", fixed = TRUE))[1],"_",projectName,".bst",sep="")
	spotConfig$io.bstFileName<-myBstFileName
	
	myResFileName<-paste(unlist(strsplit(basename(spotConfig$io.resFileName), ".", fixed = TRUE))[1],"_",projectName,".res",sep="")
	spotConfig$io.resFileName<-myResFileName
	
	myDesFileName<-paste(unlist(strsplit(basename(spotConfig$io.desFileName), ".", fixed = TRUE))[1],"_",projectName,".des",sep="")
	spotConfig$io.desFileName<-myDesFileName
	
	spotConfig$report.io.screen=FALSE
	spotConfig$report.io.pdf=FALSE
	spotConfig$io.verbosity=0
	
	return(spotConfig)
	
}



