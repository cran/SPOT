## Experimental research in evolutionary computation
## author: thomas.bartz-beielstein@fh-koeln.de
## http://www.springer.com/3-540-32026-1
##
## Copyright (C) 2003-2010 T. Bartz-Beielstein and C. Lasarczyk
## This program is free software;
## you can redistribute it and/or modify it under the terms of the 
## GNU General Public License as published by the Free Software Foundation; 
## either version 3 of the License,
## or (at your option) any later version.
## This program is distributed in the hope that it will be useful, 
## but WITHOUT ANY WARRANTY; without even the implied warranty of 
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
## See the GNU General Public License for more details.
## You should have received a copy of the GNU General Public License along
##  with this program; if not, see <http://www.gnu.org/licenses/>.
##

# Package Description for Roxygene:
#' Sequential Parameter Optimization Toolbox in R
#'
#' \tabular{ll}{
#' Package: \tab SPOT\cr
#' Type: \tab Package\cr
#' Version: \tab 0.1.1065\cr
#' Date: \tab 14.10.2010\cr
#' License: \tab GPL (>= 3)\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' SPOT is a package for R, using statistic models to find
#' optimal parameters for optimization algorithms. SPOT is a very flexible and 
#' user oriented tool box for parameter optimization. The flexibility has its 
#' price: to fully use all the possibilities of flexibility the user is requested 
#' to look at a large number of spot-parameters to change. The good news is, that 
#' some defaults are given that already work perfectly well for 90 percent of the users.
#'
#' @name SPOT-package
#' @aliases SPOT
#' @docType package
#' @title Sequential Parameter Optimization Toolbox in R
#' @author Thomas Bartz-Beielstein \email{thomas.bartz-beielstein@@fh-koeln.de} with contributions from: J. Ziegenhirt, W.
#'    Konen, O. Flasch, P. Koch, M. Zaefferer
#' @references
#' \url{http://www.gm.fh-koeln.de/campus/personen/lehrende/thomas.bartz-beielstein/00489/} \cr
#' \url{http://www.springer.com/3-540-32026-1}
#' @keywords package
#' @seealso \code{\link{spot}}
#End of Package Description
NA #NULL, ends description without hiding first function
###################################################################################
## spot.R consists of three parts: 
## - PART ONE: some help functions
## - PART TWO: the steps implemented as functions too
## - PART THREE: the main SPO algorithm
###################################################################################

###################################################################################
###################################################################################
# PART ONE: help functions 
###################################################################################
###################################################################################

###################################################################################
## spotPrepareSystem()
##
## Prepare the system first: checks for missing packages and installs them if necessary
## this should only be necessary ONCE but it may fail due to permission problems.
## see FAQ No. 1
## This function is called but should have no effect, if the package is installed correctly,
## since loading the package should also load the packages it is depending on.
## This function is just left over for the developers, for easy developing the code
###################################################################################
## Where are the packages used?
## - AlgDesign is used by spotCreateDesignDoe() for it needs the function gen.factorial()
##
###################################################################################
###################################################################################
#' SPOT Prepare System - loads all required packages for SPOT
#' 
#' installs and loads all packages that are needed for the core functionality of SPOT
#' (hard coded in the function). All user defined Plugins needs to call
#' \code{\link{spotInstAndLoadPackages}} to add their dependencies properly\cr
#' This function is only provided for use in non-packaged version, for all packages 
#' are listed in the "Depends line" of DESCRIPTION 
#' 
#'
###################################################################################
spotPrepareSystem <- function(){
	### check whether necessary packages are installed and install missing packages
	necessaryPackages = c( 'lattice', 'graphics',
			'fields','rpart', 'maptree', 'colorspace', 
			'gtools' );
	###### default packages with various use 
	# 'gtools' Various R programming tools
	# 'colorspace',Color Space Manipulation 
	# 'lattice', 'graphics','fields' standard packages 
	###### default packages that are specified to be used in:  
	# spotPreditTree AND spotPlotBst: 'rpart'
	# spotPlotBst: 'maptree'
	###### deleted because use was not found:  
	# 'MASS' -  Main Package of Venables and Ripley's MASS -used in ???
	# 'vcd' Visualizing Categorical Data - used in ???
	# 'stats' -  R statistical functions used in ??? 
	# 'DoE.base' used in ???
	# 'car' used  in ???
	######
	## deleted from list and moved to the calling functions: 
	## rsm, tgp, randomForest, mlegp, FrF2, DoE.wrapper, AlgDesign, lhs
	# spotPredictLm: 'rsm'
	# spotPredictTgp:  'tgp',
	# spotPredictRandomForest: 'randomForest',
	# spotPredictMlegp: 'mlegp',
	# spotCreateDesignFrF2 : 'FrF2',  'DoE.wrapper',
	# spotPredictDiceKriging: ,'DiceKriging' # depreciated
	# spotCreateDesignBasicDoe: 'AlgDesign',
	#
	
	spotInstAndLoadPackages(necessaryPackages)
	
	
}#end spotPrepareSystem

###################################################################################
## SPOT Prepare
###################################################################################
#' Prepares the configuration (spotConfig) for SPOT  
#' 
#' Set some globally important parameters for SPOT to run, creating the
#' parameter list (spotConfig) for SPOT from given usersConfigFile  (.conf -file)
#' 
#' @note For developers: this function also manages the include of all functions needed - 
#' in the packaged version this is already done when package is installed.
#'
#' @param srcPath 			the absolute path to the SPOT sources
#' @param configFile 		the absolute path including filespecifier
#' @param spotConfigUser	a list of parameters used to configure spot, usually  spotConfigUser=NA will be passed to this function, which means the configuration will only be read from the \code{configFile}, not given by manual user input. 		
#'							Notice that parameters given in spotConfigUser will overwrite both default values assigned by SPOT, AND values defined in the config file
#'							However, values not passed by spotConfigUser will still be used as defaults. If you want to see those defaults, look at \code{\link{spotGetOptions}}  
#' @return list \code{spotConfig} \cr
#' - \code{spotConfig} is the list of spot parameters created by this function
#'
#' @references  \code{\link{SPOT}} \code{\link{spotGetOptions}} \code{\link{spot}}
###################################################################################
spotPrepare <- function(srcPath,configFile,spotConfigUser){
	# Close graphic windows	
	graphics.off()
	######################################
	### Load sources
	######################################	
	## Add path to files
	createSourcePath <- function(sourceFileName){
		normalizePath(paste(srcPath,sourceFileName, sep="/"));
	}	
	# the following should never be necessary for "package" version.
	# For the NON-packaged version the R-files are sourced in case the function checked does NOT exist
	# USUALLY the R-file has a function of the same name, BUT!! if you will add another
	# file, please fill the if(!exists("<function>")){ part with a function declared in that file!!!
	if(!exists("spotWriteBest")){
		source(createSourcePath("spotHelpFunctions.R"), local=FALSE);
	}
	if(!exists("spotGetOptions")){
		source(createSourcePath("spotGetOptions.R"), local=FALSE);
	}
	if(!exists("spotPrepareData")){
		source(createSourcePath("spotPrepareData.R"), local=FALSE);
	}
	if(!exists("spotGenerateSequentialDesign")){
		source(createSourcePath("spotGenerateSequentialDesign.R"), local=FALSE);
	}
	if(!exists("spotReportDefault")){
		source(createSourcePath("spotReportDefault.R"), local=FALSE);
	}
	if(!exists("spotGetRepeats")){
		source(createSourcePath("spotStatistics.R"), local=FALSE);
	}
	if(!exists("spotPlotBst")){
		source(createSourcePath("spotPlot.R"), local=FALSE);
	}
	if(!exists("spotMetaRead")){
		source(createSourcePath("spotMeta.R"), local=FALSE);
	}
	if(!exists("spotFuncStartBranin")){
		source(createSourcePath("spotFuncStartBranin.R"), local=FALSE);
	}	
	## everything happens relative to users configuration file
	setwd(dirname(configFile));	
	## Call configuration program that extracts infos from userconf	
	spotConfig <- spotGetOptions(srcPath=srcPath,configFile);
	## MZ 04.09.2010: New feature implemented, so user can set options in commandline when calling  spot()
	if(is.list(spotConfigUser)){
		spotConfig <- append(spotConfigUser,spotConfig); 
		spotConfig <-spotConfig[!duplicated(names(spotConfig))];#Commandline Input from user will overwrite configfile/default parameters here !!
		#TODO: maybe a message to user about overwritten settings		
	}
	return(spotConfig)	
} # end spotPrepare()

###################################################################################
###################################################################################
## PART TWO:  The SPO Steps 
###################################################################################
###################################################################################

###################################################################################
## First Step: Initial
#' SPOT Step: Initial (First SPOT- Step)
#' 
#' Creates a sequential design based on the results derived so far. Therefor it is
#' essential to have another design evaluated before and have a .res file to use.
#' afterwards the design is extended by 4 columns: CONFIG, REPEATS,STEP, SEED 
#'
#' uses the functions \code{spotConfig$init.design.func} and \code{link{spotWriteDes}}
#' that writes a  design to the file <xxx>.des
#' 
#' @param spotConfig the list of all parameters is given, but the used ones are: \cr 
#'   \code{spotConfig$init.design.func} holds the spotCreateDesign<XXX> function to be used 
#'			for building an initial design. \cr 
#'   \code{spotConfig$init.design.size} number of points that should be created for the initial design \cr
#'   \code{spotConfig$init.design.retries} gives the number of trials to find a design with the greates minimal distance, (default is 1)\cr 
#'   \code{spotConfig$init.design.repeats} number of repeats for one initial design-point\cr
#'   \code{spotConfig$io.colname.repeats} column name for data of "spotConfig$init.design.repeats"\cr
#'   \code{spotConfig$io.colname.step} if given a second Column is created \cr
#'   \code{spotConfig$alg.seed} seed value for reproducable runs\cr
#'   \code{spotConfig$srcPath} source path as given when spot() is called (or uses default)\cr
#'   \code{spotConfig$io.verbosity} verbosity for command window output, which is passed to the output function
###################################################################################
spotStepInitial <- function(spotConfig) {	
	## Sets the seed for all random number generators in SPOT
	set.seed(spotConfig$spot.seed) 
	#clear old  data 
	spotConfig$alg.currentResult<-NULL;
	spotConfig$alg.currentBest<-NULL;
	
	spotWriteLines(spotConfig$io.verbosity,2,"Create Inital Design", con=stderr());	
	spotSafelyAddSource(spotConfig$init.design.path,spotConfig$init.design.func,spotConfig$io.verbosity)	
	##
	## write actual region of interest file (same data as roi file)	
	## TODO: Add type information to aroi file
	A <- spotConfig$alg.roi	
	B <- A	
	A <-matrix(as.matrix(A), nrow =length(row.names(A)))
	A <- cbind(row.names(B), A)  
	colnames(A) <- c("name", "low", "high", "type")	
	if(spotConfig$spot.fileMode){ #check if this works TODO
		spotWriteAroi(spotConfig, A)	
	}#else{
	spotConfig$alg.aroi<-A;
	#}	
	initDes<-eval(call(spotConfig$init.design.func, 
					spotConfig,
					spotConfig$init.design.size,
					spotConfig$init.design.retries))
	
	## FIRST COLUMN ADDED: Named "CONFIG" - holding a count variable: 
	## number of the configuration provided
	configNumber<-1:nrow(initDes)
	initDes <- cbind(initDes,configNumber);
	colnames(initDes)[ncol(initDes)] <- "CONFIG";
	
	## SECOND COLUMN ADDED: 
	## number of repeats for the initial design points as "repeats" 
	initDes <- cbind(initDes,spotConfig$init.design.repeats);
	colnames(initDes)[ncol(initDes)] <- spotConfig$io.colname.repeats;
	## <optionally> THIRD COLUMN ADDED: column documenting the number of configurations so far (steps-column)
	## initially the number of steps is 0 (refers to auto.loop.steps)
	if (!is.na(spotConfig$io.colname.step)) {
		initDes <- cbind(initDes,0);
		colnames(initDes)[ncol(initDes)] <- spotConfig$io.colname.step;
	}	
	## FORTH COLUMN ADDED: 
	## Named "SEED" - holding the number of the seed for the randomgenerator
	## used (same seed provides reproducable runs)
	seed <- spotConfig$alg.seed; 
	## could be considering the last used seed according to the last res, 
	## but not yet considered here	
	initDes <- cbind(initDes,seed);
	colnames(initDes)[ncol(initDes)] <- "SEED";	
	if (spotConfig$spot.fileMode){
		if (file.exists(spotConfig$io.desFileName)){
			file.remove(spotConfig$io.desFileName)
		}
		## write the design to a NEW .des-file 
		spotWriteDes(spotConfig,initDes)		
		## Now delete the old .res and .bst files
		if (spotConfig$init.delete.bstFile & file.exists(spotConfig$io.bstFileName)){
			file.remove(spotConfig$io.bstFileName)
		}
		if (spotConfig$init.delete.resFile & file.exists(spotConfig$io.resFileName)){
			file.remove(spotConfig$io.resFileName)
		}
	}#else{
	spotConfig$alg.currentDesign<-initDes;		
	#}	
	return(spotConfig)
}

###################################################################################
## Second Step: Algorithm Call 
###################################################################################
#' SPOT Step Algorithm Call 
#'
#' This is the second SPOT Step after step "initial" - but also needed 
#' after each step "sequential", and is a call frame for the algorithm-call.
#'
#' The algorithm is the heart of what the user must provide, but SPOT should be 
#' able to handle them in the most flexible manner. So this is the interface 
#' providing several possibilities to include an algorithm as (unix)-shell or as 
#' R-function (the latter could also provide a system call, so it should be able
#' to integrate user written algorithms as flexible as possible. 
#' Result/Effects: external, user written function is called and expected to
#' write the results into the file: spotConfig$io.resFileName 
#' 
#' @param spotConfig the list of all parameters is given, but the used ones are:\cr 
#'   \code{spotConfig$alg.language} switches between R-source or (unix)-shell/binary -call \cr
#'   \code{spotConfig$algSourceSrcPath} needed for an R-function call: 
#'		must hold the path to the R-File - either relative to the config-file or
#' 		as absolute path \cr
#'   \code{spotConfig$alg.func} the name of the R-Function (or the binary/shell) accordingly\cr
#'   \code{spotConfig$io.apdFileName} filename for the problem definition of the algorithm, 
#' 			first parameter of the generically defined R-function spotConfig$alg.func \cr
#'   \code{spotConfig$io.desFileName} filename for the input of the algorithm, 
#' 			second parameter of the generically defined R-function spotConfig$alg.func \cr
#'   \code{spotConfig$io.resFileName} filename for the output of the algorithm
#' 			third parameter of the generically defined R-function spotConfig$alg.func\cr
#'   \code{spotConfig$io.verbosity} verbosity for command window output, which is passed to the output function
#'
#' @references  \code{\link{SPOT}} \code{\link{spot}} \code{\link{spotStepInitial}}
#' \code{\link{spotStepSequential}}
####################################################################################
spotStepRunAlg <- function(spotConfig){
	spotWriteLines(spotConfig$io.verbosity,2,paste("spotStepRunAlg started with ",spotConfig$alg.path,spotConfig$alg.func,sep=""))
	# 1) algorithm is encapsulated in an R-file
	if (spotConfig$alg.language=="sourceR"){
		spotSafelyAddSource(spotConfig$alg.path,spotConfig$alg.func,spotConfig$io.verbosity)
		spotConfig<-eval(call(spotConfig$alg.func, spotConfig)) 
	}  else { # spotConfig$alg.language== "unixSh"       
		# 2) algorithm is encapsulated in a (unix)-shell script or a binary
		if(file.exists(spotConfig$alg.func)){#TODO
			if(spotConfig$spot.fileMode){
				retCall<-system(paste(spotConfig$alg.func, spotConfig$io.apdFileName, spotConfig$io.desFileName, spotConfig$io.resFileName))
			}else{
				spotConfig<-system(paste(spotConfig$alg.func, spotConfig)) #TODOMZ not working, no examples? 
			}
		}else{
			spotWriteLines(spotConfig$io.verbosity,0,paste("Error: spot.R::sptStepRunAlg tries to execute non existing file ",spotConfig$alg.func,sep="" ))
		}
	}
	return(spotConfig)
}

###################################################################################
## Third Step: Sequential
#' SPOT Step Sequential
#'
#' Third SPOT Step to generate a sequential new design, this
#' is mainly a call of \code{\link{spotGenerateSequentialDesign}}
#' 
#' Creates a sequential design based on the results derived so far. Therefor it is
#' essential to have another design evaluated before and have a .res file to use.
#' It uses the functions \code{\link{spotGenerateSequentialDesign}} and \code{\link{spotWriteDes}}
#' writes a sequential design to the file <xxx>.des
#' 
#' @param spotConfig the list of all parameters is given, but the used ones are: \cr
#'   \code{spotConfig$io.resFileName} is checked for existence is not, function fails with error\cr
#'   \code{spotConfig$algSourceSrcPath} needed for the error message \cr
#'   \code{spotConfig$userConfFileName} needed for the error message\cr
###################################################################################
spotStepSequential <- function(spotConfig) {
	spotWriteLines(spotConfig$io.verbosity,2,"Create Sequential Design", con=stderr());
	if(spotConfig$spot.fileMode){
		if (!file.exists(spotConfig$io.resFileName)){
			spotWriteLines(spotConfig$io.verbosity,0,"Error in spot.R::spotStepSequential:")
			spotWriteLines(spotConfig$io.verbosity,0,".res file not found, spotStepAlgRun() has to be executed before.")
			spotWriteLines(spotConfig$io.verbosity,0,paste("Try: spot(",spotConfig$userConfFileName,",\"run\",",spotConfig$srcPath , sep=" "))		
		}
	}else{
		if(!nrow(spotConfig$alg.currentResult)>0){
			spotWriteLines(spotConfig$io.verbosity,0,"Error in spot.R::spotStepSequential:")
			spotWriteLines(spotConfig$io.verbosity,0,"result data not found, spotStepAlgRun() has to be executed before.")
			spotWriteLines(spotConfig$io.verbosity,0,paste("Try: spot(",spotConfig$userConfFileName,",\"run\",",spotConfig$srcPath , sep=" "))		
		}
	}
	spotConfig <- spotGenerateSequentialDesign(spotConfig);	
	return(spotConfig)
}

###################################################################################
## Forth Step Report
###################################################################################
#' SPOT Step Report
#'
#' Forth and last step for SPOT, that is by default a call of \link{spotReportDefault}
#' 
#' This step provides a very basic report about the .res-file, based on settings in the \code{spotConfig}
#' The used parameters of \code{spotConfig} are just \code{spotConfig$report.func} and code{spotConfig$report.path}
#' specifying which report shall be called. The user can specify his own report and should set the 
#' values {report.func} and {report.path} in the configuration file according to the specification rules
#' given. If nothing is set, the default report is used.
#' 
#' @param spotConfig the list of all parameters is given, it is forwarded to the call of the reportfunction
#' @references  \code{\link{SPOT}} \code{\link{spot}} \link{spotReportDefault} \code{\link{spotGetOptions}} 
#' 
###################################################################################
spotStepReport <- function(spotConfig) {
	spotSafelyAddSource(spotConfig$report.path,spotConfig$report.func,spotConfig$io.verbosity)
	spotConfig<-eval(call(spotConfig$report.func, spotConfig))
}

###################################################################################
## Step Auto
###################################################################################
#' SPOT Step Auto Opt
#'
#' spotStepAutoOpt is the default task called, when spot is started.  
#' 
#' The \code{auto} task calls the tasks \code{init} and \code{run} once
#' and loops \code{auto.loop.steps} times over the steps \code{seq}  and \code{run}
#' finalising the function with a call of the report function. Instead of \code{auto.loop.steps}
#' also \code{auto.loop.nevals} can be used as a stopping criterion.
#' 
#' @param spotConfig the list of all parameters is given, it is forwarded to the call of the reportfunction
#' the used parameters of spotConfig are just spotConfig$auto.loop.steps
#' specifying the number of meta modells that should be calculated
#' 
#' @references  \code{\link{SPOT}} \code{\link{spot}} \code{\link{spotStepInitial}}
#' \code{\link{spotStepSequential}} \code{\link{spotStepRunAlg}} \code{\link{spotStepReport}} 
#' \code{\link{spotGetOptions}} 
###################################################################################
spotStepAutoOpt <- function(spotConfig){
	spotConfig=spotStepInitial(spotConfig);
	spotConfig=spotStepRunAlg(spotConfig)
	j <-1;
	k <- nrow(spotGetRawDataMatrixB(spotConfig));	
	while (j <= spotConfig$auto.loop.steps && k <= spotConfig$auto.loop.nevals)
	{
		k <- nrow(spotGetRawDataMatrixB(spotConfig));	
		spotWriteLines(spotConfig$io.verbosity,2,paste("SPOT Step:", j), con=stderr());
		spotConfig=spotStepSequential(spotConfig);		
		spotConfig=spotStepRunAlg(spotConfig)
		j <- j+1;
	}
	if(spotConfig$io.verbosity>2){  
		mergedData <- spotPrepareData(spotConfig)
		spotConfig=spotWriteBest(mergedData, spotConfig);	
		spotPlotBst(spotConfig)   
	} 
	spotConfig=spotStepReport(spotConfig); 
	return(spotConfig)
}
###################################################################################
## Step Meta
###################################################################################
#' SPOT Step Meta 
#'
#' The \code{meta} task calls spotStepMetaOpt which itself calls the task \code{auto} 
#' with several different fixed
#' parameters to provide a mixed optimization mechanism: analyse a fully qualified 
#' test of some parameters and the intelligent optimization of other parameters.
#' e.g. the number of the dimension of a problem etc.
#' 
#' To start this step you could for example do this:\cr
#' \code{spot("configFileName.conf","meta")}\cr 
#' An additional \code{<configFileName>.meta} is needed. This meta file should have lines with 
#' vectors of values that are to be given to parameters used in the .apd file. \cr
#' \code{maxit=c(100,200,300,400,500)}\cr
#' \code{x0=list(c(1,1),c(2,2),c(5,5),c(10,10))}\cr
#' will lead to .apd files with each combination for the params starting with:\cr
#' \code{maxit=100}\cr
#' \code{x0=c(1,1)}\cr
#' to\cr
#' \code{maxit=500}\cr
#' \code{x0=c(10,10)}\cr
#' as in this example there are five values for \code{maxit} and four for \code{x0}
#' a set of 20 different projects is evaluated. The results will be written to 
#' \code{<configFileName>.fbs}
#' All temporary results will be deleted by default. If they should be kept fo further 
#' investigation the .conf file should have the additional value
#' \code{meta.keepAllFiles=TRUE} 
#' Then each parameter configuration will be stored like a separate SPOT-project in an own
#' subdirectory. 
#' 
#' @param spotConfig the list of all parameters is given
#' 
#' @references  \code{\link{spotStepAutoOpt}} 
#' \code{\link{spotGetOptions}} 
###################################################################################
## TODO JZ: .meta-file syntax explanation, 
##			 testing
##		adding a usefull example: see /trunk/R.d/testSrc
##		deleting of temporary files, (and a switch to suppress this deleting)
##
spotStepMetaOpt <- function(spotConfig) {
	spotInstAndLoadPackages("AlgDesign")
	# read the Meta File
	writeLines(spotConfig$io.metaFileName)
	myList<-spotMetaRead(spotConfig$io.metaFileName)
	myAssignmentOp<-spotMetaGetAssignmentOp(spotConfig$io.metaFileName)# assignment operator
	#browser()
	nVars<-length(myList)
	# create a vector "x" holding the length of each variable
	x <- as.numeric(lapply(myList, length))
	# full factorial design with indicies for all combinations:
	if (nVars==1){
		dat <- matrix(1:x, byrow = TRUE)
	}
	else{
		dat<-gen.factorial(x,varNames=names(myList),factors="all")
	}
	## Loop over full factorial combinations of all parameters specified in .meta
	for (j in 1:nrow(dat)) {
		## close all remaining graphic devices - from old spotStepAutoOpt Runs
		graphics.off() 
		
		# empty the vectors for projectName and apdLines that are to be added to the apdFile
		# dependent to the factors of that one row of dat[j]
		projectName<-character()  
		apdLines<-character()
		myFbs<-list()
		
		for (k in 1:nVars) {
			# left side of the  assignment 
			## the factorial value of the kth variable for this dat[j]-row is assigned to a character variable:
			kthFactor <- myList[[k]][dat[[j,k]]]
			kthFactorName <- as.character(kthFactor)
			# projectName is generated from these Factors:
			projectName <- paste(projectName,names(myList)[k],kthFactorName,sep="")
			# adapt the factor for proper writing, if it is a character it needs quotes \"
			if((class(myList[[k]])=="list" && class(myList[[k]][[dat[[j,k]]]])=="character")||(class(myList[[k]])=="character"))
				kthFactorName <- paste("\"",kthFactorName,"\"",sep="")
			# now create a <varname>=<singleValue> line for the apd-file 
			lVal <- paste(names(myList)[k],myAssignmentOp,kthFactorName,sep="")
			# and a list of all the variables to be added to .fbs file
			myFbs <- c(myFbs, list(kthFactor))
			names(myFbs)[length(myFbs)] <- names(myList)[k]
			## add the created line to the list of apd lines for this one dat-row: (a single spot-"auto" run)
			apdLines <- (c(apdLines,lVal))
			if (k!=nVars){
				projectName <- paste(projectName,"_",sep="")
			}
		}
		## NOW: all values are generated for ONE run 
		oldDir<-getwd()
		## create a temporary spotConfig for the calling of spotStepAuto 
		newSpotConfig<-spotMetaCreateSubProject(spotConfig,projectName,apdLines)
		###############   THIS calls the spotAutoOpt for ONE line
		newSpotConfig=spotStepAutoOpt(newSpotConfig)
		#now transfer the last line of the bst-file to the parent directory
		################
		#if(spotConfig$spot.fileMode){
		#	tmpBst<-read.table(newSpotConfig$io.bstFileName,header=TRUE,sep=" ");
		#}else{
		tmpBst<-newSpotConfig$alg.currentBest;
		#}
		################
		# remove files if not necessary
		if (!spotConfig$meta.keepAllFiles) 
			file.remove(dir())
		setwd(oldDir)
		if (!spotConfig$meta.keepAllFiles) {
			conFilePrefix <- unlist(strsplit(basename(spotConfig$configFileName), ".", fixed = TRUE))[1]
			myProject<-paste(conFilePrefix,projectName,sep="_")
			file.remove(myProject)
		}
		myFbsFlattened <- spotMetaFlattenFbsRow(myFbs)
		
		if(!file.exists(spotConfig$io.fbsFileName)) {
			colNames=TRUE
		} else  colNames=FALSE
				
		write.table(file=spotConfig$io.fbsFileName,
				cbind(tmpBst[nrow(tmpBst),],myFbsFlattened),
				row.names = FALSE,
				col.names = colNames,
				sep = " ",
				append = !colNames,
				quote=FALSE)
				
	} # for (j in 1:nrow(dat))... (loop over full factorial design)
	spotSafelyAddSource(spotConfig$report.meta.path,spotConfig$report.meta.func,spotConfig$io.verbosity)
	retCall<-eval(call(spotConfig$report.meta.func, spotConfig))
	#return(spotConfig) #TODOMZ
}

############# end function definitions ############################################################

###################################################################################################
## PART THREE: SPOT: The Programm
###################################################################################################
#' Main function for the use of SPOT
#' 
#' Sequential Parameter Optimization Toolbox (SPOT) provides a toolbox for the 
#' sequential optimization of parameter driven tasks. 
#' MUST be called with at least the first parameter specified (configFile)
#'
#' The path given with the \code{userConfigFile} also fixes the working directory used
#' throughout the run of all SPOT functions. All files that are needed for input/output
#' can and will be given relative to the path of the userConfigFile (this also holds for 
#' the binary of the algorithm). This refers to files that are specified in the configFile
#' by the user. 
#'
#' @param configFile	the absolute path including filespecifier, there is no default, this value should always be given
#' @param spotTask		[init|seq|run|auto|rep] the switch for the tool used, default is "auto" 
#' @param srcPath		the absolute path to user written sources that extend SPOT, the default(NA) will search for sources in the path <.libPath()>/SPOT/R  
#' @param spotConfig	a list of parameters used to configure spot, default is spotConfig=NA, which means the configuration will only be read from the \code{configFile}, not given by manual user input. 		
#'						Notice that parameters given in spotConfig will overwrite both default values assigned by SPOT, AND values defined in the Config file
#'						However, values not passed by spotConfig will still be used as defaults. If you want to see those defaults, look at \code{\link{spotGetOptions}}  
#' @note \code{spot()} expects char vectors as input, e.g. \code{spot("c:/configfile.conf","auto")}
#' @references  \code{\link{SPOT}} \code{\link{spot}} \code{\link{spotStepAutoOpt}}  \code{\link{spotStepInitial}}
#' \code{\link{spotStepSequential}} \code{\link{spotStepRunAlg}} \code{\link{spotStepReport}} 
#' \code{\link{spotPrepare}} \code{\link{spotPrepareSystem}}  
###################################################################################################
spot <- function(configFile,spotTask="auto",srcPath=NA,spotConfig=NA){
	writeLines("spot.R::spot started ")
	callingDirectory<-getwd()	
	if(is.na(srcPath)){
		for(k in 1:length(.libPaths())){ 
			if(file.exists(paste(.libPaths()[k],"SPOT","R",sep="/"))){
				srcPath<-(paste(.libPaths()[k],"SPOT","R",sep="/"))
				break;
			}
		}
	}
	## PRELIMINARIES 1: load all functions belonging to SPOT - not necessary if provided SPOT is installed as package - useful for developers...
	spotConfig<-spotPrepare(srcPath,configFile,spotConfig)
	## PRELIMINARIES 2: dynamic install of packages used - omitted for package-use
	# spotPrepareSystem() 
	## SWITCH task according to the extracted from command line 
	resSwitch <- switch(spotTask
			, init=, initial=spotStepInitial(spotConfig) # First Step
			, seq=, sequential=spotStepSequential(spotConfig) # Second Step
			, run=, runalg=spotStepRunAlg(spotConfig)	# Third Step
			, rep=, report=spotStepReport(spotConfig)		# Fourth Step
			, auto=, automatic=spotStepAutoOpt(spotConfig)	# Automatically call First to Forth Step
			, meta=spotStepMetaOpt(spotConfig)	# Automatically call several spotStepAutoOpt - Runs to provide a systematic testing tool an fixed Parameters in .apd file	
			, "invalid switch" # return this at wrong CMD task
	);
	## ERROR handling 
	## valid switch returns null, otherwise show error warning and short help
	if (is.character(resSwitch) && resSwitch == "invalid switch") {
		spotWriteLines(spotConfig$io.verbosity,0,paste("ERROR, unknown task:", spotTask), con=stderr());
		spotWriteLines(spotConfig$io.verbosity,0,"\nValid tasks are:\
						auto       - run tuning in automated mode\
						initial    - to create an initial design\
						run        - start the program, algorithm, simulator\
						sequential - to create further design points\
						report     - to generate a report from your results"
				, con=stderr());
	}
	# go back to -  well where ever you came from
	setwd(callingDirectory)
	return(resSwitch);
}

