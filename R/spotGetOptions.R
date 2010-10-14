## Experimental research in evolutionary computation
## author: thomas.bartz-beielstein@fh-koeln.de
## http://www.springer.com/3-540-32026-1
##
## Copyright (C) 2009 T. Bartz-Beielstein and C. Lasarczyk
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
###################################################################################################
#' spotGetOptions: set all options by conf-file or by default
#' 
#' spotGetOptions \cr
#' 1.) sets default values \cr 
#' 2.) overwrites all default values by the settings the user provides with the config file (.conf-file) \cr
#' All options described here, that are not marked as "internal variable" may be changed 
#' by the user. This will be done by reading the ".conf"-file that the user has specified 
#' as the first (and maybe sole) parameter to the function spot(). 
#' To change this default value of a variable, simply write a line into the ".conf"-file following
#' this syntax:\cr
#' <variable>=<value> e.g.: \code{spot.seed=54321}\cr
#' This function will do even more: the user may define his own variables in the .conf-file and may use them
#' in user written plugins. All plugins will get the whole list of options with the parameter 
#' "spotConfig". As a result a variable given in the .conf file as \cr 
#' \code{my.var=37} \cr
#' may be refered to by spotConfig$my.var and can be used in all functions - especially in the functions 
#' that are designed to be open to adaptions where ever necessary. 
#' 
#' @param srcPath the absolute path to the SPOT sources
#' @param configFileName users config file (.conf) the absolute path including filespecifier of the user config File
#' @return spotGetOptions returns the list of all SPOT options  created by this function:
#' 			\item{srcPath}{[see 3rd Parameter of \link{spot}] internal variable: global path to all user written sources that should be added to SPOT.} 
#' 			\item{auto.loop.steps}{[\code{1}] number of iterations the loop over all SPOT-steps should be repeated}
#' 			\item{auto.loop.nevals}{[\code{Inf}] budget  of algorithm/simulator runs
#' 			 - most important parameter for run-time of the algorithm in case the spot-function is called with the "auto"-task }
#' 			\item{spot.fileMode}{[\code{TRUE}] boolean, that defines if files are used to read and write results (which is the "classic" spot procedure) or if SPOT will only use the workspace to store variables.}
#' 			\item{spot.seed}{[\code{123}] global seed setting for all random generator dependent calls within SPOT. same seed shall repeat same results, \cr
#' 						BUT: please note: this is NOT the seed for the algorithm!!! see alg.seed}
#' 			\item{alg.language}{[\code{"sourceR"}] language the algorithm is written in. Can either be "sourceR" or "unixSh". 
#' 						We rcommend to use an R-function and see the examples for how best several sources may be called with a wrapper written in R}
#' 			\item{alg.func}{[\code{"spotFuncStartBranin"}] the function-name of the algorithm.} 
#' 			\item{}{The interpretation and handling strongly depends on the setting of the parameter \code{alg.language}: \cr 
#' 						if alg.language="sourceR" SPOT searches for a file <alg.func>.R in the <alg.path>\cr
#' 						if alg.language="unixSh" the name will be used without any changes in a system call}
#' 			\item{alg.path}{[\code{NA} defaults to \code{srcPath}] path to find the R-source for the <alg.func>}
#' 			\item{alg.resultColumn}{[\code{"Y"}] string to indicate the name of the result column.  }
#' 			\item{alg.seed}{[\code{1234}] seed for random generator to be used by the user defined algorithm. 
#'						This is needed to reproduce the results }
#' 			\item{alg.roi}{internal parameter for the initial region of interest (do not try to set this one, it will be overwritten with default values).
#'                       It is used to provide an easy to use matrix with the data from the  ".roi"-file (= Region Of Interest)}
#' 			\item{alg.aroi}{internal parameter for the actual region of interest (do not try to set this one, it will be overwritten with default values). 
#' 							It is used to provide an easy to use matrix with the data from the  ".aroi"-file (= Actual Region Of Interest)}
#' 			\item{alg.currentDesign}{[usually not changed by user] data frame of the design that will be evaluated by the next call to \link{spotStepRunAlg}} 
#' 			\item{alg.currentResult}{[usually not changed by user] data frame that contains the results of the target algorithm runs} 
#' 			\item{alg.currentBest}{[usually not changed by user] data frame that contains the best results of each step conducted by spot} 
#' 			\item{io.columnSep}{[\code{""}] column seperator for the input/output files, default means: arbitrary whitespace sequence, 
#' 							should be set by the value you want to have between your columns}
#' 			\item{io.colname.repeats}{[\code{"REPEATS"}] string holding the name of the column in the .des-file that indicates how much (randomly different) repeats with a design should be performed by the algorithm }
#' 			\item{io.colname.step}{[\code{"STEP"}] string holding the name of the column documenting the number of different configurations so far (steps-column) }
#' 			\item{io.apdFileName}{[depends: \code{<configFileName>.apd}] name of the .apd -file (Algorithm Problem Definition file, holding all specification the user written algorithm needs to perfom a complete optimization)}
#' 			\item{io.roiFileName}{[depends: \code{<configFileName>.roi}] name of the .roi -file (Region Of Interest - File, holding all varying parameters and constraints)}
#' 			\item{io.desFileName}{[depends: \code{<configFileName>.des}] name of the .des -file (DESign file, the file the user written algorithm uses as input to the parameters it should change)}
#' 			\item{io.resFileName}{[depends: \code{<configFileName>.res}] name of the .res -file (RESult file) the user written algorithm has to write its results into this file }
#' 			\item{io.bstFileName}{[depends: \code{<configFileName>.bst}] name of the .bst -file (BeST file) the result-file will be condensed to this file }
#' 			\item{io.pdfFileName}{[depends: \code{<configFileName>.pdf}] name of the .pdf -file the default report will write its summary of results in this pdf file }
#' 			\item{io.metaFileName}{[depends: \code{<configFileName>.meta}] name of the .meta -file, a file holding additional parameters for a full-factorial test-field starting several "auto" tasks}
#' 			\item{io.fbsFileName}{[depends: \code{<configFileName>.bst}] name of the .fbs -file (Final BestSolution file) collects all final best values of all .bst files during a .meta-run }
#' 			\item{io.verbosity}{[\code{3}] level of verbosity of the programm, 0 should be silent and 3 should produce all output- sometimes just interesting for the developer...}
#' 			\item{init.design.func}{[\code{"spotCreateDesignLhs"}] name of the function to create an initial design. Please also see the notes SPOT - extensions}
#' 			\item{init.design.path}{[\code{NA} defaults to \code{srcPath}] path where to find the <init.design.func>.R -file}
#' 			\item{init.design.size}{[\code{NA}] number of initial design points to be created. Required by some space filling desing generators. Will be used in the <init.design.func>.R-file.  Default (NA) will start a formula to calculate a good value.}
#' 			\item{init.design.retries}{[\code{100}] number of retries the initial designs should be retried to find randomly a design with maximum distance between the points 
#' 								This parameter will be ignored if the function is deterministic (like doe)}
#' 			\item{init.design.repeats}{[\code{1}] number of repeats for each design point to be called with the <alg.func>}
#' 			\item{init.delete.resFile}{[\code{TRUE}] delete an existing resultfile }
#' 			\item{init.delete.bstFile}{[\code{TRUE}] delete an existing bst file }
#' 			\item{design.paramSignif}{[\code{NA}] number of significant digits that should be considered in the design process of building an initial AND a sequential design}
#' 			\item{seq.design.size}{[\code{10000}] number of sequential design points to be created}
#' 			\item{seq.design.retries}{[\code{10}] number of retries the initial designs should be retried to find randomly a design with maximum distance between the points,  
#' 								This parameter will be ignored if the function is deterministic (like doe)}
#' 			\item{seq.design.oldBest.size}{[\code{1}] number of the best already evaluated design points that should be taken into consideration for the next sequential designs (e.g. for to have an appropriate number of repeats}
#' 			\item{seq.design.new.size}{[\code{2}] according to the predictor the new design pounts during the seq step are ordered by their expected values. This parameter states how much new design points should be evaluated}
#' 			\item{seq.design.maxRepeats}{[\code{NA}] each design point is to be evaluated several times for statistically sound results. The number of "repeats" will increase, but will not exceed this seq.design.maxRepeats - value }
#' 			\item{seq.design.increase.func}{[\code{"spotSeqDesignIncreasePlusOne"}] functional description of how the repeats are increased (until the seq.design.maxRepeats are reached). Default increases the number of repeats by adding one.}
#' 			\item{seq.design.increase.path}{[\code{NA} defaults to \code{srcPath}] path where to find the <seq.design.increase.func>.R -file. Please also see the notes SPOT - extensions}
#' 			\item{seq.design.func}{[\code{"spotCreateDesignLhs"}] name of the function to create sequential design. Please also see the notes SPOT - extensions}
#' 			\item{seq.design.path}{[\code{NA} defaults to \code{srcPath}] path where to find the <seq.design.func>.R -file}
#' 			\item{seq.predictionModel.func}{[\code{"spotPredictLm"}] name of the function calling a predictor. Default uses a Linear Model. Please also see the notes SPOT - extensions}
#' 			\item{seq.predictionModel.path}{[\code{NA} defaults to \code{srcPath}] path where to find the <seq.predictionModel.func>.R -file}
#' 			\item{seq.log.x}{[\code{FALSE}] use log(x) values for the prediction model}
#' 			\item{seq.log.y}{[\code{FALSE}] use log(y) values for the prediction model}
#' 			\item{seq.merge.func}{ [\code{mean}] defines the function that merges the results from the different repeat-runs for a design. Default is to calculate the mean value.}
#' 			\item{seq.transformation.func}{[\code{I}] function for transformation of "Y" before new model is created, default: Identitity function}
#' 			\item{seq.useGradient}{[\code{FALSE}] use gradient information for the prediction model}
#' 			\item{seq.useCanonicalPath}{[\code{FALSE}] if gradient information is used, start at saddle point and follow the most steeply rising ridge in both directions. Default: start at origin and follow the path of the steepest descent in one direction}
#' 			\item{seq.useAdaptiveRoi}{[\code{FALSE}] use region of intereset adaptation}
#' 			\item{meta.keepAllFiles}{[\code{FALSE}] Meta optimization produces lots of temporary files - to keep these files for analysis this switch must be set to TRUE}
#'			\item{report.func}{[\code{"spotReportDefault"}] name of the function providing the report (default="spotReportDefault" Please also see the notes SPOT - extensions }
#'			\item{report.meta.func}{[\code{"spotReportMetaDefault"}] name of the function providing the report for meta runs, do not use these functions for \code{report.func}.}
#'			\item{report.path}{[\code{NA} defaults to \code{srcPath}] path where to find the <report.func>.R -file}
#'			\item{report.meta.path}{[\code{NA} defaults to \code{srcPath}] path where to find the <report.meta.func>.R -file}
#' 			\item{report.hist}{[\code{0}] report should hold a histogramme (0=no, 1=yes)}
#' 			\item{report.scatter}{[\code{0}] report should hold a scatterplot (0=no, 1=yes)}
#' 			\item{report.io.screen}{[\code{FALSE}] report graphics will be printed to screen (FALSE=no, TRUE=yes)}
#' 			\item{report.io.pdf}{[\code{TRUE}] report graphics will be printed to pdf (FALSE=no, TRUE=yes)}
#' 			\item{report.io.maxPlots}{[\code{11}] a plot of the performance is continuosly updated, showing the .roi variables in the order given in that file. The graphic is limited to 11 variables. A smaller value here may focus to the relevant variables only}
#' @references  \code{SPOT} \code{spotPrepare}

## hint for programmers: 
## ALL Variables defined in this function AND by the sourced .conf-file will be populated to the resulting list
## that is returned and used throughout the program as "spotConfig". This function just sets some defaults
## if a really temporary variable is needed for finally calculate a variable worth to be added to the list
## you must declare this "local" variable by a leading dot: e.g.  .dataPath 
##


spotGetOptions <- function( srcPath=".",configFileName) {
	writeLines("spotGetOptions... started", con=stderr());	
	#######################################
	### Begin: Algorithm design related ###
	#######################################	
	## Specify language of algorithm to be tuned.
	## Type: STRING:
	## Can be one of the following:
	## 1) sourceR, i.e., R script to be called by source("alg.func")
	## 2) unixSh, i.e., unix Shell script, to be called by system("alg.func") 
	alg.language = "sourceR"	
	## to specify the additional R source file for the algorithm 
	## two information my be given: the path to the R-file
	## and the functionname. The R-file now
	## 1) MUST be located in the directory given (alg.path) - default is srcPath
	## 2) MUST have the same name as the function (PLUS the extension ".R")
	## 3) MUST be given in alg.func
	## Type: STRING:
	## This R script will be sourced by source(paste(alg.path,alg.func,sep="/"))
	alg.path = NA;	
	## Specify name of the function
	## this may be 
	## 1) (in case "alg.language==sourceR") a function in an R-file, 
	## then please consider the notes above about the alg.path
	## OR 
	## 2) (in case "alg.language==unixSh") holds the (and full absolute path) of the algorithm to be tuned.
	## This unixSh can be either:
	## 1) a shell script (Unix)
	## 2) a .bat file (Win)
	## Type: STRING
	alg.func = "spotFuncStartBranin"	
	## Column name containing results
	alg.resultColumn = "Y"
	alg.seed <- 1234;	
	## ##########################
	## ##### Init related   #####
	## ##########################	
	## design create for initial step
	init.design.path = NA
	init.design.func = "spotCreateDesignLhs"	
	## Initial number of design points, 
	## if NA, formula will be used
	# number of initial design points
	init.design.size <- NA;
	# repeats for improving min-max designs (not used by doe)
	init.design.retries <- 100;
	# number of repeated runs of each configuration:
	init.design.repeats <- 1;
	# keep or delete existing resultfile? (default: TRUE)
	init.delete.resFile <- TRUE
	# keep or delete existing bstfile? (default: TRUE)
	init.delete.bstFile <- TRUE	
	## #################################
	## ##### Sequential Step related ###
	## #################################
	## Function for a summary of the results from the algorithm at each design point
	seq.merge.func <- mean;	
	## function for transformation of "Y" before new model is created, default: Identitity function
	seq.transformation.func <- I;
	seq.log.x <- FALSE;
	seq.log.y <- FALSE;	
	## design create for sequentiel step: (must guarantee a higher search space)
	seq.design.path = NA
	seq.design.func = "spotCreateDesignLhd"	
	seq.design.size <- 200;
	seq.design.retries <- 10;
	seq.design.maxRepeats <- NA;
	seq.design.increase.func <- "spotSeqDesignIncreasePlusOne"
	seq.design.increase.path <- NA
	## how many old (best) points shall be repeated 
	seq.design.oldBest.size <- 1;
	## how many new points shall be added 
	seq.design.new.size <- 2;
	## ###################################
	## ##### Prediction Modell related ###
	## ###################################	
	seq.predictionModel.path = NA
	seq.predictionModel.func = "spotPredictLm";
	#seq.predictionModel.func = "spotPredictTree";	
	#seq.predictionModel.func = "spotPredictMlegp";	
	#seq.predictionModel.func = "spotPredictRandomForrest";	
	#seq.predictionModel.func = "spotPredictTgp";	
	#seq.predictionModel.func = "spotPredictLmOptim";
	seq.useGradient = FALSE
	seq.useAdaptiveRoi = FALSE
	seq.useCanonicalPath = FALSE
	## #####################################
	## ##### Globally needed           #####
	## #####################################
	spot.seed <- 123
	
	## How many spot iterations should be performed?
	auto.loop.steps <- 1;
	auto.loop.nevals <- Inf;
	## number of signicient digits of the design parameter (BOTH initial AND sequential design)
	design.paramSignif <- NA;	
	## #####################################
	## ##### Step      meta            #####
	## #####################################
	meta.keepAllFiles = FALSE
	## ####################################################################################################
	## ##### IO related (files for input and output, and variables to specify formatting of IO-files  #####
	## ####################################################################################################
	## TBB: 24 2 2009:
	io.colname.repeats <- "REPEATS";	
	## Name of the column, where the actual auto.loop.step will be stored in the res file
	io.colname.step <- "STEP";	
	## io.verbosity 0 means be quit, 3 means tell me everything
	io.verbosity<-3	
	## Separation of columns, default: arbitrary Whitespacesequenz
	io.columnSep = "";
	## ###########################
	## ##### Report related #####
	## ###########################
	## added report.func for the report step 
	## set default values to "spotDefaultReport" 
	report.path = NA
	report.meta.path = NA
	report.func = "spotReportDefault";
	report.meta.func = "spotReportMetaDefault";
	### generate simple histogram in report (0=no, 1 = yes, default =0):
	report.hist = 0;
	### generate simple scatterplot in report (0=no, 1 = yes, default =0):
	report.scatter = 0;	
    ## Should graphical output be generated in a pdf File? FALSE  = NO
	# implemented only for default report
	report.io.pdf<-FALSE
	## Should graphical output be generated on screen? FALSE  = NO
	# implemented only for default report
	report.io.screen<-TRUE
	## reduce the number of relevant variables for the best-plot (that is also shown continuously)
	report.io.maxPlots<-11
	## New variable:
	spot.fileMode=TRUE;
	##########################################################################
	.dataPath <- dirname(configFileName);
	writeLines(paste("  Data Path (all experiment data are relevant to this location): "
					, .dataPath
					, collapse="")
					, con=stderr());
	.genericFileNamePrefix <-  unlist(strsplit(basename(configFileName), ".", fixed = TRUE))[1]
	writeLines(paste("  File name prefix: "
				, .genericFileNamePrefix
				, collapse="")
				, con=stderr());
	io.resFileName <- paste(.genericFileNamePrefix,"res",sep=".")
	io.desFileName <- paste(.genericFileNamePrefix,"des",sep=".")
	io.bstFileName <- paste(.genericFileNamePrefix,"bst",sep=".")
	io.pdfFileName <- paste(.genericFileNamePrefix,"pdf",sep=".")	
	io.roiFileName <- paste(.genericFileNamePrefix,"roi",sep=".")
	io.aroiFileName <- paste(.genericFileNamePrefix,"aroi",sep=".")
	io.apdFileName <- paste(.genericFileNamePrefix,"apd",sep=".")
	io.metaFileName <- paste(.genericFileNamePrefix,"meta",sep=".")
	io.fbsFileName <- paste(.genericFileNamePrefix,"fbs",sep=".")
	
	############################################################################
	### load user settings, this overwrite the defaults that are set up to this line
	### the basename of the configFile was used in the main 
	############################################################################
	userConfFileName  <-  basename(configFileName);
	.lsBeforeSource<-ls()
	#################################################################
	### load configuration
	#################################################################
	source(userConfFileName, local=TRUE); # ! otherwise default values will not be overwritten
	#################################################################
	### load configuration done!!!!!!!
	#################################################################
	.lsAfterSource<-ls()
	### give some warnings if NEW variables are created by the conf-file
	if (length(.lsDiff<-setdiff(.lsAfterSource,.lsBeforeSource))){
		writeLines(paste("WARNING, a new variable defined by conf-file (",userConfFileName,"):",.lsDiff))
	}	else{
		writeLines(paste("  User conf loaded from: "
						, userConfFileName
						, collapse="")
				, con=stderr());
	}	
	### io.resFileName <- normalizePath(paste(dataPath,io.resFileName, sep="/"));
	writeLines(paste("  ResultFile Name : ", io.resFileName, collapse="")
			, con=stderr());
	### io.desFileName <- normalizePath(paste(dataPath,io.desFileName, sep="/"));
	writeLines(paste("  DesignFile Name : ", io.desFileName, collapse="")
			, con=stderr());
	### io.bstFileName <- normalizePath(paste(dataPath,io.bstFileName, sep="/"));
		writeLines(paste("  BestFile Name : ", io.bstFileName, collapse="")
			, con=stderr());
	### io.fbsFileName <- normalizePath(paste(dataPath,io.fbsFileName, sep="/"));
	writeLines(paste("  FinalBestSolution FbsFile Name : ", io.fbsFileName, collapse="")
			, con=stderr());
	## TBB: Added 26 Feb 2009:
	### io.pdfFileName <- normalizePath(paste(dataPath,io.pdfFileName, sep="/"));
	writeLines(paste("  pdfFile Name : ", io.pdfFileName, collapse="")
			, con=stderr());
		
	writeLines(paste("  Load algorithm design (ROI): ", io.roiFileName, collapse="")
			, con=stderr());
	## alg.roi is a table that holds the data of the .roi-file for easy and quick use in some functions 
	alg.roi <- read.table( io.roiFileName
			, sep = io.columnSep
			, header = TRUE
			, as.is=TRUE
			, row.names = 1 #Parameter als Zeilennamen
	);
	## at startup, the actual roi (alg.aroi) is the same as the initial roi (alg.roi)
	alg.aroi <- alg.roi
	#}
		
	##
	##check the paths given to the user defined R-functions
	##(or better:  the ones the user MAY define if he wishes) 
	## IF he has not defined a path, then default srcPath will be used
	if(is.na(alg.path)){
		alg.path = srcPath ;
	}
	if(is.na(init.design.path)){
		init.design.path = srcPath ;
	}
	if(is.na(seq.design.path)){
		seq.design.path = srcPath ;
	}
	if(is.na(seq.design.increase.path)){
		seq.design.increase.path = srcPath ;
	}
	if(is.na(seq.predictionModel.path)){
		seq.predictionModel.path = srcPath ;
	}
	if(is.na(report.path)){
		report.path = srcPath ;
	}
	if(is.na(report.meta.path)){
		report.meta.path = srcPath ;
	}
	writeLines("spotGetOptions finished", con=stderr());
	## generate a list of ALL the defined variables (default AND user written = sourced by .conf-file!)
	## ls returns a list of all variables in this environment, that is: all the "local" variables (except the ones with leading dot
	.x<-ls();
	## now use sapply to generate the list of name/value of all variables. 
	spotConfig<-sapply(.x, function (.x) { get(.x)}, USE.NAMES=TRUE)
	## and return this list:
#	######################################
#	### Return configuration list
#	######################################
	return(spotConfig)
}