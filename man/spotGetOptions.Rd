\name{spotGetOptions}
\alias{spotGetOptions}
\title{spotGetOptions: set all options by conf-file or by default...}
\usage{spotGetOptions(srcPath=".", configFileName)}
\description{spotGetOptions: set all options by conf-file or by default}
\details{spotGetOptions \cr
1.) sets default values \cr 
2.) overwrites all default values by the settings the user provides with the config file (.conf-file) \cr
All options described here, that are not marked as "internal variable" may be changed 
by the user. This will be done by reading the ".conf"-file that the user has specified 
as the first (and maybe sole) parameter to the function spot(). 
To change this default value of a variable, simply write a line into the ".conf"-file following
this syntax:\cr
<variable>=<value> e.g.: \code{spot.seed=54321}\cr
This function will do even more: the user may define his own variables in the .conf-file and may use them
in user written plugins. All plugins will get the whole list of options with the parameter 
"spotConfig". As a result a variable given in the .conf file as \cr 
\code{my.var=37} \cr
may be refered to by spotConfig$my.var and can be used in all functions - especially in the functions 
that are designed to be open to adaptions where ever necessary.}
\value{spotGetOptions returns the list of all SPOT options  created by this function:
\item{srcPath}{[see 3rd Parameter of \link{spot}] internal variable: global path to all user written sources that should be added to SPOT.} 
\item{auto.loop.steps}{[\code{1}] number of iterations the loop over all SPOT-steps should be repeated}
\item{auto.loop.nevals}{[\code{Inf}] budget  of algorithm/simulator runs
- most important parameter for run-time of the algorithm in case the spot-function is called with the "auto"-task }
\item{spot.fileMode}{[\code{TRUE}] boolean, that defines if files are used to read and write results (which is the "classic" spot procedure) or if SPOT will only use the workspace to store variables.}
\item{spot.seed}{[\code{123}] global seed setting for all random generator dependent calls within SPOT. same seed shall repeat same results, \cr
BUT: please note: this is NOT the seed for the algorithm!!! see alg.seed}
\item{alg.language}{[\code{"sourceR"}] language the algorithm is written in. Can either be "sourceR" or "unixSh". 
We rcommend to use an R-function and see the examples for how best several sources may be called with a wrapper written in R}
\item{alg.func}{[\code{"spotFuncStartBranin"}] the function-name of the algorithm.} 
\item{}{The interpretation and handling strongly depends on the setting of the parameter \code{alg.language}: \cr 
if alg.language="sourceR" SPOT searches for a file <alg.func>.R in the <alg.path>\cr
if alg.language="unixSh" the name will be used without any changes in a system call}
\item{alg.path}{[\code{NA} defaults to \code{srcPath}] path to find the R-source for the <alg.func>}
\item{alg.resultColumn}{[\code{"Y"}] string to indicate the name of the result column.  }
\item{alg.seed}{[\code{1234}] seed for random generator to be used by the user defined algorithm. 
This is needed to reproduce the results }
\item{alg.roi}{internal parameter for the initial region of interest (do not try to set this one, it will be overwritten with default values).
It is used to provide an easy to use matrix with the data from the  ".roi"-file (= Region Of Interest)}
\item{alg.aroi}{internal parameter for the actual region of interest (do not try to set this one, it will be overwritten with default values). 
It is used to provide an easy to use matrix with the data from the  ".aroi"-file (= Actual Region Of Interest)}
\item{alg.currentDesign}{[usually not changed by user] data frame of the design that will be evaluated by the next call to \link{spotStepRunAlg}} 
\item{alg.currentResult}{[usually not changed by user] data frame that contains the results of the target algorithm runs} 
\item{alg.currentBest}{[usually not changed by user] data frame that contains the best results of each step conducted by spot} 
\item{io.columnSep}{[\code{""}] column seperator for the input/output files, default means: arbitrary whitespace sequence, 
should be set by the value you want to have between your columns}
\item{io.colname.repeats}{[\code{"REPEATS"}] string holding the name of the column in the .des-file that indicates how much (randomly different) repeats with a design should be performed by the algorithm }
\item{io.colname.step}{[\code{"STEP"}] string holding the name of the column documenting the number of different configurations so far (steps-column) }
\item{io.apdFileName}{[depends: \code{<configFileName>.apd}] name of the .apd -file (Algorithm Problem Definition file, holding all specification the user written algorithm needs to perfom a complete optimization)}
\item{io.roiFileName}{[depends: \code{<configFileName>.roi}] name of the .roi -file (Region Of Interest - File, holding all varying parameters and constraints)}
\item{io.desFileName}{[depends: \code{<configFileName>.des}] name of the .des -file (DESign file, the file the user written algorithm uses as input to the parameters it should change)}
\item{io.resFileName}{[depends: \code{<configFileName>.res}] name of the .res -file (RESult file) the user written algorithm has to write its results into this file }
\item{io.bstFileName}{[depends: \code{<configFileName>.bst}] name of the .bst -file (BeST file) the result-file will be condensed to this file }
\item{io.pdfFileName}{[depends: \code{<configFileName>.pdf}] name of the .pdf -file the default report will write its summary of results in this pdf file }
\item{io.metaFileName}{[depends: \code{<configFileName>.meta}] name of the .meta -file, a file holding additional parameters for a full-factorial test-field starting several "auto" tasks}
\item{io.fbsFileName}{[depends: \code{<configFileName>.bst}] name of the .fbs -file (Final BestSolution file) collects all final best values of all .bst files during a .meta-run }
\item{io.verbosity}{[\code{3}] level of verbosity of the programm, 0 should be silent and 3 should produce all output- sometimes just interesting for the developer...}
\item{init.design.func}{[\code{"spotCreateDesignLhs"}] name of the function to create an initial design. Please also see the notes SPOT - extensions}
\item{init.design.path}{[\code{NA} defaults to \code{srcPath}] path where to find the <init.design.func>.R -file}
\item{init.design.size}{[\code{NA}] number of initial design points to be created. Required by some space filling desing generators. Will be used in the <init.design.func>.R-file.  Default (NA) will start a formula to calculate a good value.}
\item{init.design.retries}{[\code{100}] number of retries the initial designs should be retried to find randomly a design with maximum distance between the points 
This parameter will be ignored if the function is deterministic (like doe)}
\item{init.design.repeats}{[\code{1}] number of repeats for each design point to be called with the <alg.func>}
\item{init.delete.resFile}{[\code{TRUE}] delete an existing resultfile }
\item{init.delete.bstFile}{[\code{TRUE}] delete an existing bst file }
\item{design.paramSignif}{[\code{NA}] number of significant digits that should be considered in the design process of building an initial AND a sequential design}
\item{seq.design.size}{[\code{10000}] number of sequential design points to be created}
\item{seq.design.retries}{[\code{10}] number of retries the initial designs should be retried to find randomly a design with maximum distance between the points,  
This parameter will be ignored if the function is deterministic (like doe)}
\item{seq.design.oldBest.size}{[\code{1}] number of the best already evaluated design points that should be taken into consideration for the next sequential designs (e.g. for to have an appropriate number of repeats}
\item{seq.design.new.size}{[\code{2}] according to the predictor the new design pounts during the seq step are ordered by their expected values. This parameter states how much new design points should be evaluated}
\item{seq.design.maxRepeats}{[\code{NA}] each design point is to be evaluated several times for statistically sound results. The number of "repeats" will increase, but will not exceed this seq.design.maxRepeats - value }
\item{seq.design.increase.func}{[\code{"spotSeqDesignIncreasePlusOne"}] functional description of how the repeats are increased (until the seq.design.maxRepeats are reached). Default increases the number of repeats by adding one.}
\item{seq.design.increase.path}{[\code{NA} defaults to \code{srcPath}] path where to find the <seq.design.increase.func>.R -file. Please also see the notes SPOT - extensions}
\item{seq.design.func}{[\code{"spotCreateDesignLhs"}] name of the function to create sequential design. Please also see the notes SPOT - extensions}
\item{seq.design.path}{[\code{NA} defaults to \code{srcPath}] path where to find the <seq.design.func>.R -file}
\item{seq.predictionModel.func}{[\code{"spotPredictLm"}] name of the function calling a predictor. Default uses a Linear Model. Please also see the notes SPOT - extensions}
\item{seq.predictionModel.path}{[\code{NA} defaults to \code{srcPath}] path where to find the <seq.predictionModel.func>.R -file}
\item{seq.log.x}{[\code{FALSE}] use log(x) values for the prediction model}
\item{seq.log.y}{[\code{FALSE}] use log(y) values for the prediction model}
\item{seq.merge.func}{ [\code{mean}] defines the function that merges the results from the different repeat-runs for a design. Default is to calculate the mean value.}
\item{seq.transformation.func}{[\code{I}] function for transformation of "Y" before new model is created, default: Identitity function}
\item{seq.useGradient}{[\code{FALSE}] use gradient information for the prediction model}
\item{seq.useCanonicalPath}{[\code{FALSE}] if gradient information is used, start at saddle point and follow the most steeply rising ridge in both directions. Default: start at origin and follow the path of the steepest descent in one direction}
\item{seq.useAdaptiveRoi}{[\code{FALSE}] use region of intereset adaptation}
\item{meta.keepAllFiles}{[\code{FALSE}] Meta optimization produces lots of temporary files - to keep these files for analysis this switch must be set to TRUE}
\item{report.func}{[\code{"spotReportDefault"}] name of the function providing the report (default="spotReportDefault" Please also see the notes SPOT - extensions }
\item{report.meta.func}{[\code{"spotReportMetaDefault"}] name of the function providing the report for meta runs, do not use these functions for \code{report.func}.}
\item{report.path}{[\code{NA} defaults to \code{srcPath}] path where to find the <report.func>.R -file}
\item{report.meta.path}{[\code{NA} defaults to \code{srcPath}] path where to find the <report.meta.func>.R -file}
\item{report.hist}{[\code{0}] report should hold a histogramme (0=no, 1=yes)}
\item{report.scatter}{[\code{0}] report should hold a scatterplot (0=no, 1=yes)}
\item{report.io.screen}{[\code{FALSE}] report graphics will be printed to screen (FALSE=no, TRUE=yes)}
\item{report.io.pdf}{[\code{TRUE}] report graphics will be printed to pdf (FALSE=no, TRUE=yes)}
\item{report.io.maxPlots}{[\code{11}] a plot of the performance is continuosly updated, showing the .roi variables in the order given in that file. The graphic is limited to 11 variables. A smaller value here may focus to the relevant variables only}}
\references{\code{SPOT} \code{spotPrepare}}
\arguments{\item{srcPath}{the absolute path to the SPOT sources}
\item{configFileName}{users config file (.conf) the absolute path including filespecifier of the user config File}}
