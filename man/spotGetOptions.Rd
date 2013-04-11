\name{spotGetOptions}
\alias{spotGetOptions}
\title{Set all options by conf-file or by default}
\usage{
  spotGetOptions(srcPath = ".", configFileName)
}
\arguments{
  \item{srcPath}{the absolute path to the SPOT sources}

  \item{configFileName}{users config file (.conf) the
  absolute path including file-specifier of the user config
  File}
}
\value{
  spotGetOptions returns the list of all SPOT options
  created by this function:
  \item{auto.loop.steps}{[\code{Inf}] number of iterations
  the loop over all SPOT-steps should be repeated}
  \item{auto.loop.nevals}{[\code{200}] budget of
  algorithm/simulator runs - most important parameter for
  run-time of the algorithm in case the spot-function is
  called with the "auto"-task }
  \item{spot.continue}{[\code{FALSE}] boolean, SPOT will
  try to continue based on existing results in spotConfig
  or .res file if this value is TRUE}
  \item{spot.fileMode}{[\code{TRUE}] boolean, that defines
  if files are used to read and write results (which is the
  "classic" spot procedure) or if SPOT will only use the
  workspace to store variables.}
  \item{spot.seed}{[\code{123}] global seed setting for all
  random generator dependent calls within SPOT. same seed
  shall repeat same results, \cr BUT: please note: this is
  NOT the seed for the algorithm! see alg.seed}
  \item{alg.func}{[\code{"spotFuncStartBranin"}] target
  function to be optimized by SPOT.\cr SPOT searches for
  the given function name in workspace} \item{}{If alg.func
  is a string, SPOT expects an interface like
  \code{\link{spotFuncStartBranin}}.\cr If alg.func is a
  function, SPOT expects a function of type y=f(x)\cr (see:
  \code{\link{spotOptimInterface}}).}
  \item{alg.resultColumn}{[\code{"Y"}] string to indicate
  the name of the result column. This might be
  automatically reset if \code{\link{spotOptimInterface}}
  is used, i.e. if a function is passed to alg.func}
  \item{}{It can be a vector of strings, if multi objective
  optimization is done.} \item{alg.seed}{[\code{1234}] seed
  for random generator to be used by the user defined
  algorithm.  This is needed to reproduce the results. Set
  to NA if seed should not be reset. }
  \item{alg.roi}{internal parameter for the initial region
  of interest (do not try to set this one, it will be
  overwritten with default values).  It is used to provide
  an easy to use matrix with the data from the ".roi"-file
  (= Region Of Interest)} \item{}{The user can create an
  ROI matrix with the \code{\link{spotROI}} constructor.}
  \item{alg.aroi}{internal parameter for the actual region
  of interest (do not try to set this one, it will be
  overwritten with default values).  It is used to provide
  an easy to use matrix with the data from the ".aroi"-file
  (= Actual Region Of Interest)}
  \item{alg.currentDesign}{[usually not changed by user]
  data frame of the design that will be evaluated by the
  next call to \link{spotStepRunAlg}}
  \item{alg.currentResult}{[usually not changed by user]
  data frame that contains the results of the target
  algorithm runs} \item{alg.currentBest}{[usually not
  changed by user] data frame that contains the best
  results of each step conducted by spot}
  \item{io.columnSep}{[\code{""}] column separator for the
  input/output files, default means: arbitrary whitespace
  sequence, should be set by the value you want to have
  between your columns} \item{io.apdFileName}{[depends:
  \code{<configFileName>.apd}] name of the .apd -file
  (Algorithm Problem Definition file, holding all
  specification the user written algorithm needs to perform
  a complete optimization)} \item{io.roiFileName}{[depends:
  \code{<configFileName>.roi}] name of the .roi -file
  (Region Of Interest - File, holding all varying
  parameters and constraints)}
  \item{io.desFileName}{[depends:
  \code{<configFileName>.des}] name of the .des -file
  (DESign file, the file the user written algorithm uses as
  input to the parameters it should change)}
  \item{io.resFileName}{[depends:
  \code{<configFileName>.res}] name of the .res -file
  (RESult file) the user written algorithm has to write its
  results into this file } \item{io.bstFileName}{[depends:
  \code{<configFileName>.bst}] name of the .bst -file (BeST
  file) the result-file will be condensed to this file }
  \item{io.pdfFileName}{[depends:
  \code{<configFileName>.pdf}] name of the .pdf -file the
  default report will write its summary of results in this
  pdf file } \item{io.fbsFileName}{[depends:
  \code{<configFileName>.bst}] name of the .fbs -file
  (Final BestSolution file) collects all final best values
  of all .bst files during a .meta-run }
  \item{io.verbosity}{[\code{3}] level of verbosity of the
  program, 0 should be silent and 3 should produce all
  output- sometimes just interesting for the developer...}
  \item{init.design.func}{[\code{"spotCreateDesignLhd"}]
  name of the function to create an initial design. Please
  also see the notes SPOT - extensions}
  \item{init.design.size}{[\code{10}] number of initial
  design points to be created. Required by some space
  filling design generators. Will be used in the
  <init.design.func>.R-file. If \code{=NA} a value is
  calculated by formula.}
  \item{init.design.retries}{[\code{100}] number of retries
  the initial designs should be retried to find randomly a
  design with maximum distance between the points This
  parameter will be ignored if the function is
  deterministic (like doe)}
  \item{init.design.repeats}{[\code{2}] number of repeats
  for each design point to be called with the <alg.func>}
  \item{init.delete.previous.files}{[\code{TRUE}] delete an
  existing res or bst file.  Should be set to FALSE if a
  SPOT run is resumed, after crash or user triggered stop.}
  \item{seq.design.size}{[\code{10000}] number of
  sequential design points to be created}
  \item{seq.design.retries}{[\code{10}] number of retries
  the initial designs should be retried to find randomly a
  design with maximum distance between the points, This
  parameter will be ignored if the function is
  deterministic (like doe)}
  \item{seq.design.oldBest.size}{[\code{1}] number of the
  best already evaluated design points that should be taken
  into consideration for the next sequential designs (e.g.
  for to have an appropriate number of repeats}
  \item{seq.design.new.size}{[\code{2}] according to the
  predictor the new design points during the seq step are
  ordered by their expected values. This parameter states
  how many new design points should be evaluated}
  \item{seq.design.maxRepeats}{[\code{NA}] each design
  point is to be evaluated several times for statistically
  sound results. The number of "repeats" will increase, but
  will not exceed this seq.design.maxRepeats - value }
  \item{seq.design.increase.func}{[\code{"spotSeqDesignIncreasePlusOne"}]
  functional description of how the repeats are increased
  (until the seq.design.maxRepeats are reached). Default
  increases the number of repeats by adding one.}
  \item{seq.design.func}{[\code{"spotCreateDesignLhd"}]
  name of the function to create sequential design. Please
  also see the notes SPOT - extensions}
  \item{seq.mco.selection}{[\code{"hypervol"}] selection
  scheme for new design candidates in case of multi
  objective optimization. "hypervol" considers contribution
  of each point, "tournament2" is a tournament selection.
  "tournament1" is not yet recommended for use.}
  \item{seq.predictionModel.func}{[\code{"spotPredictRandomForest"}]
  name of the function calling a predictor. Default uses a
  Random Forest.} \item{seq.predictionOpt.func}{[\code{NA}
  If not NA this string will be interpreted as a function
  name. The function is expected to add a new setting to
  the sequential design. See
  \code{\link{spotPredictOptMulti}}} \item{seq.merge.func}{
  [\code{mean}] defines the function that merges the
  results from the different repeat-runs for a design.
  Default is to calculate the mean value.}
  \item{seq.transformation.func}{[\code{I}] function for
  transformation of "Y" before new model is created,
  default: Identity function}
  \item{seq.useAdaptiveRoi}{[\code{FALSE}] use region of
  interest adaptation}
  \item{report.func}{[\code{"spotReportDefault"}] name of
  the function providing the report
  ("spotReportSens","spotReport3d","spotReportContour") }
  \item{report.io.screen}{[\code{FALSE}] report graphics
  will be printed to screen (FALSE=no, TRUE=yes)}
  \item{report.io.pdf}{[\code{TRUE}] report graphics will
  be printed to pdf (FALSE=no, TRUE=yes)}
}
\description{
  spotGetOptions \cr 1.) sets default values \cr 2.)
  overwrites all default values by the settings the user
  provides with the config file (.conf-file) \cr All
  options described here, that are not marked as "internal
  variable" may be changed by the user. This will be done
  by reading the ".conf"-file that the user has specified
  as the first (and maybe sole) parameter to the function
  spot(). To change this default value of a variable,
  simply write a line into the ".conf"-file following this
  syntax:\cr <variable>=<value> e.g.:
  \code{spot.seed=54321}\cr This function will do even
  more: the user may define his own variables in the
  .conf-file and may use them in user written plugins. All
  plugins will get the whole list of options with the
  parameter "spotConfig". As a result a variable given in
  the .conf file as \cr \code{my.var=37} \cr may be
  referred to by spotConfig$my.var and can be used in all
  functions - especially in the functions that are designed
  to be open to adaptions where ever necessary.
}
\note{
  Additional settings can and will be written to the
  spotConfig list by other optional functions. See the
  documentation of these to functions for details.
}
\seealso{
  \code{SPOT} \code{spotPrepare}
}

