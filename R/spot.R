#' @title spot
#'
#' @description  Sequential Parameter Optimization.
#' This is one of the main interfaces for using the SPOT package. Based on a user-given objective function
#' and configuration, \code{spot} finds the parameter setting that yields the lowest objective value (minimization).
#' To that end, it uses methods from the fields of design of experiment, statistical modeling / machine learning
#' and optimization.
#'
#' @param x is an optional start point (or set of start points), specified as a matrix. One row for each point, and one column for each optimized parameter.
#' @param fun is the objective function. It should receive a matrix x and return a matrix y.
#' In case the function uses external code and is noisy, an additional seed parameter may be used, see the \code{control$seedFun} argument below for details.
#' Mostly, fun must have format y = f(x, ...). If a noisy function requires some specific seed handling, e.g., in some other non-R code,
#' a seed can be passed to fun. For that purpose, the user must specify \code{control$noise = TRUE} and fun should be \code{fun(x, seed, ...)}
#' @param lower is a vector that defines the lower boundary of search space. This determines also the dimensionality of the problem.
#' @param upper is a vector that defines the upper boundary of search space.
#' @param control is a list with control settings for spot. See \code{\link{spotControl}}.
#' @param ... additional parameters passed to \code{fun}.
#'
#' @return This function returns a list with:
#' \describe{
#'		\item{\code{xbest}}{Parameters of the best found solution (matrix).}
#'		\item{\code{ybest}}{Objective function value of the best found solution (matrix).}
#'		\item{\code{x}}{Archive of all evaluation parameters (matrix).}
#'		\item{\code{y}}{Archive of the respective objective function values (matrix).}
#'		\item{\code{count}}{Number of performed objective function evaluations.}
#'		\item{\code{msg}}{Message specifying the reason of termination.}
#'		\item{\code{modelFit}}{The fit of the last build model, i.e., an object returned by the last call to the function specified by \code{control$model}.}
#' }
#'
#' @examples
#' ## Only a few examples. More examples can be found in the vignette and in
#' ## the paper "In a Nutshell -- The Sequential Parameter Optimization Toolbox",
#' ## see https://arxiv.org/abs/1712.04076
#'
#' ## 1. Most simple example: Kriging + LHS search + predicted mean optimization
#' ## (not expected improvement)
#' set.seed(1)
#' res <- spot(x=NULL,funSphere,c(-2,-3),c(1,2),
#'              control=list(funEvals=15))
#' res$xbest
#' res$ybest
#'
#' ## 2. With expected improvement
#' set.seed(1)
#' res <- spot(x=NULL,funSphere,c(-2,-3),c(1,2),
#'             control=list(funEvals=15,
#'                          modelControl=list(target="ei")))
#' res$xbest
#' res$ybest
#'
#' ### 3. Use local optimization instead of LHS search
#' set.seed(1)
#' res <- spot(,funSphere,c(-2,-3),c(1,2),
#'             control=list(funEvals=15,
#'                          modelControl=list(target="ei"),
#'                          optimizer=optimLBFGSB))
#' res$xbest
#' res$ybest
#'
#' ### 4. Use transformed input values
#' set.seed(1)
#' f2 <- function(x){2^x}
#' lower <- c(-100, -100)
#' upper <- c(100, 100)
#' transformFun <- rep("f2", length(lower))
#' res <- spot(x=NULL,funSphere,lower=lower, upper=upper,
#'              control=list(funEvals=15,
#'                           modelControl=list(target="ei"),
#'                           optimizer=optimLBFGSB,
#'                           transformFun=transformFun))
#' res$xbest
#' res$ybest
#'
#' @export
spot <-
  function(x = NULL,
           fun,
           lower,
           upper,
           control = list(),
           ...) {
    ## (S-0) Setup:
    PASSED <- initialInputCheck(x, fun, lower, upper, control)
    control <- spotFillControlList(control, lower, upper)
    ## If replicateResult is true, only one parameter setting
    ## is replicated (reevaluated).
    ## Setting is specified as "lower" vector.
    ## Number of replications are specified via funEvals
    if (control$replicateResult) {
      upper <- lower
      control$designControl$replicates <- 1
      control$designControl$size <- control$funEvals
    }
    
    ## (S-1) Initial design generation
    set.seed(control$seedSPOT)
    if (control$verbosity == 0) {
      x <- suppressWarnings(suppressMessages(
        control$design(
          x = x,
          lower = lower,
          upper = upper,
          control =
            control$designControl
        )
      ))
    } else{
      x <-
        control$design(
          x = x,
          lower = lower,
          upper = upper,
          control = control$designControl
        )
    }
    ## Rounding values produced by the design function to integers, etc.
    x <- repairNonNumeric(x, control$types)
    
    ## (S-2) Eval initial design
    y <- tryCatch(
      expr = {
        objectiveFunctionEvaluation(
          x = NULL,
          xnew = x,
          fun = fun,
          control = control,
          ...
        )
      },
      error = function(e) {
        message("objectiveFunctionEvaluation(). Caught an error! in spot()")
        print(e)
      }
    )
    vmessage(control$verbosity,
             "spot(): y matrix after initial design execution",
             y)
    
    ## (S-3) Imputation: Treating NA and Inf
    ## This should be done after the initial design , before the main loop for (x,y),
    ## and afterwards  only for (xnew, ynew):
    if (!is.null(control$yImputation$handleNAsMethod)) {
      y <- imputeY(x = x,
                   y = y,
                   control = control)
    }
    vmessage(control$verbosity,
             "spot(): y matrix after imputation before passing to spotLoop()",
             y)
    
    ## (S-4) spotLoop
    result <-
      spotLoop(
        x = x,
        y = y,
        fun = fun,
        lower = lower,
        upper = upper,
        control = control,
        ...
      )
    ## result from spotLoop()
    if (control$directOptControl$funEvals > 0) {
      ## (S-22) Starting point for direct optimization
      vmessage(control$verbosity, "Starting Direct Optimization with x-best result Before Direct Optimization:", result$xbest)
      vmessage(control$verbosity, "Starting Direct Optimization with y-best result Before Direct Optimization:", result$ybest)
      xbest <- result$xbest
      if (!is.null(control$directOptControl$eval_g_ineq)  &&
          (
            control$directOptControl$opts$algorithm == "NLOPT_GN_ISRES"  &
            control$directOptControl$eval_g_ineq(xbest) < 0
          )) {
        ## xbest does not satisfy ineq constraint =>
        ## xbest will NOT be used as a starting point x0
        x0 <- NULL
      } else{
        ## xbest does satisfy ineq constraint OR
        ## no constraint function is used =>
        ## take xbest as starting point x0
        x0 <- xbest
      }
      
      ## (S-23) Direct optimization on the real fun
      optimResDirect <- control$directOpt(
        x = x0,
        fun = fun,
        lower = lower,
        upper = upper,
        control$directOptControl,
        ...
      )
      
      ## (S-24) Update results adding direct 
      ## if directOpt finds an improved solution, modify the
      ## best results accordingly:
      if (result$ybest > optimResDirect$ybest) {
        result$xbest <-  optimResDirect$xbest
        result$ybest <-  optimResDirect$ybest
      }
     result$x <- rbind(result$x, optimResDirect$x)
     result$y <- rbind(result$y, optimResDirect$y)
     ## Plots, output, etc
      ybestVec <- c(result$ybestVec , result$ybest)
      if (control$plots) {
        plot(result$y,
             type = "l",
             main = paste("ybest:", min(ybestVec)))
        abline(a = 0, b = 0)
        lines(1:length(ybestVec), ybestVec, col = "red")
      }
    } # end if direct optimization
    
    vmessage(control$verbosity, "Final x Result from SPOT:", result$xbest)
    vmessage(control$verbosity, "Final y Result from SPOT:", result$ybest)
     
    ## Add full run information
    if (control$returnFullControlList) {
      result$control <- control
    }
    
    ## Finally, clean up
    spotCleanup(control)
    result$control$time$endTime <- Sys.time()
    return(result)
  }


#' @title spotFillControlList
#'
#' @description Fill in some values for the control list. Internal use only.
#'
#' @param controlList list of controls, see \code{\link{spotControl}}.
#' @return a list
#' @export
#' @keywords internal

spotFillControlList <- function(controlList, lower, upper) {
  dimension <- length(lower)
  con <- spotControl(dimension)
  con[names(controlList)] <- controlList
  controlList <- con
  rm(con)
  
  ## All functions should deal with the same data types
  controlList$designControl$types <- controlList$types
  controlList$modelControl$types <- controlList$types
  controlList$optimizerControl$types <- controlList$types
  
  # Start time
  controlList$time$startTime <- Sys.time()
  controlList$time$endTime <- NA
  
  # Add lower/upper. So they are available in the control list
  # But this generates duplicate information, because
  # they are stored as lower and as controlList$lower =>
  # FIXME: remove "lower" and use control$lower only?
  controlList$lower <- lower
  controlList$upper <- upper
  
  return(controlList)
}


#' @title spotControl
#' @description Default Control list for spot.
#' This function returns the default controls for the functions \code{\link{spot}} and \code{\link{spotLoop}}.
#' @details
#' Control is a list of the settings:
#' \describe{
#'   \item{\code{design}}{A function that creates an initial design of experiment. Functions that accept the same parameters,
#'				and return a matrix like \code{\link{designLHD}} or \code{\link{designUniformRandom}} can be used. Default is \code{\link{designLHD}}.}
#'   \item{\code{designControl}}{A list of controls passed to the \code{control} list of the \code{design} function. See help
#'				of the respective function for details. Default is an empty \code{list}.}
#'   \item{\code{directOpt}}{A function that is used to optimize after the \code{spot} run is finished.
#'    Functions that accept the same parameters, and return a matrix like \code{\link{optimNLOPTR}}
#'				or \code{\link{optimDE}} can be used. Default is \code{\link{optimNLOPTR}}.}
#'   \item{\code{directOptControl}}{A list of controls, which determine whether a direct optimization
#'   (exploitation of the final search region) is performed. Default is to run no direct optimization, i.e.,
#'   \code{directOptControl = list(funEvals = 0)list}.
#'   \describe{
#'   \item{\code{funEvals}}{This is the budget of function evaluations of the direct optimization performed
#'   after the SMBO is performed. Default is \code{list(funEvals = 0)}.}
#'   }
#'   }
#'   \item{\code{duplicate}}{In case of a deterministic (non-noisy) objective function, this handles duplicated candidate solutions.
#'				By default (\code{duplicate = "EXPLORE"}), duplicates are replaced by new candidate solutions, generated by random
#'				sampling with uniform distribution. If desired, the user can set this to "STOP", which means that the optimization
#'				stops and results are returned to the user (with a warning). This may be desirable, as duplicates can be a indicator
#'				for convergence, or for a problem with the configuration.
#'				In case of noise, duplicates are allowed.}
#'   \item{\code{funEvals}}{This is the budget of function evaluations (spot uses no more than funEvals evaluations of fun), defaults to 20.}
#'  \item{\code{handleNAsMethod}}{A function that treats NAs if there are any present in the result vector of the objective function.
#'              Default: \code{NULL}. By default NAs will not be treated.}
#'   \item{\code{infillCriterion}}{A function defining an infillCriterion to be used while optimizing a model. Default: NULL. For example check infillExpectedImprovement}
#'   \item{\code{model}}{A function that builds a statistical model of the observed data. Functions that accept the same
#'				parameters, and return a matrix like \code{\link{buildKriging}} or \code{\link{buildRandomForest}}
#'				can be used. Default is \code{\link{buildKriging}}.}
#'   \item{\code{modelControl}}{A list of controls passed to the \code{control} list of the \code{model} function.
#'				See help of the respective function for details. Default is an empty \code{list}.}
#'   \item{\code{multiStart}}{Number of restarts for optimization on the surrrogate
#'   model. Default: \code{1}, i.e., no restarts.}
#'   \item{\code{noise}}{Boolean, whether the objective function has noise or not. Default is non-noisy, that is, \code{FALSE}.}
#'   \item{\code{OCBA}}{Boolean, indicating whether Optimal Computing Budget Allocation (OCBA) should be used in case of a noisy
#'				objective function or not. OCBA controls the number of replications for each candidate solution.
#' 				Note, that \code{replicates} should be larger than one in that case, and that the initial experimental design
#'				(see \code{design}) should also have replicates larger one. Default is \code{FALSE}.}
#'   \item{\code{OCBABudget}}{The number of objective function evaluations that OCBA can distribute in each iteration.
#'   Default is 3.}
#'   \item{\code{optimizer}}{A function that is used to optimize based on \code{model}, finding the most promising
#'				candidate solutions. Functions that accept the same parameters, and return a matrix like \code{\link{optimLHD}}
#'				or \code{\link{optimDE}} can be used. Default is \code{\link{optimLHD}}.}
#'   \item{\code{optimizerControl}}{A list of controls passed to the \code{control} list of the \code{optimizer} function.
#'				See help of the respective function for details. Default is an empty \code{list}.}
#'   \item{\code{parNames}}{ Vector of parameter names of each variable as a string, defaults \code{c("x1", "x2", "x3",..)}.}
#'   \item{\code{plots}}{Whether progress should be tracked by a line plot, default is \code{FALSE}}
#'   \item{\code{progress}}{Whether progress should be visualized, default is \code{FALSE}}
#'   \item{\code{replicates}}{The number of times a candidate solution is initially evaluated, that is, in the initial design,
#'				or when created by the optimizer. Default is \code{1}.}
#'   \item{\code{replicateResult}}{\code{logical}. If \code{TRUE}, one result is
#'   replicated. The result is specified as  the \code{lower} vector and
#'   re-evaluated \code{funEvals} times. No model building and
#'   optimization is performed, only evaluations on the
#'   objective function. Default: \code{FALSE}.}
#'	 \item{\code{returnFullControlList}}{\code{logical}. Return the full control
#'	 list. Can be switched off to save memory/space. Default: \code{TRUE}.}
#'   \item{\code{seedFun}}{An initial seed for the objective function in case of noise, by default \code{NA}. The default means that no seed is set.
#'				The user should be very careful with this setting. It is intended to generate reproducible experiments for each objective
#'				function evaluation, e.g., when tuning non-deterministic algorithms. If the objective function uses a constant number
#'				of random number generations, this may be undesirable. Note, that this seed is by default set prior to each evaluation. A replicated
#'				evaluation will receive an incremented value of the seed.
#'				Sometimes, the user may want to call external code using random numbers. To allow for that case, the user can specify an objective function (\code{fun}),
#' 				which has a second parameter \code{seed}, in addition to first parameter (matrix \code{x}). This seed can then be passed
#'				to the external code, for random number generator initialization. See end of examples section for a demonstration.}
#'   \item{\code{seedSPOT}}{This value is used to initialize the random number generator. It ensures that experiments are reproducible. Default is \code{1}.}

#'   \item{\code{subsetSelect}}{A function that selects a subset from a given set of design points. Default is \code{\link{selectAll}}.}
#'   \item{\code{subsetControl}}{A list of controls passed to the \code{control} list of the \code{subsetSelect} function. See help
#'				of the respective function for details. Default is an empty \code{list}.}
#'   \item{\code{time}}{List with the following time information:
#'   \describe{
#'   \item{\code{maxTime}}{\code{num} Maximum allowed run time (in minutes) for \code{spot} or \code{spotLoop}.
#'          The default value for \code{maxTime} (in minutes) is \code{Inf} and can be overwritten by the user.
#'          The internal value \code{startTime}, that is used to control \code{maxTime},
#'          will be set by \code{\link{spotFillControlList}}.
#'          Note: \code{maxTime}  is only an approximate value. It does not affect the \code{directOpt} run.}
#'   \item{\code{startTime}}{Start time. Will be set in \code{\link{spotFillControlList}}.}
#'   \item{\code{endTime}}{End time.}
#'   }
#'  }
#'   \item{\code{types}}{ Vector of data type of each variable as a string, defaults \code{"numeric"} for all variables.}
#'   \item{\code{verbosity}}{Integer level specifying how much output should be given by SPOT. 0 (default) ignores warnings of internal optimizers /models.
#'              1 will show warnings and output.}
#' }
#' @param dimension problem dimension, that is, the number of optimized parameters. This parameter is mandatory since v2.8.4.
#'
#' @return a list
#' @export
spotControl <- function(dimension = NA) {
  if (is.na(dimension))
    stop("Error: spotControl() call w/o dimension.")
  list(
    funEvals = 20,
    lower = NA,
    ## lower and are not defined here, because
    upper = NA,
    ## their are separate arguments.
    design = designLHD,
    designControl = list(),
    directOpt = optimNLOPTR,
    ## default: do not use directOpt:
    directOptControl = list(funEvals = 0),
    infillCriterion = NULL,
    ## By default no infillCriterion is used
    ## maxTime is deprecated and replaced by the list "time"
    ## maxTime is only included for backward compatibility
    maxTime = Inf,
    model = buildKriging,
    modelControl = list(modelInitialized = FALSE),
    multiStart = 0,
    # whether or not the target function is non-deterministic:
    noise = FALSE,
    OCBA = FALSE,
    # the budget available to OCBA, to be distributed to replications of "old" solutions:
    OCBABudget = 1,
    optimizer = optimLHD,
    optimizerControl = list(),
    parNames = paste0("x", 1:dimension),
    plots = FALSE,
    progress = FALSE,
    # the number of replications for all "new" solutions
    # (unless generated by the initial design, which handles them separately):
    replicates = 1,
    ## Replicate one solution "funEvals" times. Solution is specified
    ## as lower vector:
    replicateResult = FALSE,
    ## return the full control list:
    returnFullControlList = TRUE,
    seedFun = NA,
    #start RNG seed for the target function (only important if non-deterministic, i.e., if noise==TRUE). NA means that seed is not set before running fun, this is important for cases where an initial seed for the target function is undesirable. (e.g., functions with constant and identical number of calls to random number generator)
    seedSPOT = 1,
    #start RNG seed for the SPOT loop
    ## Note: in case of optimization algorithm tuning, where the optimization algorithm again solves
    ## a noisy objective function, the user should handle proper random number generator seeds for the
    ## noisy objective function.
    ## SPOT only sets/handles seeds for the main process as well as the tuned algorithm.
    ## Check objectiveFunctionEvaluation to see how to properly disentangle different "random streams"
    ## in R.
    ## Note: all evaluations are passed to the model, including repeated entries for replicated samples
    ## (e.g., based on OCBA). The model itself should handle repeated samples.
    ## The exception from this rule should be if noise==FALSE, in that case, duplicates are removed.
    ## (?) TODO -> what if only one duplicate suggested -> will be suggested again in next iteration?
    subsetSelect = selectAll,
    subsetControl = list(),
    ## Start time will be set in spotFillControlList()
    time = list(maxTime = Inf),
    ## tolerance. Can be used as a termination criterion (or break).
    tolerance = sqrt(.Machine$double.eps),
    # transformation of x values
    transformFun = vector(),
    types = rep("numeric", dimension),
    verbosity = 0,
    # number of new design points proposed by the surrogate model
    # (last rows of the xnew matrix):
    xNewActualSize = 0,
    xBestOcba = NA,
    yBestOcba = NA,
    # list of functions to determine imputations, see also handleNAsMethod
    yImputation = list(
      imputeCriteriaFuns = list(is.na, is.infinite, is.nan),
      handleNAsMethod = NULL,
      # Method for treating NAs, default NULL, no method used
      penaltyImputation = 3 # penalty used for imputed values (used by handleNAsKrigingWorst)
    )
  )
}


#' @title Sequential Parameter Optimization Main Loop
#'
#' @description SPOT is usually started via the function \code{\link{spot}}. However, SPOT runs can be continued
#' (i.e., with a larger budget specified in \code{control$funEvals}) by using \code{spotLoop}.
#' This is the main loop of SPOT iterations. It requires the user to give the same inputs as
#' specified for \code{\link{spot}}. Note: \code{control$funEvals} must be larger than the value
#' used in the previous run, because it specifies the total number of function evaluations and
#' not the additional number of evalutions.
#'
#' @param x \code{(m,n) matrix} that contains the known candidate solutions.
#' The SPOT loop is started with these values. Each row represents one \code{n} dimensional
#' data point. Each of the \code{m} columns represents one optimized parameter.
#' @param y \code{(m,p) matrix} that represents observations for each point in \code{x},
#' Each of the \code{m} rows represents solutions for one data point.
#' @param fun \code{function} that represents the objective function.
#' It should receive a matrix \code{x} and return a matrix \code{y}.
#' In case the function uses external code and is noisy, an additional seed parameter may be used,
#' see the \code{control$seedFun} argument below for details.
#' @param lower is a vector that defines the lower boundary of search space.
#' This determines also the dimension of the problem.
#' @param upper is a vector that defines the upper boundary of search space.
#' @param control is a list with control settings for spot. See \code{\link{spotControl}}.
#' @param ... additional parameters passed to \code{fun}.
#'
#' @importFrom graphics abline
#' @importFrom graphics plot
#'
#' @return This function returns a list with:
#' \describe{
#'		\item{{xbest}}{Parameters of the best found solution (matrix).}
#'		\item{\code{ybest}}{Objective function value of the best found solution (matrix).}
#'		\item{\code{x}}{Archive of all evaluation parameters (matrix).}
#'		\item{\code{y}}{Archive of the respective objective function values (matrix).}
#'		\item{\code{count}}{Number of performed objective function evaluations.}
#'		\item{\code{msg}}{Message specifying the reason of termination.}
#'		\item{\code{modelFit}}{The fit of the last build model, i.e.,
#'		an object returned by the last call to the function specified by \code{control$model}.}
#' }
#'
#' @examples
#' ## Most simple example: Kriging + LHS + predicted
#' ## mean optimization (not expected improvement)
#' \donttest{
#' control <- list(funEvals=20)
#' res <- spot(,funSphere,c(-2,-3),c(1,2),control)
#' ## now continue with larger budget.
#' ## 5 additional runs will be performed.
#' control$funEvals <- 25
#' res2 <- spotLoop(res$x,res$y,funSphere,c(-2,-3),c(1,2),control)
#' res2$xbest
#' res2$ybest
#' }
#' @export
spotLoop <- function(x, y, fun, lower, upper, control, ...) {
  if (nrow(x) != nrow(y)) {
    print(x)
    print(y)
    stop("spotLoop(): x and y dimension mismatch!")
  }
  
  ## (S-5) Initial input check and control list update
  initialInputCheck(x, fun, lower, upper, control, inSpotLoop = TRUE)
  dimension <- length(lower)
  con <- spotControl(dimension)
  con[names(control)] <- control
  control <- con
  rm(con)
  control <- spotFillControlList(control, lower, upper)
  vmessage(control$verbosity, 
      "spotLoop: y matrix (i) either received from spot() after initial design 
      execution or (ii) from previous runs.", y)
  
  ## (S-6) Imputation: Treating NA and Inf
  if (!is.null(control$yImputation$handleNAsMethod)) {
    y <- imputeY(x = x,
                 y = y,
                 control = control)
  }
  vmessage(control$verbosity, 
           "spotLoop: y matrix after imputation.", y)
  
  ## (S-7) Counter and logs
  count <- nrow(y)
  modelFit <- NA
  ybestVec <- rep(min(y[, 1]), count)
  ## ySurr has NAs, because no surrogate model was build so far:
  ySurr <- matrix(NA, nrow = 1, ncol = count)
  vmessage(control$verbosity, "Initial x-values before entering main loop in spotLoop()", x)
  vmessage(control$verbosity, "Initial y-values before entering main loop in spotLoop()", y)
  vmessage(control$verbosity, "Initial ybestVec before entering main loop in spotLoop()", ybestVec)
  
  ## (S-8) Termination criteria (while loop)
  while ((count < control$funEvals) &
         (difftime(Sys.time(), control$time$startTime, units = 'mins')
          < control$time$maxTime)) {
    
    ## (S-9) Subsect select
    ## Select points for model building (model requires one y-value only)
    ## Model building is done with this subset, but the complete
    ## result set is kept as "y" and combined with the new evaluations further below
    selectRes <-  tryCatch(
      expr = {
        control$subsetSelect(x = x,
                             y = y[, 1, drop = FALSE],
                             control = control$subsetControl)
      },
      error = function(e) {
        message('control$subsetSelect() in spot.R: Caught an error!')
        print(e)
      }
    )
    vmessage(control$verbosity, 
             "Selected x-subset from control$subsetSelect() in spot.R",
             selectRes$x)
    vmessage(control$verbosity, 
             "Selected y-subset from control$subsetSelect() in spot.R",
             selectRes$y)

    ## (S-10) Surrogate model fit
    ## Model building using control$model (default is buildKriging).
    ## Based on fit
    modelFit <-  tryCatch(
      expr = {
        control$model(x = selectRes$x,
                      y = selectRes$y,
                      control = control$modelControl) #todo return modelControl to allow memory?
      },
      error = function(e) {
        message('control$model() in spotLoop(): Caught an error!')
        message("Selected x values:")
        print(selectRes$x)
        message("Selected y values:")
        print(selectRes$y)
        print(e)
      }
    )
    ## Some models can be updated. 
    ## So, we store information whether the model is already build.
    control$modelControl$modelInitialized <- TRUE
    
    ## (S-11) Optimization function on the surrogate
    ## Generate a surrogate target function from the model. Based on predict.
    # funSurrogate <- evaluateModel(modelFit, control$infillCriterion, control$verbosity)
    funSurrogate <-  tryCatch(
      expr = {
        evaluateModel(modelFit, control$infillCriterion, control$verbosity)
      },
      error = function(e) {
        message('evaluateModel in spotLoop(): Caught an error!')
        print(e)
      }
    )
    
    ## (S-12) Random starting points for optimization on the surrogate
    vmessage(control$verbosity,"MultiStarts", control$multiStart)
    x0 <- getMultiStartPoints(x, y, control)
    vmessage(control$verbosity,"x0 from getMultiStartPoints",x0)
    
    
    ## (S-13a) Search on the surrogate without starting point
    if (is.null(x0)) {
      optimResSurr <- tryCatch(
        expr = {
          control$optimizer(x = NULL,
                            funSurrogate,
                            lower,
                            upper,
                            control$optimizerControl)
        },
        error = function(e) {
          message('Calling control$optimizer() in spot.R: Caught an error!')
          print(e)
        }
      )
      ## Result w/o multi starts: from acquisition on surrogate (take the best value only):
      xnew <- optimResSurr$xbest
      ## value on the surrogate (can be "y", "s2, "ei", "negLog10ei" etc.)
      ySurrNew <- optimResSurr$ybest
    } else{
      ## (S-13b) Search on the surrogate with starting point/s x0:
      resSurr <- matrix(NA, nrow = nrow(x0), ncol = ncol(x0) + 1)
      for (i in 1:nrow(x0)) {
        optimResSurr <- tryCatch(
          expr = {
            control$optimizer(x = x0[i, , drop = FALSE],
                              funSurrogate,
                              lower,
                              upper,
                              control$optimizerControl)
          },
          error = function(e) {
            message('optimResSurr: Calling control$optimizer() in spot.R: Caught an error!')
            vmessage(1,"Failed search on the surrogate with starting point/s x0:", x0)
            print(e)
          }
        )
        resSurr[i, ] <- c(optimResSurr$xbest, optimResSurr$ybest)
      }
      
      ## (S-14) Compile results from search on surrogate
      m <- which.min(resSurr[, ncol(x) + 1])
      ## Determine xnew based on multi start results
      xnew <- resSurr[m, 1:ncol(x), drop=FALSE]
      ## value on the surrogate (can be "y", "s2, "ei", "negLog10ei" etc.)
      ySurrNew <- resSurr[m, ncol(x) + 1]
    } ## end else
    vmessage(control$verbosity, "xnew from control$optimizer() in spot():", xnew)

    ## (S-15) Handling of duplicates and repair
    xnew <- tryCatch(
      expr = {
        duplicateAndReplicateHandling(xnew, x, lower, upper, control)
      },
      error = function(e) {
        message('Calling duplicateAndReplicateHandling() in spot.R: Caught an error!')
        print(e)
      }
    )
    ## Rounding non-numeric values produced by the optimizer
    xnew <- tryCatch(
      expr = {
        repairNonNumeric(xnew, control$types)
      },
      error = function(e) {
        message('Calling repairNonNumeric() in spot.R: Caught an error!')
        print(e)
      }
    )
    
    ## (S-16) OCBA
    ## If desired, use OCBA to handle replications of old solutions
    vmessage(control$verbosity, "xnew before OCBA:", xnew)
    if (control$noise & control$OCBA) {
      try(xnew <- rbind(xnew, repeatsOCBA(x, y[, 1, drop = FALSE],
                                          control$OCBABudget)))
    }
    vmessage(control$verbosity, "xnew after OCBA:", xnew)
    ## Prevent exceeding the budget:
    xnew <-
      xnew[1:min(max(control$funEvals - count, 1), nrow(xnew)), , drop = FALSE]
    
    ## Determine number of new design points:
    control$xNewActualSize <- nrow(xnew)
    
    ## (S-17) Evaluate xnew with objective function
    ynew <- tryCatch(
      expr = {
        objectiveFunctionEvaluation(
          x = x,
          xnew = xnew,
          fun = fun,
          control = control,
          ...
        )
      },
      error = function(e) {
        print(x)
        print(xnew)
        message("ynew: objectiveFunctionEvaluation() in spotLoop(): Caught an error!")
        print(e)
        if (!is.null(control$yImputation$handleNAsMethod)) {
          message("Error will be corrected using the configured NA handling technique.")
          n <- nrow(xnew)
          m <- ncol(y)
          return(matrix(rep(NA, m * n), nrow = n))
        }
      }
    )
    ##
    vmessage(control$verbosity > 0, "Result (ynew) after objectiveFunctionEvaluation() in spot.R:",
      ynew)
    
    ## (S-18) Impute
    ## Combine before impute. This provides a larger basis for imputation.
    colnames(xnew) <- colnames(x)
    x <- rbind(x, xnew)
    y <- rbind(y, ynew)
    if (!is.null(control$yImputation$handleNAsMethod)) {
      y <- imputeY(x = x,
                   y = y,
                   control = control)
    }
    vmessage(control$verbosity, "Combined x and xnew in spot after imputation:", x)
    vmessage(control$verbosity, "Combined y and ynew in spot after imputation:", y)
    
    ## (S-19) Update counter, logs, etc.  
    ySurr <- c(ySurr, ySurrNew)
    count <- count + nrow(ynew)
    
    ## (S-20) Reporting in while loop: Best values and result list
    indexBest <- which.min(y[, 1, drop = FALSE])
    ybestVec <- c(ybestVec , y[indexBest, 1, drop = FALSE])
    if (control$plots) {
      plot(y[, 1], type = "l", main = paste("ybest:", min(ybestVec)))
      abline(a = 0, b = 0)
      lines(1:length(ybestVec), ybestVec, col = "red")
    }
    
    ## Progress: funEvals
    if (control$progress &
        (control$funEvals > 0) & is.finite(control$funEvals)) {
      cat(paste0(
        round(count / (control$funEvals) * 100),
        "% completed. Best y val:",
        min(ybestVec),
        "\n"
      ))
      if (count == (control$funEvals))
        cat("Done. Best y val:", min(ybestVec), " \n")
    }
    ## Progress: time
    if (control$progress &
        is.finite(control$time$maxTime) &
        (control$time$maxTime > 0)) {
      usedTime <- round(
        difftime(Sys.time(), control$time$startTime, units = 'mins') / control$time$maxTime * 100
      )
      cat(paste0(usedTime, "% completed. Best y val:", min(ybestVec), "\n"))
      if ((
        difftime(Sys.time(), control$time$startTime, units = 'mins') >= control$time$maxTime
      ))
        cat("Done. Best y val:", min(ybestVec), " \n")
    }
  } # while loop
  
  ## (S-21) Reporting after while loop in spotLoop
  indexBest <- which.min(y[, 1, drop = FALSE])
  if (ncol(y) > 1) {
    logInfo <- y[, -1, drop = FALSE]
  }  else{
    logInfo <- NA
  }
  
  if (length(control$transformFun) > 0) {
    xt <- transformX(xNat = x, fn = control$transformFun)
  } else {
    xt <- NA
  }
  
  if (control$noise & control$OCBA) {
    ocbaRes <- ocbaRanking(
      x = x,
      y = y,
      fun = fun,
      control = control,
      ...
    )
    control$xBestOcba <- ocbaRes[1, 1:(ncol(ocbaRes) - 1), drop=FALSE]
    control$yBestOcba <- ocbaRes[1, ncol(ocbaRes), drop=FALSE]
  }
  list(
    xbest = x[indexBest, , drop = FALSE],
    ybest = y[indexBest, 1, drop = FALSE],
    xBestOcba = control$xBestOcba,
    yBestOcba = control$yBestOcba,
    x = x,
    xt = xt,
    y = y[, 1, drop = FALSE],
    logInfo = logInfo,
    count = count,
    msg = "budget exhausted",
    modelFit = modelFit,
    ybestVec = ybestVec,
    ySurr = ySurr
  )
}

#' @title Clean up
#'
#' @description Remove objects
#'
#' @param control \code{list} of \code{spot} control parameters.
#'
#' @importFrom laGP deleteGPseps
#' @importFrom laGP deleteGPs
#' @importFrom laGP deleteGPsep
#'
#' @export
spotCleanup <- function(control) {
  if (inherits(control, "spotGaussianProcessModel")) {
    deleteGPsep(control$fit)
    deleteGPs()
    deleteGPseps()
  }
}
