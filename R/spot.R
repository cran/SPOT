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
#' res <- spot(,funSphere,c(-2,-3),c(1,2),
#'              control=list(funEvals=15))
#' res$xbest
#' res$ybest
#' 
#' ## 2. With expected improvement
#' set.seed(1)
#' res <- spot(,funSphere,c(-2,-3),c(1,2),
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
#' 
#' @export

spot <-
  function(x = NULL,
           fun,
           lower,
           upper,
           control = list(),
           ...) {
    #Initial Input Checking
    PASSED <- initialInputCheck(x, fun, lower, upper, control)
    ## default settings
    dimension <- length(lower)
    control <- spotFillControlList(control, dimension)
    
    ## Initial design generation
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
    
    ## Evaluate initial design with objective function
    y <-
      objectiveFunctionEvaluation(
        x = NULL,
        xnew = x,
        fun = fun,
        seedFun = control$seedFun,
        noise = control$noise,
        ...
      )
    
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
    # result
     if(control$directOptControl$funEvals > 0){
       if (control$verbosity > 0){
      print("Starting Direct Optimization:")
      print("*******************************")
      print("Result Before Direct Optimization:")
      print(result$xbest)
      print(result$ybest)
      print("*******************************")
      }
### Begin 1. Feb 2021:
       xbest <- result$xbest
       
       if (!is.null(control$directOptControl$eval_g_ineq)  && 
           (control$directOptControl$opts$algorithm == "NLOPT_GN_ISRES"  & control$directOptControl$eval_g_ineq(xbest) < 0)
       ) {
         ## xbest does not satisfy ineq constraint =>
         ## xbest will NOT be used as a starting point x0
         x0 <- NULL
       } else{
         ## xbest does satisfy ineq constraint OR 
         ## no constraint function is used =>
         ## take xbest as starting point x0
         x0 <- xbest
       }
       
       optimResDirect <- control$directOpt(x = x0,
                                         fun = fun,
                                         lower = lower,
                                         upper = upper,
                                         control$directOptControl,
                                         ...) 
      
### End 1. Feb 2021
      #       
      # optimResDirect <- control$directOpt(x = result$xbest,
      #                                     fun = fun,
      #                                     lower = lower,
      #                                     upper = upper,
      #                                     control$directOptControl,
      #                                     ...
      # )
      # 
       
       
      ## if directOpt finds an improved solution, modify the 
      ## best results accordingly:
      if(result$ybest > optimResDirect$ybest){
      result$xbest <-  optimResDirect$xbest
      result$ybest <-  optimResDirect$ybest
      }
       
       
       result$x <- rbind(result$x,optimResDirect$x)
      # if (is.numeric(optimResDirect$x)){
      #   result$x <- rbind(result$x,optimResDirect$x)}
      # else{
      #   result$x <- rbind(result$x,optimResDirect$xbest)
      # }
       result$y <- rbind(result$y,optimResDirect$y)
      # if (is.numeric(optimResDirect$y)){
      #   result$y <- rbind(result$y,optimResDirect$y)}
      # else{
      #   result$y <- rbind(result$y,optimResDirect$ybest)
      # }
      
      ## Plots, output, etc
      ybestVec <- c(result$ybestVec , result$ybest)
      if (control$plots) {
        plot(result$y, type = "l", main = paste("ybest:", min(ybestVec)))
        abline(a = 0, b = 0)
        lines(1:length(ybestVec), ybestVec, col = "red")
      }
      
     }
    if (control$verbosity > 0){
    print("Ending Optimization")
    print("*******************************")
    print("Final Result from SPOT:")
    print(result$xbest)
    print(result$ybest)
    print("*******************************")
    }
    ## Finally, clean up
    spotCleanup(control)
    return(result)
  }


#' @title spotFillControlList
#' 
#' @description Fill in some values for the control list. Internal use only.
#'
#' @param controlList list of controls, see \code{\link{spotControl}}.
#' @param dimension dimension of the optimization problem. See \code{\link{spotControl}}.
#' @return a list
#' @export
#' @keywords internal

spotFillControlList <- function(controlList, dimension) {
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
  
  return(controlList)
}


#' @title spotControl
#' @description Default Control list for spot.
#' This function returns the default controls for the functions \code{\link{spot}} and \code{\link{spotLoop}}.
#' @details 
#' Control is a list of the settings:
#' \describe{
#'   \item{\code{funEvals}}{ This is the budget of function evaluations (spot uses no more than funEvals evaluations of fun), defaults to 20.}
#'   \item{\code{types}}{ Vector of data type of each variable as a string, defaults \code{"numeric"} for all variables.}
#'   \item{\code{subsetSelect}}{A function that selects a subset from a given set of design points. Default is \code{\link{selectAll}}.}
#'   \item{\code{subsetControl}}{A list of controls passed to the \code{control} list of the \code{subsetSelect} function. See help
#'				of the respective function for details. Default is an empty \code{list}.}
#'   \item{\code{design}}{A function that creates an initial design of experiment. Functions that accept the same parameters,
#'				and return a matrix like \code{\link{designLHD}} or \code{\link{designUniformRandom}} can be used. Default is \code{\link{designLHD}}.}
#'   \item{\code{designControl}}{A list of controls passed to the \code{control} list of the \code{design} function. See help
#'				of the respective function for details. Default is an empty \code{list}.}
#'   \item{\code{model}}{A function that builds a statistical model of the observed data. Functions that accept the same
#'				parameters, and return a matrix like \code{\link{buildKriging}} or \code{\link{buildRandomForest}}
#'				can be used. Default is \code{\link{buildKriging}}.}
#'   \item{\code{modelControl}}{A list of controls passed to the \code{control} list of the \code{model} function.
#'				See help of the respective function for details.Default is an empty \code{list}.}
#'   \item{\code{optimizer}}{A function that is used to optimize based on \code{model}, finding the most promising
#'				candidate solutions. Functions that accept the same parameters, and return a matrix like \code{\link{optimLHD}}
#'				or \code{\link{optimDE}} can be used. Default is \code{\link{optimLHD}}.}
#'   \item{\code{optimizerControl}}{A list of controls passed to the \code{control} list of the \code{optimizer} function.
#'				See help of the respective function for details. Default is an empty \code{list}.}
#'   \item{\code{directOpt}}{A function that is used to optimize after the \code{spot} run is finished.
#'    Functions that accept the same parameters, and return a matrix like \code{\link{optimNLOPTR}}
#'				or \code{\link{optimDE}} can be used. Default is \code{\link{optimNLOPTR}}.}
#'   \item{\code{directOptControl}}{A list of controls passed to the \code{control} list of the \code{directOpt} function.
#'				See help of the respective function for details. Default is \code{list(funEvals = 0)}.}
#'   \item{\code{noise}}{Boolean, whether the objective function has noise or not. Default is non-noisy, that is, \code{FALSE}.}
#'   \item{\code{OCBA}}{Boolean, indicating whether Optimal Computing Budget Allocation (OCBA) should be used in case of a noisy
#'				objective function or not. OCBA controls the number of replications for each candidate solution.
#' 				Note, that \code{replicates} should be larger than one in that case, and that the initial experimental design
#'				(see \code{design}) should also have replicates larger one. Default is \code{FALSE}.}
#'   \item{\code{OCBAbudget}}{The number of objective function evaluations that OCBA can distribute in each iteration. Default is 3.}
#'   \item{\code{replicates}}{The number of times a candidate solution is initially evaluated, that is, in the initial design,
#'				or when created by the optimizer. Default is \code{1}.}
#'   \item{\code{seedFun}}{An initial seed for the objective function in case of noise, by default \code{NA}. The default means that no seed is set.
#'				The user should be very careful with this setting. It is intended to generate reproducible experiments for each objective
#'				function evaluation, e.g., when tuning non-deterministic algorithms. If the objective function uses a constant number
#'				of random number generations, this may be undesirable. Note, that this seed is by default set prior to each evaluation. A replicated
#'				evaluation will receive an incremented value of the seed.
#'				Sometimes, the user may want to call external code using random numbers. To allow for that case, the user can specify an objective function (\code{fun}),
#' 				which has a second parameter \code{seed}, in addition to first parameter (matrix \code{x}). This seed can then be passed
#'				to the external code, for random number generator initialization. See end of examples section for a demonstration.}
#'   \item{\code{seedSPOT}}{This value is used to initialize the random number generator. It ensures that experiments are reproducible. Default is \code{1}.}
#'   \item{\code{duplicate}}{In case of a deterministic (non-noisy) objective function, this handles duplicated candidate solutions.
#'				By default (\code{duplicate = "EXPLORE"}), duplicates are replaced by new candidate solutions, generated by random
#'				sampling with uniform distribution. If desired, the user can set this to "STOP", which means that the optimization
#'				stops and results are returned to the user (with a warning). This may be desirable, as duplicates can be a indicator
#'				for convergence, or for a problem with the configuration.
#'				In case of noise, duplicates are allowed.}
#'   \item{\code{plots}}{Whether progress should be tracked by a line plot, default is \code{FALSE}}
#'   \item{\code{progress}}{Whether progress should be visualized, default is \code{FALSE}}
#'   \item{\code{infillCriterion}}{A function defining an infillCriterion to be used while optimizing a model. Default: NULL. For example check infillExpectedImprovement}
#'   \item{\code{verbosity}}{Integer level specifying how much output should be given by SPOT. 0 (default) ignores warnings of internal optimizers /models.
#'              1 will show warnings and output.}
#'   \item{\code{maxTime}}{\code{num} Maximum allowed run time (in minutes) for \code{spot} or \code{spotLoop}. 
#'          The default value for \code{maxTime} (in minutes) is \code{Inf} and can be overwritten by the user.
#'          The internal value \code{startTime}, that is used to control \code{maxTime},
#'          will be set by \code{\link{spotFillControlList}}. 
#'          Note: \code{maxTime}  is only an approximate value. It does not affect the \code{directOpt} run.
#'   }            
#' }
#' @param dimension problem dimension, that is, the number of optimized parameters.
#'
#' @return a list
#' @export
#' @keywords internal
spotControl <- function(dimension) {
  list(
    funEvals = 20,
    types = rep("numeric", dimension),
    design = designLHD,
    designControl = list(),
    subsetSelect = selectAll,
    subsetControl = list(),
    direct = FALSE,
    model = buildKriging,
    modelControl = list(modelInitialized = FALSE),
    optimizer = optimLHD,
    optimizerControl = list(),
    directOpt = optimNLOPTR,
    ## default: do not use directOpt:
    directOptControl = list(funEvals=0),
    plots = FALSE,
    progress = FALSE,
    OCBA = FALSE,
    OCBABudget = 1,
    #the budget available to OCBA, to be distributed to replications of "old" solutions
    replicates = 1,
    # number of new design points proposed by the surrogate model (last rows of the xnew matrix)
    xNewActualSize = 0,
    #the number of replications for all "new" solutions (unless generated by the initial design, which handles them separately)
    noise = FALSE,
    #whether or not the target function is non-deterministic.
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
    infillCriterion = NULL,
    ## By default no infillCriterion is used
    verbosity = 0,
    ## Start time will be set in spotFillControlList()
    maxTime = Inf
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
  #Initial Input Checking
  initialInputCheck(x, fun, lower, upper, control, inSpotLoop = TRUE)
  
  ## default settings
  dimension <- length(lower)
  con <- spotControl(dimension)
  con[names(control)] <- control
  control <- con
  rm(con)
  
  control <- spotFillControlList(control, dimension)
  
  ## Initialize evaluation counter
  count <- nrow(y)
  
  ## Main Loop
  modelFit <- NA
  ybestVec <- rep(min(y[,1]), count)
  while ((count < control$funEvals) & 
         (difftime(Sys.time(), control$time$startTime, units='mins') 
          < control$maxTime)) {
    
    ## Select points for model building
    selectRes <- control$subsetSelect(x = x,
                                      y = y[,1,drop=FALSE],
                                      control = control$subsetControl)
    
    ## Model building using control$model (default is buildKriging). 
    ## Based on fit
    modelFit <- control$model(x = selectRes$x,
                              y = selectRes$y,
                              control = control$modelControl) #todo return modelControl to allow memory?
    ## Some models can be updated. So, we store information whether the model is already build.
    control$modelControl$modelInitialized <- TRUE
    
    ## Generate a surrogate target function from the model. Based on predict.
    funSurrogate <-
      evaluateModel(modelFit, control$infillCriterion)
    
    ## Model optimization
    indexBest <- which.min(y[,1,drop=FALSE])
    xbest <- x[indexBest, , drop = FALSE]
   
    if (!is.null(control$optimizerControl$eval_g_ineq)  && 
        (control$optimizerControl$opts$algorithm == "NLOPT_GN_ISRES"  & control$optimizerControl$eval_g_ineq(xbest) < 0)
        ) {
      ## xbest does not satisfy ineq constraint =>
      ## xbest will NOT be used as a starting point x0
      x0 <- NULL
       } else{
      ## xbest does satisfy ineq constraint OR 
      ## no constraint function is used =>
      ## take xbest as starting point x0
      x0 <- xbest
    }
    
    optimResSurr <- control$optimizer(x = x0,
                                      funSurrogate,
                                      lower,
                                      upper,
                                      control$optimizerControl) #todo return optimizerControl to allow memory?
    xnew <- optimResSurr$xbest
    
  
    ## Handling of duplicates
    xnew <-
      duplicateAndReplicateHandling(xnew, x, lower, upper, control)
    
    ## Rounding non-numeric values produced by the optimizer
    xnew <- repairNonNumeric(xnew, control$types)
    
    ## If desired, use OCBA to handle replications of old solutions
    if (control$noise & control$OCBA) {
      xnew <- rbind(xnew, repeatsOCBA(x, y[,1,drop=FALSE], control$OCBABudget))
    }
    
    ## Prevent exceeding the budget:
    xnew <-
      xnew[1:min(max(control$funEvals - count, 1), nrow(xnew)), , drop = FALSE]
    
    ## Determine number of new design points:
    control$xNewActualSize <- nrow(xnew)
    
    ## Evaluation with objective function
    ynew <-
      objectiveFunctionEvaluation(
        x = x,
        xnew = xnew,
        fun = fun,
        seedFun = control$seedFun,
        noise = control$noise,
        ...
      )
    
    ##
    colnames(xnew) <- colnames(x)
    x <- rbind(x, xnew)
    y <- rbind(y, ynew)
    count <- count + nrow(ynew)
    
    ## Plots, output, etc
    indexBest <- which.min(y[,1,drop=FALSE])
    ybestVec <- c(ybestVec , y[indexBest, 1, drop = FALSE])
    if (control$plots) {
      plot(y[,1], type = "l", main = paste("ybest:", min(ybestVec)))
      abline(a = 0, b = 0)
      lines(1:length(ybestVec), ybestVec, col = "red")
    }
    
    ## Progress
    if (control$progress & (control$funEvals > 0)) {
      cat(paste0(round(count / (
        control$funEvals
      ) * 100), "% completed.\n"))
      if (count == (control$funEvals))
        cat("Done.\n")
    }
  } # while loop
  
  indexBest <- which.min(y[,1,drop=FALSE])
  if(ncol(y)>1)
    logInfo <- y[,-1,drop=FALSE]
  else
    logInfo <- NA
  list(
    xbest = x[indexBest, , drop = FALSE],
    ybest = y[indexBest, 1, drop = FALSE],
    x = x,
    y = y[,1,drop=FALSE],
    logInfo = logInfo,
    count = count,
    msg = "budget exhausted",
    modelFit = modelFit,
    ybestVec = ybestVec
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
spotCleanup <- function(control){
  if (class(control) == "spotGaussianProcessModel"){
    deleteGPsep(control$fit)
    deleteGPs()
    deleteGPseps()
  }
}
  

