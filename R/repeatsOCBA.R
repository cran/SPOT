#' @title Low Level OCBA
#'
#' @description  Compute the Optimal Computing Budget Allocation.
#'
#' @param sMean vector of sample means of candidate solutions.
#' @param sVar vector of sample variances of candidate solutions.
#' Note, that these should be non-zero. If the solutions with lowest or second lowest \code{mean} have zero variance,
#' This function will not distribute any additional evaluations, and return a vector of zeros.
#' If any other solution has zero observed variance, the respective solution will be ignored, and never
#' be assigned additional evaluations.
#' @param n number of already performed evaluations for each candidate solutions.
#' @param addBudget the number of additional evaluations (replications) that can be distributed to the candidate solutions.
#' @param verbosity verbosity (default: 0)
#' @return A vector that specifies how often each solution should be evaluated.
#'
#' @seealso \code{\link{repeatsOCBA}} is based on this function.
#'
#' @export
#' @examples
#' ## Simple test
#' OCBA(1:10,1:10,rep(4,10),3)
#' ## Example from the referenced book:
#' res <- OCBA(c(1.2,2.1,3.4,4.87,6.05),c(3.3,2.0,4.5,5.3,6.9),c(12,6,5,5,4),100)
#' ##expected result:
#' res == c(48,38,11,2,1)
#' @keywords internal
#' @references Chun-hung Chen and Loo Hay Lee. 2010. Stochastic Simulation Optimization: An Optimal Computing Budget Allocation (1st ed.). World Scientific Publishing Co., Inc., River Edge, NJ, USA.
OCBA <- function(sMean, sVar, n, addBudget, verbosity = 0) {

  nd <- length(sMean) #the number of designs
  # Some safety check
  if (length(sVar) != nd)
    stop("Vectors sMean and sVar for OCBA are required to have the same length.")
  # Total used budget after this step.
  totalBudget <- addBudget + sum(n)
  # Best observed mean value
  bestMeanValue <- min(sMean)
  # number of samples with best mean
  nrOfBest <- sum(bestMeanValue == sMean)
  sOrder <- order(sMean)
  # Fail safe for zero-variance cases
  indexVarZero <- numeric()
  # Warning if all variances are equal to zero
  if (length((sVar[sVar==0])) == length(sVar)){
    warning("all variances are 0", call. = FALSE)
  }
  # Warning if additional repititions are equal to zero
  if (addBudget==0){
    warning("number of additional evaluations is 0", call. = FALSE)
  }
  
  if (any(sVar == 0)) {
    # If the best and/or second best have zero variance, OCBA is stopped. The budget is not used for re-evaluation.
    if (any(sVar[sOrder][1:(nrOfBest + 1)] == 0)) {
      warning("Best or second best value has zero variance. Budget is not used for evaluation.", call. = FALSE)
      return(numeric(nd)) #return only zeros.    #TODO: or handle second best var zero differently? e.g.,
    }
    # If other samples have zero variance, they are removed from the list before the OCBA calculation
    indexVarZero <-
      which(sVar == 0) #TODO: zero variance fail safes need testing!
    if (any(sVar == 0)) {
    warning("Zero variance samples are removed before starting the calculations.", call. = FALSE)
    }
  }
 
  # If all (remaining samples) have the same mean, just assign additional budget to one sample
  if (nrOfBest == (nd - length(indexVarZero))) {
    additionalReplications <- numeric(nd)
    additionalReplications[1] <- addBudget
    return(additionalReplications)
  }
  # ID of the best
  bestID <-
    sOrder[1:nrOfBest]  #TODO: dieser ansatz mit mehreren besten ist nicht im referenz code...
  # ID of the sec. best
  secondBestID <- sOrder[nrOfBest + 1]
  #initialize ratios
  ratio <- numeric(nd)
  # Ratio for sec best (N_s / N_s = 1)
  ratio[secondBestID] <- 1
  # Ratio for all others
  for (i in 1:nd) {
    if (!(i %in% bestID) & i != secondBestID & !(i %in% indexVarZero)) {
      temp <-
        (sMean[secondBestID] - bestMeanValue) / (sMean[i] - bestMeanValue)
      ratio[i] <-
        temp ^ 2 * sVar[i] / sVar[secondBestID] #This is the Ratio N_i / N_s
    }
  }
  # Ratio for best
  if (length(indexVarZero) > 0)
    temp <- sum(ratio[-indexVarZero] ^ 2 / sVar[-indexVarZero])
  else
    temp <- sum(ratio ^ 2 / sVar)
  ratio[bestID] <- sqrt(sVar[bestID] * temp)
  # Initialize allocation loop
  active <- rep(TRUE, nd)
  moreAlloc <- TRUE
  tempBudget <- totalBudget
  desiredReplications <- numeric(nd)
  # Allocation loop
  while (moreAlloc) {
    moreAlloc <- FALSE
    # sum of ratios of active samples
    ratioSum <- sum(ratio[active])
    # calculate number of desired replications of active samples
    desiredReplications[active] <-
      floor((tempBudget / ratioSum) * ratio[active])
    if(verbosity > 0){
    message("desiredReplications")
    print(desiredReplications)
    }
    # print(floor((tempBudget/ratioSum)*ratio[active]))
    # determine samples that already have reached or exceeded the desired 
    # number of replications
    moreThanDesired <- (desiredReplications < n)
    if(verbosity > 0){
    message("moreThanDesired:")
    print(moreThanDesired)
    message("n:")
    print(n)
    }
    # set number of desired replications to number of existing replications
    desiredReplications[moreThanDesired] <- n[moreThanDesired]
    moreAlloc <- any(moreThanDesired)
    active[moreThanDesired] <- FALSE
    if (moreAlloc)
      tempBudget <- totalBudget - sum(desiredReplications[!active])
  }
  #print(desiredReplications)
  additionalReplications <- desiredReplications - n
  ## Differenz wird dem Besten zugeschlagen  #TODO: wenn mehrere beste: die mit der groessten varianz.
  ## Das ist ein bugfix, vorher haben alle das volle budget bekommen.
  tempBudget <- sum(desiredReplications)
  bestIdHighestVariance <-
    bestID[which.max(sVar[bestID])] #if sVar is also equal, choose first.
  additionalReplications[bestIdHighestVariance] <-
    additionalReplications[bestIdHighestVariance] + (totalBudget - tempBudget)
  additionalReplications
}
##test book example, zero variance cases, test sum of vector = addbudget...


#' @title Optimal Computing Budget Allocation
#'
#' @description Simple interface to the Optimal Computing Budget Allocation algorithm.
#'
#' @param x matrix of samples. Identical rows indicate repeated evaluations. 
#' Any sample should be evaluated at least twice, to get an estimate of the variance.
#' @param y observations of the respective samples. For repeated evaluations, y should differ (variance not zero).
#' @param budget of additional evaluations to be allocated to the samples.
#' @param verbosity verbosity 
#' @importFrom stats var
#'
#' @return A vector that specifies how often each solution should be evaluated.
#'
#' @seealso repeatsOCBA calls \code{\link{OCBA}}, which also provides some additional details.
#'
#' @export
#' @examples
#' x <- matrix(c(1:3,1:3),9,2)
#' y <- runif(9)
#' repeatsOCBA(x,y,10)
#' @references Chun-hung Chen and Loo Hay Lee. 2010. Stochastic Simulation Optimization: An Optimal Computing Budget Allocation (1st ed.). World Scientific Publishing Co., Inc., River Edge, NJ, USA.
repeatsOCBA <-
  function(x, y, budget, verbosity = 0) {
    #matrix x and budget for OCBA   #TODO: y matrix?
    x <- data.matrix(x)
    xlist <- split(x, row(x))
    uniquex <- unique(xlist)
    sVar <- NULL
    sMean <- NULL
    n <- NULL
    for (xi in uniquex) {
      ind <- xlist %in% list(xi)
      n <- c(n, sum(ind))
      sVar <- c(sVar, var(y[ind]))
      sMean <- c(sMean, mean(y[ind]))
    }
    OCBAres <-
      OCBA(sMean, sVar, n, budget, verbosity = verbosity) 
    # FIXME: Find a better solution if OCBAres is all zeros...
    if(sum(OCBAres) == 0){
      OCBAres <- c(rep(1,budget), rep(0, length(OCBAres)-budget))
    }
    uniquex <-
      matrix(unlist(uniquex),
             nrow = length(uniquex),
             byrow = TRUE)
    #return:
    uniquex[unlist(mapply(rep, 1:length(OCBAres), OCBAres)), ]
  }

#' @title ocbaRanking
#' @description Return the ocba ranking (xbest, ybest) for noisy optimization
#' @details Based on \code{\link{repeatsOCBA}}
#' 
#' @param x matrix of x values
#' @param y matrix of y values, one dimensional!
#' @param fun objective function
#' @param control control list, see \code{\link{spotControl}}
#' @param ... additional arguments to fun
#' 
#' @importFrom stats aggregate
#' 
#' @return (x,y) matrix of sorted (by y) values. In case of noise are these values aggregated (y-mean) values.
#' 
#' @export
ocbaRanking <- function(x, y, fun, control, ...) {
  ## consider first y-dim only: 
 y <- y[, 1, drop = FALSE]
 try(xnew <- repeatsOCBA(x, y,
                                        control$OCBABudget))
  if (control$verbosity  >  0) {
    message("xnew after OCBA in finalRanking():")
    print(xnew)
  }
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
      message("ynew: objectiveFunctionEvaluation() in finalRanking(): Caught an error!")
      print(e)
      if (!is.null(control$yImputation$handleNAsMethod)) {
        message("Error will be corrected using the configured NA handling technique.")
        n <- nrow(xnew)
        m <- ncol(y)
        return(matrix(rep(NA, m * n), nrow = n))
      }
    }
  )
  ## consider first y-dim only: 
  ynew <- ynew[, 1, drop = FALSE]
  ## Combine before impute. This provides a larger basis for imputation.
  colnames(xnew) <- colnames(x)
  x <- rbind(x, xnew)
  y <- rbind(y, ynew)
  
  ## Treating NA and Inf for new values
  if (!is.null(control$yImputation$handleNAsMethod)) {
    y <- imputeY(x = x,
                 y = y,
                 control = control)
  }
  df <- cbind(x,y)
  colnames(df) <- c(paste0("x", 1:dim(x)[2]), "y")
  res <- aggregate(y ~ ., data=df, FUN=mean) 
  res <- as.matrix(res[order(res$y),], ncol = ncol(x)+1)
  return(res)
}
  
  
  

