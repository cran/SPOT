context("evaluateModel")

test_that("test evaluate model", {
  set.seed(1)
  #fun <- funNoise
  fun <- funError
  lower <- c(-1,-1)
  upper <- c(1,1)
  
  dim = 2
  lower = c(-2, -3)
  upper = c(1, 2)
  
  fun <- function(X){
    y <- funSphere(X)
    naMarker <- sample(c(FALSE,TRUE), nrow(X), replace = TRUE, prob = c(0.9,0.1))
    if(sum(naMarker) > 0){
      y[naMarker,] <- NA
    }
    return(y)
  }
  
  
  
  control <- list(OCBA=TRUE,
                  OCBABudget=3,
                  replicates=2,
                  noise=TRUE,
                  designControl = list(replicates=2))
  control <- spotFillControlList(control, lower, upper)
  control$multiStart <- 2
  control$yImputation <- list(
    handleNAsMethod = handleNAsMean,
    imputeCriteriaFuns = list(is.infinite, is.na, is.nan),
    penaltyImputation = 3
  )
  # Initial design  
  x <- control$design(
    x = NULL,
    lower = lower,
    upper = upper,
    control = control$designControl
  )
  x <- repairNonNumeric(x, control$types)
  
  # Eval initial design
  y <-  objectiveFunctionEvaluation(
    x = NULL,
    xnew = x,
    fun = fun,
    control = control)
  if (!is.null(control$yImputation$handleNAsMethod)) {
    y <- imputeY(x = x,
                 y = y,
                 control = control)
  }
  
  # spotLoop
  initialInputCheck(x, fun, lower, upper, control, inSpotLoop = TRUE)
  
  # controlList update
  dimension <- length(lower)
  con <- spotControl(dimension)
  con[names(control)] <- control
  control <- con
  rm(con)
  control <- spotFillControlList(control, lower, upper)
  
  # Counter and logs
  count <- nrow(y)  
  modelFit <- NA
  ybestVec <- rep(min(y[, 1]), count)
  ySurr <- matrix(NA, nrow=1, ncol=count)
  
  # Subsect select
  selectRes <-  control$subsetSelect(x = x,
                                     y = y[, 1, drop = FALSE],
                                     control = control$subsetControl)
  
  # Surrogate model fit
  modelFit <- control$model(x = selectRes$x,
                            y = selectRes$y,
                            control = control$modelControl)
  
  # Optimization function on the surrogate
  funSurrogate <-  evaluateModel(modelFit, 
                                 control$infillCriterion, 
                                 control$verbosity)
  x0 <- getMultiStartPoints(x, y, control)
  resSurr <- matrix(NA, nrow = nrow(x0), ncol = ncol(x0) + 1)
  for (i in 1:nrow(x0)) {
  optimResSurr <- try(control$optimizer(x = x0[i, , drop = FALSE],
                                        funSurrogate,
                                        lower,
                                        upper,
                                        control$optimizerControl))
  optimResSurr <- list(xbest=x0[i, , drop = FALSE],
                       ybest = matrix(NA, nrow=1))
  resSurr[i,] <- c(optimResSurr$xbest, optimResSurr$ybest)
  }
  resSurr
  
  impY <- matrix(imputeY(x = rbind(resSurr[, 1:ncol(x0)],x),
                         y = rbind(resSurr[, ncol(x0)+1, drop=FALSE],y),
                         control = control), 
                 ncol=1)
  resAll <- data.frame(cbind(x0, resSurr[, 1:ncol(x0)], impY[1:nrow(x0), ,drop=FALSE]))
  m <- which.min(resAll[, 2 * ncol(x) + 1])
  xnew <- as.matrix(resAll[m, (ncol(x) + 1):(2 * ncol(x))])
  ySurrNew <- resAll[m, 2 * ncol(x) + 1]
  expect_true(!is.na(ySurrNew))
})
