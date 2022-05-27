context("Duplicates and Replicates")

test_that("check that no NA´s are created when second best variance is zero", {
    conf <- readRDS(testthat::test_path("testData", "debugSave.rds"))
    #print(conf)
    expect_warning(
        OCBA(sMean = conf[[1]], sVar = conf[[2]], n = conf[[3]], addBudget = conf[[4]])
    )
})

test_that("OCBA when all means are the same", {
    conf <- readRDS(testthat::test_path("testData", "debugSave.rds"))
    
    conf[[1]][1:21] <- conf[[1]][1]
    conf[[2]][1:21] <- 10e-6
    
    res <- OCBA(sMean = conf[[1]], sVar = conf[[2]], n = conf[[3]], addBudget = conf[[4]])
    
    expect_equal(conf[[4]], sum(res))
    expect_equal(conf[[4]][1], sum(res))
})

test_that("OCBA returns a reasonable answer", {
    conf <- readRDS(testthat::test_path("testData", "debugSave.rds"))
    
    # give fake variance to this index
    conf[[2]][20] <- 1e-5
    conf[[4]] <- 10
    
    conf[[2]] <- conf[[2]] * 10
    
    expect_warning(
        res <- OCBA(sMean = conf[[1]], sVar = conf[[2]], n = conf[[3]], addBudget = conf[[4]])
    )
    
    # # assigned replicates:
    # print(res)
    # # order by goodness
    # allowedByGoodness <- rep(0,21)
    # allowedByGoodness[order(conf[[1]])[1:10]] <- 1
    # print(allowedByGoodness)
    # 
    # # order by variance
    # allowedByVariance <- rep(0,21)
    # allowedByVariance[order(conf[[2]])[1:10]] <- 1
    # print(allowedByVariance)
    # 
    # ## print direct comparison
    # print(res)
    # print(as.numeric(allowedByGoodness | allowedByVariance))
    # print(conf[[3]])
    # 
    # pDF <- data.frame("means" = conf[[1]], "var" = conf[[2]], "repeats" = res, "index" = 1:21)
    # pDF <- pDF[pDF$var>0,]
    # 
    # ggplot(pDF, aes(x=index, y=means, colour=!(repeats>0))) + 
    #     geom_errorbar(aes(ymin=means-var, ymax=means+var), width=1) +
    #     geom_point()
    
    
    expect_equal(conf[[4]], sum(res))
})



test_that("check repeatsOCBA: are <= #OCBABudget solutions proposed?", {
  set.seed(1)
  fun = funError 
  lower <- c(-1, -1)
  upper <- c(1, 1)
  control <- list(replicates=2,
                  OCBA = TRUE,
                  OCBABudget = 3,
                  noise = TRUE,
                  multiStart=1,
                  designControl = list(size=10,
                                       replicates=2),
                  yImputation = list(handleNAsMethod = handleNAsMean,
                                    imputeCriteriaFuns = list(is.infinite, is.na, is.nan),
                                    penaltyImputation = 3)
  )
  control <- spotFillControlList(control, lower, upper)
  
  
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
    control = control
  )
  
  # spotLoop
  initialInputCheck(x, fun, lower, upper, control, inSpotLoop = TRUE)
  
  # controlList update
  dimension <- length(lower)
  con <- spotControl(dimension)
  con[names(control)] <- control
  control <- con
  rm(con)
  control <- spotFillControlList(control, lower, upper)
  
  if (!is.null(control$yImputation$handleNAsMethod)) {
    y <- imputeY(x = x,
                 y = y,
                 control = control)
  }
  
  # Counter and logs
  count <- nrow(y)
  modelFit <- NA
  ybestVec <- rep(min(y[, 1]), count)
  ySurr <- matrix(NA, nrow = 1, ncol = count)
  
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
  
  # Random starting points for optimization on the surrogate
  x0 <- getMultiStartPoints(x, y, control)
  resSurr <- matrix(NA, nrow = nrow(x0), ncol = ncol(x0) + 1)
  
  # Search on the surrogate with starting point/s x0:
  for (i in 1:nrow(x0)) {
    optimResSurr <- control$optimizer(x = x0[i, , drop = FALSE],
                                      funSurrogate,
                                      lower,
                                      upper,
                                      control$optimizerControl)
    resSurr[i,] <- c(optimResSurr$xbest, optimResSurr$ybest)
  }
  
  # Compile results from search on surrogate
  resAll <- data.frame(cbind(x0, resSurr))
  m <- which.min(resAll[, 2 * ncol(x) + 1])
  xnew <- as.matrix(resAll[m, (ncol(x) + 1):(2 * ncol(x))])
  ySurrNew <- resAll[m, 2 * ncol(x) + 1]
  
  # Handling of duplicates
  xnew <-
    duplicateAndReplicateHandling(xnew, x, lower, upper, control)
  
  # Repair non-numeric results
  xnew <- repairNonNumeric(xnew, control$types)
  
  # OCBA
  if (control$noise &
      control$OCBA) {
    xnew <-
      rbind(xnew, expect_warning(repeatsOCBA(x, y[, 1, drop = FALSE], control$OCBABudget, verbosity = 1)))
  }
  
   #expect_equal(dim(xnew), dim(result$xBestOcba))
})



test_that("check rankingOcba for multiple y values", {
  set.seed(1)
  fun = funMoo 
  lower <- c(-1, -1)
  upper <- c(1, 1)
  control <- list(replicates=2,
                  OCBA = TRUE,
                  OCBABudget = 3,
                  noise = TRUE,
                  multiStart=1,
                  designControl = list(size=10,
                                       replicates=2)
                  )
  control <- spotFillControlList(control, lower, upper)
  
  
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
    control = control
  )
  
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
  ySurr <- matrix(NA, nrow = 1, ncol = count)
  
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
  
  # Random starting points for optimization on the surrogate
  x0 <- getMultiStartPoints(x, y, control)
  resSurr <- matrix(NA, nrow = nrow(x0), ncol = ncol(x0) + 1)
  
  # Search on the surrogate with starting point/s x0:
  for (i in 1:nrow(x0)) {
    optimResSurr <- control$optimizer(x = x0[i, , drop = FALSE],
                                      funSurrogate,
                                      lower,
                                      upper,
                                      control$optimizerControl)
    resSurr[i,] <- c(optimResSurr$xbest, optimResSurr$ybest)
  }
  
  # Compile results from search on surrogate
  resAll <- data.frame(cbind(x0, resSurr))
  m <- which.min(resAll[, 2 * ncol(x) + 1])
  xnew <- as.matrix(resAll[m, (ncol(x) + 1):(2 * ncol(x))])
  ySurrNew <- resAll[m, 2 * ncol(x) + 1]
  
  # Handling of duplicates
  xnew <-
    duplicateAndReplicateHandling(xnew, x, lower, upper, control)
  
  # Repair non-numeric results
  xnew <- repairNonNumeric(xnew, control$types)
  
  # OCBA
  if (control$noise &
      control$OCBA) {
    xnew <-
      rbind(xnew, repeatsOCBA(x, y[, 1, drop = FALSE], control$OCBABudget))
  }
  
  # Evaluate xnew
  ynew <- objectiveFunctionEvaluation(
    x = x,
    xnew = xnew,
    fun = fun,
    control = control
  )
  
  # Combine before impute. This provides a larger basis for imputation.
  colnames(xnew) <- colnames(x)
  x <- rbind(x, xnew)
  y <- rbind(y, ynew)
  
  # Treating NA and Inf for new values
  if (!is.null(control$yImputation$handleNAsMethod)) {
    y <- imputeY(x = x,
                 y = y,
                 control = control)
  }
  
  # Update counter, logs, etc.
  ySurr <- c(ySurr, ySurrNew)
  count <- count + nrow(ynew)
  indexBest <- which.min(y[, 1, drop = FALSE])
  ybestVec <- c(ybestVec , y[indexBest, 1, drop = FALSE])
  if (ncol(y) > 1) {
    logInfo <- y[,-1, drop = FALSE]
  }  else{
    logInfo <- NA
  }
  if (length(control$transformFun) > 0) {
    xt <- transformX(xNat = x, fn = control$transformFun)
  } else {
    xt <- NA
  }
  
  if (control$noise & control$OCBA) {
    ocbaRes <- ocbaRanking(x=x,
                           y=y,
                           fun=fun, 
                           control=control)
    control$xBestOcba <- ocbaRes[1, 1:(ncol(ocbaRes)-1), drop=FALSE]
    control$yBestOcba <- ocbaRes[1, ncol(ocbaRes), drop=FALSE]
  }
  result <- list(
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
  expect_equal(dim(result$xbest), dim(result$xBestOcba))
})
