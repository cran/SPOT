context("Na removal")

test_that("NAs are correctly removed I: handleNAsKrigingWorst", {
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
    
    control <- spotControl(dimension = dim)
    control$verbosity = 0
    control$designControl$size = 10
    control$funEvals = 15
    control$yImputation$handleNAsMethod <- handleNAsKrigingWorst
   res <-   spot(x = NULL,
                 fun = fun,
                 lower = lower,
                 upper = upper,
                 control)
   expect_equal(res$count, control$funEvals)
})  

test_that("NAs are correctly removed II: handleNAsMean", {
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
  
  control <- spotControl(dimension = dim)
  control$verbosity = 0
  control$designControl$size = 10
  control$funEvals = 15
  control$yImputation$handleNAsMethod <- handleNAsMean
    res <- spot(x = NULL,
             fun = fun,
             lower = lower,
             upper = upper,
             control)
    expect_equal(res$count, control$funEvals)
})

test_that("NAs are correctly removed III: handleNAsKrigingWorst if no NAs occur", {
  dim = 2
  lower = c(-2, -3)
  upper = c(1, 2)
  
  control <- spotControl(dimension = dim)
  control$verbosity = 0
  control$designControl$size = 10
  control$funEvals = 15
  control$yImputation$handleNAsMethod <- handleNAsKrigingWorst
  control$verbosity <- 1
     res <- spot(x = NULL,
             fun = funSphere,
             lower = lower,
             upper = upper,
             control)
     expect_equal(res$count, control$funEvals)
})

test_that("NAs are correctly removed IV: controlList updated correctly?", {
  dim = 2
  lower = c(-2, -3)
  upper = c(1, 2)
  controlList <- list(funEvals=20,
             modelControl=list(target="ei"),
             verbosity = 1,
             designControl = list(size = 10),
             funEvals = 15,
             yImputation = list(handleNAsMethod = handleNAsKrigingWorst,
                                imputeCriteriaFuns = list(is.na)))

  c1 <- spotFillControlList(controlList=controlList, lower=lower, upper=upper)
  # check if imputeCriteriaFuns is initialized correctly
  expect_true(!is.null(c1$yImputation$imputeCriteriaFuns))
  
})

test_that("NAs are correctly removed V: update controlList + handleNAsKrigingWorst", {
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
  funEvals <- 15
  res <-  spot(x = NULL,
                fun = fun,
                lower = lower,
                upper = upper,
                control=list(funEvals=funEvals,
                            optimizer=optimLBFGSB,
                            modelControl=list(target="ei"),
                            verbosity = 1,
                            designControl = list(size = 10),
                            yImputation = list(handleNAsMethod = handleNAsKrigingWorst,
                                               imputeCriteriaFuns = list(is.na, is.infinite),
                                               penaltyImputation = 3)
                            )
  )
  expect_equal(res$count, funEvals)
})  

test_that("NA and Inf treatments", {
  control <- spotControl(dimension = 8)
#  control$yImputation$handleNAsMethod <- handleNAsKrigingWorst
x <- matrix(runif(20), ncol = 2)
y <- funSphere(x)
y[3] <- NA
y1 <- handleNAsKrigingWorst(x=x, y=y, imputeCriteriaFuns=control$yImputation$imputeCriteriaFuns)
expect_true(!any(is.na(y1)))

vecWithNAs <- c(-1, 0,1,NA,3,Inf,5,NA)
y1 <- handleNAsMean(x=NULL, y=vecWithNAs, imputeCriteriaFuns=control$yImputation$imputeCriteriaFuns)
expect_true(!any(is.na(y1)))
})



test_that("NaNs are correctly removed VI: handleNAsMean", {
  dim = 2
  lower = c(-2, -3)
  upper = c(1, 2)
  
  fun <- function(X){
    y <- funSphere(X)
    naMarker <- sample(c(FALSE,TRUE), nrow(X), replace = TRUE, prob = c(0.9,0.1))
    if(sum(naMarker) > 0){
      y[naMarker,] <- NaN
    }
    return(y)
  }
  
  control <- spotControl(dimension = dim)
  control$verbosity = 0
  control$designControl$size = 10
  control$funEvals = 15
  control$yImputation$handleNAsMethod <- handleNAsMean
  res <- spot(x = NULL,
              fun = fun,
              lower = lower,
              upper = upper,
              control)
  expect_equal(res$count, control$funEvals)
})

test_that("Infs are correctly removed VII: handleNAsMean", {
  dim = 2
  lower = c(-2, -3)
  upper = c(1, 2)
  
  fun <- function(X){
    y <- funSphere(X)
    naMarker <- sample(c(FALSE,TRUE), nrow(X), replace = TRUE, prob = c(0.9,0.1))
    if(sum(naMarker) > 0){
      y[naMarker,] <- Inf
    }
    return(y)
  }
  
  control <- spotControl(dimension = dim)
  control$verbosity = 0
  control$designControl$size = 10
  control$funEvals = 15
  control$yImputation$handleNAsMethod <- handleNAsMean
  res <- spot(x = NULL,
              fun = fun,
              lower = lower,
              upper = upper,
              control)
  expect_equal(res$count, control$funEvals)
})

test_that("Infs are correctly removed VIII: handleNAsMean multi objective", {
  dim = 2
  lower = c(-2, -3)
  upper = c(1, 2)
  
  fun <- function(X){
    y <- funMoo(X) + cbind(rep(-10,dim(X)[1]), rep(100, dim(X)[1]))
    naMarker <- sample(c(FALSE,TRUE), nrow(X), replace = TRUE, prob = c(0.9,0.1))
    if(sum(naMarker) > 0){
      y[naMarker,] <- c(-Inf, Inf)
    }
    return(y)
  }
  
  control <- spotControl(dimension = dim)
  control$verbosity = 0
  control$designControl$size = 10
  control$funEvals = 15
  control$yImputation$handleNAsMethod <- handleNAsMean
  res <- spot(x = NULL,
              fun = fun,
              lower = lower,
              upper = upper,
              control)
  expect_equal(res$count, control$funEvals)
})


test_that("funError(): Infs, NAs, NANs are correctly removed IX: handleNAsMean single objective", {
  dim = 3
  lower = rep(-1,dim)
  upper = rep(2, dim)
  fun <- function(x){funError(x=x, outDim=1)}
  control <- spotControl(dimension = dim)
  control$verbosity = 1
  control$designControl$size = 10
  control$funEvals = 15
  control$yImputation$handleNAsMethod <- handleNAsMean
  res <- spot(x = NULL,
              fun = fun,
              lower = lower,
              upper = upper,
              control)
  expect_equal(res$count, control$funEvals)
})

test_that("funError(): Infs, NAs, NANs are correctly removed X: handleNAsMean multi objective", {
  dim = 3
  lower = rep(-1,dim)
  upper = rep(2, dim)
  fun <- function(x){funError(x=x, outDim=7, prob=0.01)}
  control <- spotControl(dimension = dim)
  control$verbosity = 1
  control$designControl$size = 10
  control$funEvals = 15
  control$yImputation$handleNAsMethod <- handleNAsMean
  res <- spot(x = NULL,
              fun = fun,
              lower = lower,
              upper = upper,
              control)
  expect_equal(res$count, control$funEvals)
})


