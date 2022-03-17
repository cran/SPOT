context("Tools")

test_that("check tool functions are evaluated without errors", {
  fn <- c("identity", "exp", "cos")
  xNat <- diag(3)
  A <- transformX(xNat, fn)
  expect_equal(sum(A[1, ]), 3)
})

test_that("check results are logged correctly: xt is used to calculate y, i.e.,
          y=f(xt) and not y=f(x)", {
  set.seed(1)
  lower <- c(-100, -100)
  upper <- c(10, 10)
  #transformFun <- rep("identity", length(lower))
  transformFun <- rep("exp", length(lower))
  n <- 11
  res <- spot(x=NULL,
              fun=funSphere,
              lower=lower, 
              upper=upper,
              control=list(funEvals=n,
                           modelControl=list(target="ei"),
                           optimizer=optimLBFGSB,
                           transformFun=transformFun,
                           verbosity = 0,
                           progress = FALSE,
                           plots = FALSE))
    expect_equal(funSphere(matrix(res$xt,n,2)), res$y)
})

test_that("check multi start point genearation", {
  control <- spotControl(dimension=2)
  control$lower <- rep(0,2)
  control$upper <- rep(1,2)
  set.seed(1)
  x <- matrix(runif(10,min=2, max=3), nrow = 5, ncol = 2)
  # best values at pos. 5
  x[5,] <- c(0,0)
  y <- funSphere(x)
  y[5,] <- 0
  control$multiStart <- 1
  x0 <- getMultiStartPoints(x,y,control)
  expect_equal(sum(abs(x0)),0)
  control$multiStart <- 3
  x0 <- getMultiStartPoints(x,y,control)
  expect_equal(nrow(x0),3)
  expect_equal(ncol(x0),2)  
  # first entry( 1st row) of x0 should contain zeros:
  expect_equal(sum(abs(x0[1,])),0)
  ## with constraints:
  ## all are feasible
  x <- matrix(c(1,2,
                1,1.1,
                0.1, 0.2,
                2,2.1,
                3,32
                ), nrow = 5, ncol = 2, byrow=TRUE)
  control$optimizerControl$eval_g_ineq <- function(x) x[1]-x[2] 
  control$optimizerControl$opts$algorithm <- "NLOPT_GN_ISRES"
  # check feasibility
  expect_true(all(apply(x,1,control$optimizerControl$eval_g_ineq) < 0))
  #control$optimizerControl$eval_g_ineq(x[5,]) < 0
  y <- funSphere(x)
  indexBest <- which.min(y[, 1, drop = FALSE])
  xbest <- x[indexBest, , drop = FALSE]
  ybest <- y[indexBest, , drop = FALSE]
  checkFeasibilityNlopGnIngres(xbest,control)
  # best values at pos. 3
  control$multiStart <- 1
  x0 <- getMultiStartPoints(x,y,control)
  expect_equal(sum(abs(x0)),0.3)
  
  
  ## x is not feasible any more
  x <- -x
  ## test infeasibility assumption  
  expect_true(!all(apply(x,1,control$optimizerControl$eval_g_ineq) < 0))
  y <- funSphere(x)
  
  ## 1. only one multi start point, i.e., no multi starts
  ## best values at pos. 3
  control$multiStart <- 1
  x0 <- getMultiStartPoints(x,y,control)
  # not feasible -> return NULL
  expect_null(x0)
  
  ## 2. three multi start points, since xbest is infeasible, three
  ## random points are returned
  control$multiStart <- 3
  x0 <- getMultiStartPoints(x,y,control)
  # xbest not feasible -> return (3,2)-random matrix without xbest
  expect_equal(nrow(x0),3)
  expect_equal(ncol(x0),2)  
})

