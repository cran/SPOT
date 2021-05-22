## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE--------------------------------------------------------------
#  library(microbenchmark)
#  library(SPOT)
#  library(babsim.hospital)
#  n <- 3
#  
#  ### Compare Run Time
#  x <- matrix(as.numeric(getParaSet(5374)[1,-1]),1,)
#  bounds <- getBounds()
#  lower <- bounds$lower
#  upper <- bounds$upper
#  
#  resb <- microbenchmark(
#    spot(x, funBaBSimHospital, lower , upper, control=list(funEvals=10*n)),
#    spot(x, funBaBSimHospital, lower , upper, control=list(funEvals=10*n, model = buildGaussianProcess)),
#    times = 2)
#  print(resb)
#  boxplot(resb)
#  
#  ### Compare Performance
#  rm(list=ls())
#  library(microbenchmark)
#  library(SPOT)
#  set.seed(1)
#  n <- 30
#  low = -2
#  up = 2
#  a = runif(n, low, 0)
#  b = runif(n, 0, up)
#  x0 = a + runif(n)*(b-a)
#  #plot(a, type = "l", ylim=c(up,low))
#  #lines(b)
#  #lines(x0)
#  x0 = matrix( x0, nrow = 1)
#  
#  set.seed(1)
#  perf1 <- spot(x= x0, funSphere, a, b, control=list(maxTime = 0.25, funEvals=10*n, plots=TRUE,
#                                                     model = buildKriging, optimizer=optimNLOPTR))
#  set.seed(1)
#  perf2 <- spot(x= x0, funSphere, a, b, control=list(maxTime = 0.25, funEvals=10*n, plots=TRUE,
#                model = buildGaussianProcess, optimizer=optimNLOPTR, directOptControl = list(funEvals=0)))
#  
#  set.seed(1)
#  perf3 <- spot(x= x0, funSphere, a, b, control=list(maxTime = 0.25, funEvals=10*n, plots=TRUE,
#                                                     model = buildGaussianProcess, optimizer=optimNLOPTR,
#                                                     directOptControl = list(funEvals=10)))
#  
#  ### Plot Repeats (Sphere Function)
#  rm(list=ls())
#  library(microbenchmark)
#  library(SPOT)
#  set.seed(1)
#  n <- 30
#  low = -2
#  up = 2
#  a = runif(n, low, 0)
#  b = runif(n, 0, up)
#  x0 = a + runif(n)*(b-a)
#  #plot(a, type = "l", ylim=c(up,low))
#  #lines(b)
#  #lines(x0)
#  x0 = matrix( x0, nrow = 1)
#  
#  reps <- 10
#  end <- 10*n
#  ninit <- n
#  
#  progSpot <- matrix(NA, nrow = reps, ncol = end)
#  for(r in 1:reps){
#    set.seed(r)
#    x0 <- a + runif(n)*(b-a)
#    x0 = matrix( x0, nrow = 1)
#    sol <- spot(x= x0, funSphere, a, b, control=list(funEvals=end,
#                                                     model = buildGaussianProcess,
#                                                     optimizer=optimNLOPTR,
#                                                     directOptControl = list(funEvals=0),
#                                                     designControl = list(size = ninit)))
#    progSpot[r, ] <- bov(sol$y, end)
#  
#  }
#  
#  matplot(t(progSpot), type="l", col="gray", lty=1,
#          xlab="n: blackbox evaluations", ylab="best objective value")
#  abline(v=ninit, lty=2)
#  legend("topright", "seed LHS", lty=2, bty="n")
#  
#  f <- funSphere
#  
#  
#  fprime <- function(x) {
#    x <- matrix( x, 1)
#    ynew <- as.vector(f(x))
#    y <<- c(y, ynew)
#    return(ynew)
#  }
#  
#  progOptim <- matrix(NA, nrow=reps, ncol=end)
#  for(r in 1:reps) {
#    y <- c()
#    x0 <- a + runif(n)*(b-a)
#    x0 <- matrix( x0, 1, )
#    os <- optim(x0, fprime, lower=a, upper=b, method="L-BFGS-B")
#    progOptim[r,] <- bov(y, end)
#  }
#  
#  
#  matplot(t(progOptim), type="l", col="red", lty=1,
#          xlab="n: blackbox evaluations", ylab="best objective value")
#  matlines(t(progSpot), type="l", col="gray", lty=1)
#  legend("topright", c("Spot", "optim"), col=c("gray", "red"), lty=1, bty="n")
#  
#  
#  ### babsim.hospital
#  rm(list=ls())
#  library(microbenchmark)
#  library(SPOT)
#  library(babsim.hospital)
#  library(nloptr)
#  library(parallel)
#  
#  ### New Babsim
#  getParallelBaseObjFun <- function(region = 5374, nCores = 2){
#    N_REPEATS = 10/nCores ## cores are used in parallel, change repeats if desired
#    singleRepeat <- function(index, x){
#      rkiwerte = babsim.hospital::rkidata
#      icuwerte = babsim.hospital::icudata
#      rkiwerte <- rkiwerte[rkiwerte$Refdatum <= as.Date("2020-12-09"),]
#      icuwerte <- icuwerte[icuwerte$daten_stand <= as.Date("2020-12-09"),]
#      region <- 5374
#      fun <- babsim.hospital:::getTrainTestObjFun(region = region,
#                                                  rkiwerte = rkiwerte,
#                                                  icuwerte = icuwerte,
#                                                  TrainSimStartDate = as.Date("2020-12-09") - 10*7,
#                                                  TrainFieldStartDate = as.Date("2020-12-09") - 6*7,
#                                                  tryOnTestSet = FALSE)
#      fun(x)
#    }
#    function(x){
#      res <- mclapply(1:N_REPEATS, singleRepeat, x, mc.cores = nCores)
#      y <- as.numeric(unlist(res))
#      median(y)
#    }
#  }
#  ## Call Example
#  objFun <- getParallelBaseObjFun()
#  objFun(as.numeric(babsim.hospital::getParaSet(5315)[1,-1]))
#  
#  
#  
#  ### Old Version
#  packageVersion("babsim.hospital")
#  funHosp <- getTrainTestObjFun(verbosity = 10000,
#                            parallel=TRUE,
#                            tryOnTestSet=FALSE,
#                            TrainSimStartDate = Sys.Date() - 12 * 7)
#  f <- function(x)
#  {matrix(apply(x, # matrix
#                1, # margin (apply over rows)
#                funHosp),
#          ,1) # number of columns
#  }
#  
#  lo <- getBounds()$lower
#  up <- getBounds()$upper
#  
#  n <- length(lo)
#  reps <- 10
#  end <- 3*n
#  ninit <- n+1
#  
#  para <- getStartParameter(region = 5374)
#  
#  progSpot <- matrix(NA, nrow = reps, ncol = end)
#  for(r in 1:reps){
#    set.seed(r)
#    x0 <- para[1,]
#    x0 = matrix( x0, nrow = 1)
#    sol <- spot(x= x0, f, lo, up, control=list(funEvals=end,
#                                                     model = buildGaussianProcess,
#                                                     optimizer=optimNLOPTR,
#                                                     directOptControl = list(funEvals= n),
#                                                     designControl = list(size = ninit)))
#    progSpot[r, ] <- bov(sol$y, end)
#  
#  }
#  
#  matplot(t(progSpot), type="l", col="gray", lty=1,
#          xlab="n: blackbox evaluations", ylab="best objective value")
#  abline(v=ninit, lty=2)
#  legend("topright", "seed LHS", lty=2, bty="n")
#  
#  ## f <- funSphere
#  
#  fprime <- function(x) {
#    x <- matrix( x, 1)
#    ynew <- as.vector(f(x))
#    y <<- c(y, ynew)
#    return(ynew)
#  }
#  
#  progOptim <- matrix(NA, nrow=reps, ncol=end)
#  for(r in 1:reps) {
#    y <- c()
#    x0 <- para[1,]
#    x0 <- matrix( x0, 1, )
#    os <- optim(x0, fprime, lower=lo, upper=up, method="L-BFGS-B",  control = list(maxit = end))
#    progOptim[r,] <- bov(y, end)
#  }
#  
#  
#  matplot(t(progOptim), type="l", col="red", lty=1,
#          xlab="n: blackbox evaluations", ylab="best objective value")
#  matlines(t(progSpot), type="l", col="gray", lty=1)
#  legend("topright", c("Spot", "optim"), col=c("gray", "red"), lty=1, bty="n")
#  
#  

