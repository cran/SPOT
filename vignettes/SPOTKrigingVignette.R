## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----SPOT, eval = FALSE-------------------------------------------------------
#  ## install.packages("devtools")
#  ## devtools::install_github("r-lib/devtools")
#  url <- "http://owos.gm.fh-koeln.de:8055/bartz/spot.git"
#  devtools::install_git(url = url)

## ----setupSPOT----------------------------------------------------------------
library("SPOT")
packageVersion("SPOT")

## ---- fig.show="hold", fig.cap="Plot"-----------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'----------------------------------------------
knitr::kable(head(mtcars, 10))

## ---- eval =FALSE-------------------------------------------------------------
#  library(sensitivity)
#  x <- morris(model = morris.fun, factors = 20, r = 4,
#  design = list(type = "oat", levels = 5, grid.jump = 3))
#      print(x)
#      plot(x)
#  library(rgl)
#  plot3d.morris(x) # (requires the package 'rgl')
#  
#  morris.fun_matrix <- function(X){
#    res_vector <- morris.fun(X) cbind(res_vector, 2 * res_vector)
#  }
#  x <- morris(model = morris.fun_matrix, factors = 20, r = 4,design = list(type = "oat", levels = 5, grid.jump = 3))
#  plot(x, y_col = 2)
#  title(main = "y_col = 2")
#  # Also only for demonstration purposes: a model function returning a # three-dimensional array
#  morris.fun_array <- function(X){
#  res_vector <- morris.fun(X)
#  res_matrix <- cbind(res_vector, 2 * res_vector) array(data = c(res_matrix, 5 * res_matrix),
#  dim = c(length(res_vector), 2, 2))
#  }
#  x <- morris(model = morris.fun_array, factors = 20, r = 4,
#  design = list(type = "simplex", scale.factor = 1)) plot(x, y_col = 2, y_dim3 = 2)
#  title(main = "y_col = 2, y_dim3 = 2")
#  

## ---- eval =FALSE-------------------------------------------------------------
#  X.grid <- parameterSets(par.ranges=list(V1=c(1,1000),V2=c(1,4)), samples=c(10,10),method="grid")
#  plot(X.grid)

## ---- eval =FALSE-------------------------------------------------------------
#  library(randtoolbox)
#  X.sobol<-parameterSets(par.ranges=list(V1=c(1,1000),V2=c(1,4)),
#                                 samples=100,method="sobol")
#  plot(X.sobol)

## ---- eval =FALSE-------------------------------------------------------------
#  # a 100-sample with X1 ~ U(0.5, 1.5)
#  # X2 ~ U(1.5, 4.5)
#  # X3 ~ U(4.5, 13.5)
#  library(boot)
#  n <- 100
#  X <- data.frame(X1 = runif(n, 0.5, 1.5),
#                  X2 = runif(n, 1.5, 4.5),
#                  X3 = runif(n, 4.5, 13.5))
#      # linear model : Y = X1^2 + X2 + X3
#  y <- with(X, X1^2 + X2 +X3)
#  # sensitivity analysis
#  x <- pcc(X, y, nboot = 100)
#  print(x)
#  plot(x)
#  library(ggplot2)
#  ggplot(x)
#  
#  x <- pcc(X, y, semi = TRUE, nboot = 100)
#  print(x)
#  plot(x)
#  

## ---- eval =FALSE-------------------------------------------------------------
#  # a model with interactions
#  p <- 50
#  beta <- numeric(length = p)
#  beta[1:5] <- runif(n = 5, min = 10, max = 50)
#  beta[6:p] <- runif(n = p - 5, min = 0, max = 0.3)
#  beta <- sample(beta)
#  gamma <- matrix(data = runif(n = p^2, min = 0, max = 0.1), nrow = p, ncol = p)
#  gamma[lower.tri(gamma, diag = TRUE)] <- 0
#  gamma[1,2] <- 5
#  gamma[5,9] <- 12
#  f <- function(x) { return(sum(x * beta) + (x %*% gamma %*% x))}

## ---- eval =FALSE-------------------------------------------------------------
#  library(babsim.hospital)
#  library(sensitivity)
#  library(SPOT)
#  bounds <- getBounds()
#  lower <- matrix(bounds$lower,1,)
#  upper <- matrix(bounds$upper,1,)
#  # 10 iterations of SB
#  p <- 29
#  k = 29
#  sa <- sb(p, interaction = FALSE)
#  for (i in 1 : k) {
#    x <- ask(sa)
#    y <- list()
#  for (i in names(x)) {
#    print(x[[i]])
#    ## f muss eine Funktion sein, die für -1 den unteren Wert und für +1 den oberen
#    ## Wert
#    u <- matrix(x[[i]],1,)
#    u <- getNatDesignFromCoded(u, a = lower, b=upper)
#    y[[i]] <- as.numeric( funBaBSimHospital(u, nCores=20) )
#    }
#  tell(sa, y)
#  }
#  print(sa)
#  plot(sa)

## ---- eval = FALSE------------------------------------------------------------
#  library(sensitivity)
#  # Test case : the non-monotonic Sobol g-function
#  # The method of sobol requires 2 samples
#  # (there are 8 factors, all following the uniform distribution on [0,1]) library(boot)
#  n <- 1000
#  X1 <- data.frame(matrix(runif(8 * n), nrow = n))
#  X2 <- data.frame(matrix(runif(8 * n), nrow = n))
#  # sensitivity analysis
#  x <- sobol(model = sobol.fun, X1 = X1, X2 = X2, order = 2, nboot = 100)
#  print(x)
#  plot(x)
#      library(ggplot2)
#      ggplot(x)

## ---- eval = FALSE------------------------------------------------------------
#  library(sensitivity)
#  x <- fast99(model = NULL, factors = 29, n = 100, q="qunif",  q.arg = list(min = 0, max =1))
#  bounds <- getBounds()
#  lower <- bounds$lower
#  upper <- bounds$upper
#  x1 <- code2nat(matrix(x$X, lower, upper)
#  y <- funBaBSimHospital(x1)
#  tell(x,y)
#  print(x)
#  plot(x)

## ---- eval = FALSE------------------------------------------------------------
#  rm(list=ls())
#  library(microbenchmark)
#  library(SPOT)
#  library(babsim.hospital)
#  set.seed(1)

## ---- eval=FALSE--------------------------------------------------------------
#  # n = number of function evvaluations:
#  n <- 3
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

## ---- eval = FALSE------------------------------------------------------------
#  rm(list=ls())
#  library(microbenchmark)
#  library(SPOT)
#  library(babsim.hospital)
#  set.seed(1)
#  x0 <- matrix(as.numeric(getParaSet(5374)[1,-1]),1,)
#  bounds <- getBounds()
#  lower <- bounds$lower
#  upper <- bounds$upper
#  
#  set.seed(1)
#  perf1 <- spot(x= x0, funBaBSimHospital, lower , upper, control=list(time=list(maxTime = 1), funEvals=100, plots=FALSE,
#                                                     model = buildKriging, optimizer=optimNLOPTR), nCores =5)
#  set.seed(1)
#  perf2 <- spot(x= x0, funBaBSimHospital, lower , upper, control=list(time=list(maxTime = 1), funEvals=100, plots=FALSE,
#                model = buildGaussianProcess, optimizer=optimNLOPTR, directOptControl = list(funEvals=0)), nCores =5)
#  
#  set.seed(1)
#  perf3 <- spot(x= x0, funBaBSimHospital, lower , upper, control=list(time=list(maxTime = 1), funEvals=100, plots=FALSE,
#                                                     model = buildGaussianProcess, optimizer=optimNLOPTR,
#                                                     directOptControl = list(funEvals=10)), nCores = 5)

## ---- eval = FALSE------------------------------------------------------------
#  rm(list=ls())
#  library(microbenchmark)
#  library(SPOT)
#  library(babsim.hospital)
#  set.seed(1)
#  eps <- sqrt(.Machine$double.eps)
#  bounds <- getBounds()
#  a <- matrix(bounds$lower,1)
#  b <- matrix(bounds$upper,1)
#  d <- dim(a)[2]
#  
#  d = 20
#  fried <- function (n, d, a, b) {
#   #X <- designLHD(lower = lower, upper = upper, control = list(size = n))
#    X <- lhs::randomLHS(n, d)
#    ##XX <- code2nat(x=X, a=a, b=b)
#    ###Ytrue <- funBaBSimHospital(XX, totalRepeats = 5, nCores = 5)
#    Ytrue <- funSphere(X[,-1])
#    Y <- Ytrue
#  # Y <- Ytrue + rnorm(n, 0, 1)
#  return(data.frame(X, Y, Ytrue))
#  }

## ---- eval =FALSE-------------------------------------------------------------
#  data <- fried(n=25*d, d=d, a=a, b=b)
#  x=as.matrix(data[,1:d])
#  y=data$Y

## ---- eval = FALSE------------------------------------------------------------
#  fitTree <- buildTreeModel(x,y)
#  plot(fitTree)

## ---- eval = FALSE------------------------------------------------------------
#  fitKrigingDACE <- buildKrigingDACE(x,y)
#  print(fitKrigingDACE$like)

## ---- eval = FALSE------------------------------------------------------------
#  N <- 100*d
#  G <- 10*d
#  m <- q1 <- q2 <- matrix(NA, ncol=d, nrow=G)
#  grid <- seq(0, 1, length=G)
#  XX <- matrix(NA, ncol=d, nrow=N)
#  

## ---- eval = FALSE------------------------------------------------------------
#  bounds <- getBounds()
#  a <- matrix(bounds$lower,1)
#  b <- matrix(bounds$upper,1)
#  for(j in 1:d)
#    { for(i in 1:G) {
#    XX[,j] <- grid[i]
#    XX[,-j] <- lhs::randomLHS(N, d-1)
#    ## XX <- code2nat(XX, a, b)
#    ##p <- laGP::predGPsep(gpi, XX, lite=TRUE, nonug=TRUE)
#    fitKrigingDACE$target <- "s"
#    p <- predict(fitKrigingDACE, XX)
#    m[i,j] <- mean(p$y)
#    ## m[i,j] <- mean(p$mean)
#    ## q1[i,j] <- mean(qnorm(0.05, p$mean, sqrt(p$s2)))
#    ## q2[i,j] <- mean(qnorm(0.95, p$mean, sqrt(p$s2)))
#    q1[i,j] <- mean(qnorm(0.05, p$y, sqrt(p$s)))
#    q2[i,j] <- mean(qnorm(0.95, p$y, sqrt(p$s)))
#  
#    }
#    }

## ---- eval = FALSE------------------------------------------------------------
#  plot(0, xlab="grid", ylab="main effect", xlim=c(0,1),
#       ylim=range(c(q1,q2)), type="n")
#  for(j in 1:d) lines(grid, m[,j], col=j, lwd=2)
#  legend("bottomright", paste0("x", 1:d), fill=1:d, horiz=TRUE, cex=0.75)

## ---- eval =FALSE-------------------------------------------------------------
#  gpi <- laGP::newGPsep(as.matrix(data[,1:29]), data$Y, d=0.1,g=var(data$Y)/10, dK=TRUE)
#  mle <- laGP::mleGPsep(gpi, param="both", tmin=rep(eps, 2),
#  tmax=c(10, var(data$Y)))

## ---- eval = FALSE------------------------------------------------------------
#  N <- 1000
#  G <- 30
#  m <- q1 <- q2 <- matrix(NA, ncol=29, nrow=G)
#  grid <- seq(0, 1, length=G)
#  XX <- matrix(NA, ncol=29, nrow=N)
#  

## ---- eval = FALSE------------------------------------------------------------
#  bounds <- getBounds()
#  a <- matrix(bounds$lower,1)
#  b <- matrix(bounds$upper,1)
#  for(j in 1:29)
#    { for(i in 1:G) {
#    XX[,j] <- grid[i]
#    XX[,-j] <- lhs::randomLHS(N, 28)
#    ## XXX <- code2nat(XX, a, b)
#    p <- laGP::predGPsep(gpi, XX, lite=TRUE, nonug=TRUE)
#    m[i,j] <- mean(p$mean)
#    q1[i,j] <- mean(qnorm(0.05, p$mean, sqrt(p$s2)))
#    q2[i,j] <- mean(qnorm(0.95, p$mean, sqrt(p$s2)))
#    }
#    }

## ---- eval = FALSE------------------------------------------------------------
#  plot(0, xlab="grid", ylab="main effect", xlim=c(0,1),
#       ylim=range(c(q1,q2)), type="n")
#  for(j in 1:29) lines(grid, m[,j], col=j, lwd=2)
#  legend("bottomright", paste0("x", 1:29), fill=1:29, horiz=TRUE, cex=0.75)

## ---- eval = FALSE------------------------------------------------------------
#  # n = problem dim
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
#  perf1 <- spot(x= x0, funSphere, a, b, control=list(time=list(maxTime = 0.25), funEvals=10*n, plots=TRUE,
#                                                     model = buildKriging, optimizer=optimNLOPTR))
#  set.seed(1)
#  perf2 <- spot(x= x0, funSphere, a, b, control=list(time=list(maxTime = 0.25), funEvals=10*n, plots=TRUE,
#                model = buildGaussianProcess, optimizer=optimNLOPTR, directOptControl = list(funEvals=0)))
#  
#  set.seed(1)
#  perf3 <- spot(x= x0, funSphere, a, b, control=list(time=list(maxTime = 0.25), funEvals=10*n, plots=TRUE,
#                                                     model = buildGaussianProcess, optimizer=optimNLOPTR,
#                                                     directOptControl = list(funEvals=10)))

## ---- eval = FALSE------------------------------------------------------------
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
#  matplot(t(progOptim), type="l", col="red", lty=1,
#          xlab="n: blackbox evaluations", ylab="best objective value")
#  matlines(t(progSpot), type="l", col="gray", lty=1)
#  legend("topright", c("Spot", "optim"), col=c("gray", "red"), lty=1, bty="n")

## ---- eval = FALSE------------------------------------------------------------
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
#  
#  

## ---- eval = FALSE------------------------------------------------------------
#  library("laGP")
#  library("MASS")
#  library("lhs")
#  library("akima")
#  library("tgp")
#  library("SPOT")

## ---- eval = FALSE------------------------------------------------------------
#  N <- 200
#  x <- matrix( seq(from=-1, to = 1, length.out = N), ncol = 1)
#  y <- funSphere(x)  + rnorm(N, 0, 0.1)
#  ###################################################################################################
#  #' fit <- buildGaussianProcess(x,y)
#  #' ## Print model parameters
#  #' print(fit)
#  #' ## Predict at new location
#  #' xNew <- matrix( c(-0.1, 0.1), ncol = 1)
#  #' predict(fit, xNew)
#  #' ## True value at location
#  #' t(funSphere(xNew))
#  ###################################################################################################
#  d <- g <- NULL
#  con<-list(samp.size = 100,
#            modelControl = list(modelInitialized = FALSE))
#    con[names(control)] <- control
#    control<-con
#  	
#    ## Case 1: model is not initialized:
#    if (control$modelControl$modelInitialized == FALSE){
#    control$x <- x
#  	control$y <- y
#  	d <- laGP::darg(NULL, x, samp.size = control$samp.size)
#  	g <- laGP::garg(list(mle = TRUE), y)
#  	fit <- laGP::newGPsep(x, y, d = d$start,
#  	                 g = g$start, dK = TRUE)
#  	laGP::jmleGPsep(fit,
#  	              drange = c(d$min, d$max),
#  	              grange = c(g$min, g$max),
#  	              dab = d$ab,
#  	              gab = g$ab)
#  	control$fit <- fit
#  	control$d <- d
#  	control$g <- g
#  	control$pNames <- colnames(x)
#  	control$yName <- "y"
#  	class(control) <- "spotGaussianProcessModel"
#    }
#  
#    control$modelControl$modelInitialized <- TRUE
#  
#    xNew <- matrix( c(-0.1, 0.1), ncol = 1)
#  
#    control$xNewActualSize <- nrow(xNew)
#    x <- rbind(x, xNew)
#    y <- funSphere(x)  + rnorm(N+control$xNewActualSize, 0, 0.1)
#      ## Case 2: model is already initialized:
#        n <- nrow(x)
#        indices <- (n - control$xNewActualSize +1):n
#        xnew <- x[indices, , drop = FALSE]
#        ynew <- y[indices, ,drop = FALSE]
#        laGP::updateGPsep(control$fit, xnew, ynew)
#        laGP::jmleGPsep(control$fit,
#                        drange = c(control$d$min, control$d$max),
#                        grange = c(control$g$min, control$g$max),
#                        dab = control$d$ab,
#                        gab = control$g$ab)
#  ## prediction:
#  
#  xNew <- matrix( c(-0.1, 0.1), ncol = 1)
#  predGPsep(control$fit, XX = xNew, lite = TRUE)
#  
#  newdata <- as.data.frame(xNew)
#  predict(control$fit, xNew)
#  
#  predict.spotGaussianProcessModel<-function(object, newdata, ...){
#  	newdata <- as.data.frame(newdata)
#    if(!all(colnames(newdata) %in% object$pNames))
#      colnames(newdata) <- object$pNames
#  
#    predGPsep(object, XX = newdata, lite = TRUE)
#  
#    # seqVec <- Vectorize(seq.default, vectorize.args = c("from", "to"))
#    # XX <- matrix( seqVec(from = xmin, to = xmax, length.out = n), ncol = dim(x)[2])
#    # res <- laGP::aGP(object$x,
#    #            object$y,
#    #            newdata,
#    #            end = 10,
#    #            d = object$d,
#    #            g = object$g,
#    #            verb = 0)
#    # plot(df$y ~ df[,2] , cex = 0.5, main = "branin data")
#    # lines(XX[,1], motogp.p$mean, lwd = 2)
#    # q1 <- qnorm(0.05, mean = motogp.p$mean, sd = sqrt(motogp.p$s2))
#    # q2 <- qnorm(0.95, mean = motogp.p$mean, sd = sqrt(motogp.p$s2))
#    # lines(XX[,1], q1, lty = 2, lwd = 2)
#    # lines(XX[,1], q2, lty = 2, lwd = 2)
#    # lines(XX[,1], motoagp$mean, col = 2, lwd = 2)
#    # q1 <- qnorm(0.05, mean = motoagp$mean, sd = sqrt(motoagp$var))
#    # q2 <- qnorm(0.95, mean = motoagp$mean, sd = sqrt(motoagp$var))
#    # lines(XX[,1], q1, lty = 2, col = 2, lwd = 2)
#    # lines(XX[,1], q2, lty = 2, col = 2, lwd = 2)
#    list(y = res$mean)
#  }
#  
#  
#  

## ---- eval = FALSE------------------------------------------------------------
#  X <- matrix(seq(0, 2 * pi,length = 6), ncol = 1)
#  str(X)

## ---- eval = FALSE------------------------------------------------------------
#  Z <- sin(X)
#  gp <- newGP(X, Z, 2, 1e-6, dK = TRUE)
#  str(gp)

## ---- eval = FALSE------------------------------------------------------------
#  mleGP(gp, tmax=20)

## ---- eval = FALSE------------------------------------------------------------
#  XX <- matrix(seq(-1, 2 * pi + 1, length = 499), ncol = ncol(X))
#  str(XX)

## ---- eval = FALSE------------------------------------------------------------
#  p <- predGP(gp, XX)
#  str(p)

## ---- eval = FALSE------------------------------------------------------------
#  library("mvtnorm")
#  N <- 100
#  ZZ <- rmvt(N, p$Sigma, p$df)
#  ZZ <- ZZ + t(matrix(rep(p$mean, N), ncol = N))
#  str(ZZ)

## ---- eval = FALSE------------------------------------------------------------
#  {matplot(XX, t(ZZ), col = "gray", lwd = 0.5, lty = 1, type = "l",
#  	bty = "n", main = "simple sinusoidal example", xlab = "x",
#  	ylab = "Y(x) | thetahat")
#  points(X, Z, pch = 19)}

## ---- eval = FALSE------------------------------------------------------------
#  x <- seq(-2, 2, by = 0.02)
#  str(x)
#  X <- as.matrix(expand.grid(x, x))
#  str(X)

## ---- eval = FALSE------------------------------------------------------------
#  N <- nrow(X)
#  f2d <- function(x)
#    {
#      g <- function(z)
#        return(exp( - (z - 1)^2) + exp( -0.8 * (z + 1)^2)
#          - 0.05 * sin(8 * (z + 0.1)))
#      -g(x[,1]) * g(x[,2])
#    }
#  Y <- f2d(X)
#  str(Y)

## ---- eval = FALSE------------------------------------------------------------
#  Xref <- matrix(c(-1.725, 1.725), nrow = 1)
#  p.mspe <- laGP(Xref, 6, 50, X, Y, d = 0.1, method="mspe")
#  str(p.mspe)

## ---- eval = FALSE------------------------------------------------------------
#  p.alc <- laGP(Xref, 6, 50, X, Y, d = 0.1, method="alc")
#  str(p.alc)

## ---- eval = FALSE------------------------------------------------------------
#  Xi <- rbind(X[p.mspe$Xi, ], X[p.alc$Xi, ])
#  {
#  plot(X[p.mspe$Xi, ], xlab = "x1", ylab = "x2", type = "n",
#    main = "comparing local designs", xlim = range(Xi[ ,1]),
#    ylim = range(Xi[ ,2]))
#  text(X[p.mspe$Xi, ], labels = 1:length(p.mspe$Xi), cex = 0.7)
#  text(X[p.alc$Xi, ], labels = 1:length(p.alc$Xi), cex = 0.7, col = 2)
#  points(Xref[1], Xref[2], pch=19, col=3)
#  legend("topright", c("mspe", "alc"), text.col = c(1, 2), bty="n")
#  }

## ---- eval = FALSE------------------------------------------------------------
#  p <- rbind(c(p.mspe$mean, p.mspe$s2, p.mspe$df),
#    c(p.alc$mean, p.alc$s2, p.alc$df))
#  colnames(p) <- c("mean", "s2", "df")
#  rownames(p) <- c("mspe", "alc")
#  p

## ---- eval = FALSE------------------------------------------------------------
#  p.mspe$mle
#  p.alc$mle

## ---- eval = FALSE------------------------------------------------------------
#  c(p.mspe$time, p.alc$time)

## ---- eval = FALSE------------------------------------------------------------
#  xx <- seq(-1.97, 1.95, by = 0.04)
#  str(xx)

## ---- eval = FALSE------------------------------------------------------------
#  XX <- as.matrix(expand.grid(xx, xx))
#  str(XX)

## ---- eval = FALSE------------------------------------------------------------
#  YY <- f2d(XX)
#  str(YY)

## ---- eval = FALSE------------------------------------------------------------
#  persp(xx, xx, -matrix(P.alc$mean, ncol = length(xx)), phi=45, theta=45,
#        main = "", xlab = "x1", ylab = "x2", zlab = "yhat(x)")

## ---- eval = FALSE------------------------------------------------------------
#  med <- 0.51
#  zs <- XX[, 2] == med
#  sv <- sqrt(P.alc$var[zs])
#  r <- range(c(-P.alc$mean[zs] + 2 * sv, -P.alc$mean[zs] - 2 * sv))
#  plot(XX[zs,1], -P.alc$mean[zs], type="l", lwd = 2, ylim = r, xlab = "x1",
#       ylab = "predicted & true response", bty = "n",
#       main = "slice through surface")
#  lines(XX[zs, 1], -P.alc$mean[zs] + 2 * sv, col = 2, lty = 2, lwd = 2)
#  lines(XX[zs, 1], -P.alc$mean[zs] - 2 * sv, col = 2, lty = 2, lwd = 2)
#  lines(XX[zs, 1], YY[zs], col = 3, lwd = 2, lty = 3)

## ---- eval = FALSE------------------------------------------------------------
#  diff <- P.alc$mean - YY
#  plot(XX[zs,1], diff[zs], type = "l", lwd = 2,
#       main = "systematic bias in prediction",
#       xlab = "x1", ylab = "y(x) - yhat(x)", bty = "n")

## ---- eval = FALSE------------------------------------------------------------
#  plot(XX[zs,1], P.alc$mle$d[zs], type = "l", lwd=2,
#       main = "spatially varying lengthscale",
#       xlab = "x1", ylab = "thetahat(x)", bty = "n")

## ---- eval = FALSE------------------------------------------------------------
#  df <- data.frame(y = log(P.alc$mle$d), XX)
#  lo <- loess(y ~ ., data = df, span = 0.01)
#  lines(XX[zs,1], exp(lo$fitted)[zs], col=2, lty=2, lwd=2)
#  legend("topright", "loess smoothed", col=2, lty=2, lwd=2, bty="n")

## ---- eval = FALSE------------------------------------------------------------
#  rmse <- data.frame(alc = sqrt(mean((P.alc$mean - YY)^2)),
#    alc2 = sqrt(mean((P.alc2$mean - YY)^2)))
#  rmse

## ---- eval = FALSE------------------------------------------------------------
#  p.alcray <- laGP(Xref, 6, 50, X, Y, d = 0.1, method = "alcray")

## ---- eval = FALSE------------------------------------------------------------
#  plot(X[p.alc$Xi,], xlab = "x1", ylab = "x2", type = "n",
#    main="comparing local designs", xlim = range(Xi[ ,1]),
#    ylim = range(Xi[ ,2]))
#  text(X[p.alc$Xi,], labels = 1:length(p.alc$Xi), cex = 0.7, col = 2)
#  text(X[p.alcray$Xi,], labels=1:length(p.mspe$Xi), cex=0.7, col = 3)
#  points(Xref[1], Xref[2], pch = 19, col = 3)
#  legend("topright", c("alc", "alcray"), text.col = c(2,3), bty = "n")

## ---- eval = FALSE------------------------------------------------------------
#  p.alcray$time

## ---- eval = FALSE------------------------------------------------------------
#  p <- rbind(p, c(p.alcray$mean, p.alcray$s2, p.alcray$df))
#  rownames(p)[3] <- c("alcray")
#  p

## ---- eval = FALSE------------------------------------------------------------
#  c(P.alcray$time, P.alcray2$time)

## ---- eval = FALSE------------------------------------------------------------
#  rmse <- cbind(rmse,
#    data.frame(alcray=sqrt(mean((P.alcray$mean - YY)^2)),
#      alcray2=sqrt(mean((P.alcray2$mean - YY)^2))))
#  rmse

## ---- eval = FALSE------------------------------------------------------------
#  N <- 100000
#  Npred <- 1000
#  dim <- 8
#  library("lhs")

## ---- eval = FALSE------------------------------------------------------------
#  T <- 10
#  nas <- rep(NA, T)
#  times <- rmse <- data.frame(mspe = nas, mspe2 = nas,
#    alc.nomle = nas, alc = nas, alc2 = nas,
#    nn.nomle = nas, nn=nas, big.nn.nomle = nas, big.nn = nas,
#    big.alcray = nas, big.alcray2 = nas)

## ---- eval = FALSE------------------------------------------------------------
#  borehole <- function(x){
#    rw <- x[1] * (0.15 - 0.05) + 0.05
#    r <-  x[2] * (50000 - 100) + 100
#    Tu <- x[3] * (115600 - 63070) + 63070
#    Hu <- x[4] * (1110 - 990) + 990
#    Tl <- x[5] * (116 - 63.1) + 63.1
#    Hl <- x[6] * (820 - 700) + 700
#    L <-  x[7] * (1680 - 1120) + 1120
#    Kw <- x[8] * (12045 - 9855) + 9855
#    m1 <- 2 * pi * Tu * (Hu - Hl)
#    m2 <- log(r / rw)
#    m3 <- 1 + 2 * L * Tu / (m2 * rw^2 * Kw) + Tu / Tl
#    return(m1/m2/m3)
#  }

## ---- eval = FALSE------------------------------------------------------------
#  timev <- apply(times, 2, mean, na.rm = TRUE)
#  rmsev <- apply(rmse, 2, mean)
#  tab <- cbind(timev, rmsev)
#  o <- order(rmsev, decreasing = FALSE)
#  tt <- rep(NA, length(rmsev))
#  for(i in 1:(length(o)-1)) {
#    tto <- t.test(rmse[ ,o[i]], rmse[ ,o[i+1]], alternative = "less",
#      paired = TRUE)
#    tt[o[i]] <- tto$p.value
#  }
#  tab <- cbind(tab, data.frame(tt))
#  tab[o, ]

## ---- eval = FALSE------------------------------------------------------------
#  thats <- matrix(NA, nrow = T, ncol = dim)
#  its <- rep(NA, T)
#  n <- 1000
#  
#  g2 <- garg(list(mle = TRUE), y)
#  d2 <- darg(list(mle = TRUE, max = 100), x)
#  
#  for(t in 1:T) {
#  
#    subs <- sample(1:N, n, replace = FALSE)
#  
#    gpsepi <- newGPsep(x[subs, ], y[subs], rep(d2$start, dim), g = 1/1000,
#      dK = TRUE)
#    that <- mleGPsep(gpsepi, param = "d", tmin = d2$min, tmax = d2$max,
#      ab = d2$ab, maxit = 200)
#    thats[t,] <- that$d
#    its[t] <- that$its
#  
#    deleteGPsep(gpsepi)
#  }

## ---- eval = FALSE------------------------------------------------------------
#  boxplot(thats, main = "distribution of thetas", xlab = "input",
#    ylab = "theta")

## ---- eval = FALSE------------------------------------------------------------
#  scales <- sqrt(apply(thats, 2, median))
#  xs <- x; xpreds <- xpred
#  for(j in 1:ncol(xs)) {
#    xs[,j] <- xs[,j] / scales[j]
#    xpreds[,j] <- xpreds[,j] / scales[j]
#  }

## ---- eval = FALSE------------------------------------------------------------
#  out14 <- aGP(xs, y, xpreds, d=list(start=1, max=20), method="alcray")

## ---- eval = FALSE------------------------------------------------------------
#  sqrt(mean((out14$mean - ypred.0)^2))

## ---- eval = FALSE------------------------------------------------------------
#  plot(X, Z, main = "simulating a larger data setup", xlab = "times",
#    ylab = "accel")
#  lines(XX, motoagp2$mean, col = 2, lwd = 2)
#  q1 <- qnorm(0.05, mean = motoagp2$mean, sd = sqrt(motoagp2$var))
#  q2 <- qnorm(0.95, mean = motoagp2$mean, sd = sqrt(motoagp2$var))
#  lines(XX, q1, col = 2, lty = 2, lwd = 2)
#  lines(XX, q2, col = 2, lty = 2, lwd = 2)

## ---- eval = FALSE------------------------------------------------------------
#  M <- function(x,u)
#    {
#      x <- as.matrix(x)
#      u <- as.matrix(u)
#      out <- (1 - exp(-1 / (2 * x[,2])))
#      out <- out * (1000 * u[,1] * x[,1]^3 + 1900 * x[ ,1]^2
#        + 2092 * x[ ,1] + 60)
#      out <- out / (100 * u[,2] * x[,1]^3 + 500 * x[ ,1]^2 + 4 * x[ ,1] + 20)
#      return(out)
#    }

## ---- eval = FALSE------------------------------------------------------------
#  bias <- function(x)
#    {
#      x <- as.matrix(x)
#      out <- 2 * (10 * x[ ,1]^2 + 4 * x[ ,2]^2) / (50 * x[ ,1] * x[ ,2] + 10)
#      return(out)
#    }

## ---- eval = FALSE------------------------------------------------------------
#  bias.est <- TRUE
#  methods <- rep("alc", 2)
#  da <- d <- darg(NULL, XU)
#  g <- garg(list(mle = TRUE), Y)

## ---- eval = FALSE------------------------------------------------------------
#  beta.prior <- function(u, a = 2, b = 2, log = TRUE)
#  {
#    if(length(a) == 1) a <- rep(a, length(u))
#    else if(length(a) != length(u)) stop("length(a) must be 1 or length(u)")
#    if(length(b) == 1) b <- rep(b, length(u))
#    else if(length(b) != length(u)) stop("length(b) must be 1 or length(u)")
#    if(log) return(sum(dbeta(u, a, b, log=TRUE)))
#    else return(prod(dbeta(u, a, b, log=FALSE)))
#  }

## ---- eval = FALSE------------------------------------------------------------
#  library("crs")
#  opts <- list("MAX_BB_EVAL" = 1000, "INITIAL_MESH_SIZE" = imesh,
#    "MIN_POLL_SIZE" = "r0.001", "DISPLAY_DEGREE" = 0)

## ---- eval = FALSE------------------------------------------------------------
#  Xu <- cbind(X, matrix(rep(u, ny), ncol = 2, byrow = TRUE))
#  Mhat.u <- aGP.seq(XU, Z, Xu, da, methods, ncalib = 2, omp.threads = nth,
#    verb = 0)
#  cmle.u <- discrep.est(X, Y, Mhat.u$mean, d, g, bias.est, FALSE)
#  cmle.u$ll <- cmle.u$ll + beta.prior(u)

## ---- eval = FALSE------------------------------------------------------------
#  data.frame(u.hat = -outi$objective, u = cmle.u$ll)

## ---- eval = FALSE------------------------------------------------------------
#  print(c(cmle$ll, -outi$objective))

## ---- eval = FALSE------------------------------------------------------------
#  data.frame(u.hat = rmse, u = rmse.u)

