## ----knitrSetup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----SPOT, include = FALSE, eval = FALSE--------------------------------------
#  ## install.packages("devtools")
#  ## devtools::install_github("r-lib/devtools")
#  url <- "http://owos.gm.fh-koeln.de:8055/bartz/spot.git"
#  devtools::install_git(url = url)

## ---- setup-------------------------------------------------------------------
library("SPOT")
packageVersion("SPOT")

## ---- include=FALSE-----------------------------------------------------------
set.seed(1)

## ---- spotSimple--------------------------------------------------------------
res <- spot(,funSphere,
            c(-1,-1),
            c(1,1))
cbind(res$xbest, res$ybest)

## ---- spotY-------------------------------------------------------------------
set.seed(1)
res <- spot(
  x = matrix(c(0.05), 1, 1),
  fun = funSphere,
  lower = -1,
  upper = 1,
  control = list(
    funEvals = 20,
    model = buildKriging,
    optimizer = optimDE,
    modelControl = list(target = "y")
  )
)
cbind(res$xbest, res$ybest)

## ---- spotEI------------------------------------------------------------------
set.seed(1)
res <- spot(
  x = matrix(c(0.05), 1, 1),
  fun = funSphere,
  lower = -1,
  upper = 1,
  control = list(
    funEvals = 20,
    model = buildKriging,
    optimizer = optimDE,
    modelControl = list(target = "ei")
  )
)
cbind(res$xbest, res$ybest)

## ---- plotEI------------------------------------------------------------------
plot(res$ybestVec, log = "y", type="b")

## ---- runEIMulti--------------------------------------------------------------
res <- spot(
   x = matrix(c(0.05), 1, 1),
  fun = funSphere,
  lower = -1,
  upper = 1,
  control = list(
    funEvals = 20,
    multiStart = 3,
    model = buildKriging,
    optimizer = optimDE,
    modelControl = list(target = "y"),
    designControl = list(size=10)
  )
)
cbind(res$xbest, res$ybest)

## ---- plotEIMulti-------------------------------------------------------------
plot(res$ybestVec, log = "y", type="b")

## ---- runBOEIMulti, eval=FALSE------------------------------------------------
#  res <- spot(
#    x = NULL,
#    fun = funSphere,
#    lower = -1,
#    upper = 1,
#    control = list(
#      funEvals = 20,
#      multiStart = 2,
#      model = buildBO,
#      optimizer = optimDE,
#      modelControl = list(target = "y"),
#      designControl = list(size=10)
#    )
#  )
#  cbind(res$xbest, res$ybest)

## ---- plotBOEIMulti-----------------------------------------------------------
par(mfrow=c(1,2))
plot(res$ybestVec, log = "y", type="b")
plot(res$ySurr[!is.na(res$ySurr)], type="b")
par(mfrow=c(1,1))

## -----------------------------------------------------------------------------


## ---- replicateBest-----------------------------------------------------------
lower <-res$xbest
resBestReplicated <- spot(x=NULL,
  fun = funSphere,
  lower = lower,
  upper = lower,
  control = list(
    replicateResult = TRUE,
    funEvals = 10
  )
)
cbind(resBestReplicated$x,
      resBestReplicated$y)

## ---- spotStart3--------------------------------------------------------------
res <- spot(,
            fun = funSphere,
            lower = c(-1,-1),
            upper = c(1,1),
            control = list( #funEvals=4,
                           #verbosity=0
                           #,designControl = list(size = 3)
            )
            )
cbind(res$x, res$y)

## ---- spotStart---------------------------------------------------------------
res <- spot(x = matrix(c(0.05,0.1,
                         -0.1, 0.01,
                         0.01, 0.02),3,2, byrow = TRUE),
            fun = funSphere,
            lower = c(-1,-1),
            upper = c(1,1),
             control = list(funEvals=5,
            designControl = list(size = 1)
            )
            )
cbind(res$x, res$y)

## ---- SANN--------------------------------------------------------------------
fs <- function(x){
  x[1]^2 + x[2]^2}
res <- optim(par=c(0.05, 0,1), 
             fn=fs, 
             method = "SANN",
             control = list(maxit = 20))
cbind(t(res$par),res$value)

## ---- spotLocal---------------------------------------------------------------
res <- spot(x = matrix(c(0.05,0.1),1,2),
            fun = funSphere,
            lower = c(-2,-3),
            upper = c(1,2),
            control=list(funEvals=20,
                         optimizer=optimLBFGSB,
                          modelControl=list(target="ei")
                         )
            )
cbind(res$xbest, res$ybest)

## ---- spotLocalMulti----------------------------------------------------------
res <- spot(x = matrix(c(0.05,0.1),1,2),
            fun = funSphere,
            lower = c(-2,-3),
            upper = c(1,2),
            control=list(funEvals=20,
                         optimizer=optimLBFGSB,
                          modelControl=list(target="ei"),
                         multiStart = 2
                         )
            )
cbind(res$xbest, res$ybest)

## ---- spotRf------------------------------------------------------------------
res <- spot(,
            funSphere,
            c(-1,-1),
            c(1,1),
            control=list(model=buildRandomForest))
cbind(res$xbest, res$ybest)

## ---- spotLM------------------------------------------------------------------
res <- spot(,
            funSphere,
            c(-2,-3),
            c(1,2),
     control=list(model=buildLM)) #lm as surrogate
cbind(res$xbest, res$ybest)

## ---- spotBO------------------------------------------------------------------
res <- spot(
  x = matrix(c(0.05, 0.1), 1, 2),
  fun = funSphere,
  lower = c(-1, -1),
  upper = c(1, 1),
  control = list(
    funEvals = 20,
    model = buildBO,
    optimizer = optimLBFGSB,
    modelControl = list(target = "ei")
  )
)
cbind(res$xbest, res$ybest)

## ---- spotLMOPT---------------------------------------------------------------
res <- spot(,funSphere,c(-2,-3),c(1,2),
   control=list(model=buildLM, optimizer=optimLBFGSB))
res$xbest

## ---- spotLasso---------------------------------------------------------------

res <- spot(,funSphere,
            lower = c(-2,-3),
            upper = c(1,2),
             control = list(funEvals=50,
                            model=buildLasso, 
                optimizer = optimNLOPTR,
            designControl = list(size = 20)
            ))
res$xbest

## ---- spotLBFGSB--------------------------------------------------------------
res <- spot(,funSphere,c(-2,-3),c(1,2), 
   control=list(model=buildKriging, optimizer = optimLBFGSB))
cbind(res$xbest, res$ybest)

## ---- spotNLOPTR--------------------------------------------------------------
res <- spot(,funSphere,c(-2,-3),c(1,2), 
     control=list(model=buildKriging, optimizer = optimNLOPTR))
cbind(res$xbest, res$ybest)

## ---- spotKrigDace------------------------------------------------------------
res <- spot(,funSphere,c(-2,-3),c(1,2),
 control=list(model=buildKrigingDACE, optimizer=optimLBFGSB))
res$xbest

## ---- transformX--------------------------------------------------------------
# Use transformed input values
set.seed(1)
f2 <- function(x){2^x}
lower <- c(-100, -100)
upper <- c(10, 10)
transformFun <- rep("f2", length(lower))
res <- spot(x=NULL,
            fun=funSphere,
            lower=lower, 
            upper=upper,
             control=list(funEvals=20,
                          modelControl=list(target="ei"),
                          optimizer=optimLBFGSB,
                          transformFun=transformFun,
                          verbosity = 0,
                          progress = FALSE,
                          plots = FALSE))
print(cbind(res$x, res$xt, res$y))
print(which.min(res$y[, 1, drop = FALSE]))
cbind(res$xbest, res$ybest)

## ---- spotNoise---------------------------------------------------------------
# noisy objective
fNoise = function(x){funSphere(x) + rnorm(nrow(x))}
res1 <-
  spot(x = NULL, 
       fun = fNoise, 
       lower = c(-2, -3), 
       upper = c(1, 2),
    control = list(funEvals = 40, noise = TRUE, verbosity=0))
# noise with replicated evaluations
res2 <-
  spot(x = NULL, 
       fun = fNoise, 
       lower = c(-2, -3), 
       upper = c(1, 2),
    control = list(
      verbosity=0,
      funEvals = 40,
      noise = TRUE,
      replicates = 2,
      designControl = list(replicates = 2)
    ))
# and with OCBA
res3 <- spot(x = NULL, 
       fun = fNoise, 
       lower = c(-2, -3), 
       upper = c(1, 2),
  control = list(
    verbosity=0,
    funEvals = 40,
    noise = TRUE,
    replicates = 2,
    OCBA = TRUE,
    OCBABudget = 5,
    designControl = list(replicates = 2)
  )
)
# Check results with non-noisy function:
funSphere(res1$xbest)
funSphere(res2$xbest)
funSphere(res3$xbest)

## ---- replicateBestNoise------------------------------------------------------
lower <-res3$xbest
resBestReplicated <- spot(
  ,
  fun = fNoise,
  lower = lower,
  upper = lower,
  control = list(
    funEvals = 10,
    replicateResults = TRUE
   )
)
cbind(resBestReplicated$x,
      resBestReplicated$y)

## ---- randomNumberSeed1a------------------------------------------------------
res1a <- spot(,function(x,seed){set.seed(seed);funSphere(x)+rnorm(nrow(x))},
     c(-2,-3),c(1,2),control=list(funEvals=25,noise=TRUE,seedFun=1))

## ---- randomNumberSeed1b------------------------------------------------------
res1b <- spot(,function(x,seed){set.seed(seed);funSphere(x)+rnorm(nrow(x))},
     c(-2,-3),c(1,2),control=list(funEvals=25,noise=TRUE,seedFun=1))

## ---- randomNumberSeed2-------------------------------------------------------
res2 <- spot(,function(x,seed){set.seed(seed);funSphere(x)+rnorm(nrow(x))},
     c(-2,-3),c(1,2),control=list(funEvals=25,noise=TRUE,seedFun=2))

## ---- printRes1a1b2-----------------------------------------------------------
sprintf("Should be equal: %f = %f. Should be different:  %f", res1a$ybest, res1b$ybest, res2$ybest)

## -----------------------------------------------------------------------------
braninFunctionFactor <- function (x) {
   y <- (x[2]  - 5.1/(4 * pi^2) * (x[1] ^2) + 5/pi * x[1]  - 6)^2 +
     10 * (1 - 1/(8 * pi)) * cos(x[1] ) + 10
   if(x[3]==1)
     y <- y +1
   else if(x[3]==2)
     y <- y -1
   return(y)
}

## -----------------------------------------------------------------------------
objFun <- function(x){apply(x,1,braninFunctionFactor)}

## ---- spotFac-----------------------------------------------------------------
set.seed(1)
res <- spot(fun=objFun,lower=c(-5,0,1),upper=c(10,15,3),
            control=list(model=buildKriging,
                         types= c("numeric","numeric","factor"),
                         optimizer=optimLHD))
 res$xbest
 res$ybest

## ---- defineBounds------------------------------------------------------------
n <- 10
a <- rep(0,n)
b <- rep(1,n)

## ---- highDimKrig-------------------------------------------------------------
tic <- proc.time()[3]
res0 <- spot(x=NULL, funSphere, lower = a, upper = b, 
             control=list(funEvals=30))
toc <- proc.time()[3]
sprintf("value: %f, time: %f",  res0$ybest, toc-tic)

## ---- highDimGP---------------------------------------------------------------
tic <- proc.time()[3]
res1 <-  spot(x=NULL, funSphere, lower = a, upper = b, 
             control=list(funEvals=30, 
                          model = buildGaussianProcess))
toc <- proc.time()[3]
sprintf("value: %f, time: %f",  res1$ybest, toc-tic)

## -----------------------------------------------------------------------------
## run spot without log
res <- spot(fun = funSphere,
            lower=c(0,0),
            upper=c(100,100)
)
## run spot with log (3-dim "y" output: first is y, last are x val (here 2-dim))
funSphereLog <- function(x){
  cbind(funSphere(x),x)
}
res2 <- spot(fun = funSphereLog,
            lower=c(0,0),
            upper=c(100,100)
)
res$logInfo
res2$logInfo
## re-evaluation of the x-values and comparison with evaluated x-values:
funSphere(res2$logInfo) == res$y

## ---- hybrid------------------------------------------------------------------
res <- spot(fun = funSphere, lower = c(-5,-5),
                upper = c(5,5), 
                control = list(funEvals = 20,
                directOpt = optimNLOPTR,
                directOptControl = list(funEvals = 10)
                ))
str(res)

## ---- constraintsSetup, eval = FALSE------------------------------------------
#  library(babsim.hospital)
#  n <- 29
#  reps <- 2
#  funEvals <- 3*n
#  size <- 2*n
#  x0 <- matrix(as.numeric(babsim.hospital::getParaSet(5374)[1,-1]),1,)
#  bounds <- getBounds()
#  a <- bounds$lower
#  b <- bounds$upper
#  g <- function(x) {
#        return(rbind(a[1] - x[1], x[1] - b[1], a[2] - x[2], x[2] - b[2],
#                     a[3] - x[3], x[3] - b[3], a[4] - x[4], x[4] - b[4],
#                     a[5] - x[5], x[5] - b[5], a[6] - x[6], x[6] - b[6],
#                     a[7] - x[7], x[7] - b[7], a[8] - x[8], x[8] - b[8],
#                     a[9] - x[9], x[9] - b[9], a[10] - x[10], x[10] - b[10],
#                     a[11] - x[11], x[11] - b[11], a[12] - x[12],  x[12] - b[12],
#                     a[13] - x[13], x[13] - b[13], a[14] - x[14],  x[14] - b[14],
#                     a[15] - x[15], x[15] - b[15], a[16] - x[16],  x[16] - b[16],
#                     a[17] - x[17], x[17] - b[17], a[18] - x[18],  x[18] - b[18],
#                     a[19] - x[19], x[19] - b[19], a[20] - x[20],  x[20] - b[20],
#                     a[21] - x[21], x[21] - b[21], a[22] - x[22],  x[22] - b[22],
#                     a[23] - x[23], x[23] - b[23], a[24] - x[24],  x[24] - b[24],
#                     a[25] - x[25], x[25] - b[25], a[26] - x[26],  x[26] - b[26],
#                     a[27] - x[27], x[27] - b[27], x[15] + x[16] - 1,
#                     x[17] + x[18] + x[19] - 1, x[20] + x[21] - 1, x[23] + x[29] - 1)
#        )
#    }

## ---- constraintsRun, eval = FALSE--------------------------------------------
#  res <- spot(
#    x = x0,
#    fun = funBaBSimHospital,
#    lower = a,
#    upper = b,
#    verbosity = 0,
#    control = list(
#      funEvals = 2 * funEvals,
#      noise = TRUE,
#      designControl = list(# inequalityConstraint = g,
#        size = size,
#        retries = 1000),
#      optimizer = optimNLOPTR,
#      optimizerControl = list(
#        opts = list(algorithm = "NLOPT_GN_ISRES"),
#        eval_g_ineq = g
#      ),
#      model =  buildKriging,
#      plots = FALSE,
#      progress = TRUE,
#      directOpt = optimNLOPTR,
#      directOptControl = list(funEvals = 0),
#      eval_g_ineq = g
#    )
#  )
#  print(res)

## ---- eval=FALSE--------------------------------------------------------------
#  # docker run --rm mrebolle/r-geccoc:Track1 -c 'Rscript objfun.R "6,7,3,3,3,5,3,3,25,17,2,1,0.25,0.05,0.07,0.005,0.07,1e-04,0.08,0.25,0.08,0.5,1e-06,2,1e-06,1e-06,1,2,0.5"'

## ---- eval =FALSE-------------------------------------------------------------
#  library(SPOT)
#  
#  evalFun <- function(candidateSolution){
#      evalCommand <- paste0("docker run --rm mrebolle/r-geccoc:Track1 -c ", "'","Rscript objfun.R ")
#      parsedCandidate <- paste(candidateSolution, sep=",", collapse = ",")
#      return(as.numeric(system(paste0(evalCommand, '"', parsedCandidate, '"', "'"), intern = TRUE)))
#  }
#  
#  #The BabSim.Hospital requires 29 parameters. Here we specify the upper and lower bounds
#  lower <- c(6,7,3,3,3,5,3,3,25,17,2,1,0.25,0.05,0.07,
#             0.005,0.07,1e-04,0.08,0.25,0.08,0.5,1e-06,
#             2,1e-06,1e-06,1,2,0.5)
#  
#  upper<- c(14,13,7,9,7,9,5,7,35,25,5,7,2,0.15,0.11,0.02,
#            0.13,0.002,0.12,0.35,0.12,0.9,0.01,4,1.1,0.0625,
#            2,5,0.75)
#  
#  wFun <- wrapFunction(evalFun)
#  
#  n <- 29
#  reps <- 2
#  funEvals <- 10*n
#  size <- 2*n
#  x0<-matrix(lower,nrow = 1)
#  
#  res <- spot(x = x0,
#    fun = wFun,
#    lower = lower,
#    upper = upper,
#    control = list(
#      funEvals = 2 * funEvals,
#      noise = TRUE,
#      designControl = list(
#        size = size,
#        retries = 1000),
#      optimizer = optimNLOPTR,
#      optimizerControl = list(
#        opts = list(algorithm = "NLOPT_GN_ISRES")
#      ),
#      model =  buildKriging,
#      plots = TRUE,
#      progress = TRUE,
#      directOpt = optimNLOPTR,
#      directOptControl = list(funEvals = 0)
#    )
#  )
#  

## ---- eval=FALSE--------------------------------------------------------------
#  git clone http://owos.gm.fh-koeln.de:8055/bartz/babsim.hospital.git

## ---- constraintsBabsimSetup, eval = FALSE------------------------------------
#  library(SPOT)
#  library(babsim.hospital)
#  
#  n <- 29
#  reps <- 2
#  funEvals <- 3*n
#  size <- 2*n
#  #Get suggested parameter values as initial point in the optimization run
#  x0 <- matrix(as.numeric(babsim.hospital::getParaSet(5374)[1,-1]),1,)
#  bounds <- getBounds()
#  a <- bounds$lower
#  b <- bounds$upper
#  g <- function(x) {
#        return(rbind(a[1] - x[1], x[1] - b[1], a[2] - x[2], x[2] - b[2],
#                     a[3] - x[3], x[3] - b[3], a[4] - x[4], x[4] - b[4],
#                     a[5] - x[5], x[5] - b[5], a[6] - x[6], x[6] - b[6],
#                     a[7] - x[7], x[7] - b[7], a[8] - x[8], x[8] - b[8],
#                     a[9] - x[9], x[9] - b[9], a[10] - x[10], x[10] - b[10],
#                     a[11] - x[11], x[11] - b[11], a[12] - x[12],  x[12] - b[12],
#                     a[13] - x[13], x[13] - b[13], a[14] - x[14],  x[14] - b[14],
#                     a[15] - x[15], x[15] - b[15], a[16] - x[16],  x[16] - b[16],
#                     a[17] - x[17], x[17] - b[17], a[18] - x[18],  x[18] - b[18],
#                     a[19] - x[19], x[19] - b[19], a[20] - x[20],  x[20] - b[20],
#                     a[21] - x[21], x[21] - b[21], a[22] - x[22],  x[22] - b[22],
#                     a[23] - x[23], x[23] - b[23], a[24] - x[24],  x[24] - b[24],
#                     a[25] - x[25], x[25] - b[25], a[26] - x[26],  x[26] - b[26],
#                     a[27] - x[27], x[27] - b[27], x[15] + x[16] - 1,
#                     x[17] + x[18] + x[19] - 1, x[20] + x[21] - 1, x[23] + x[29] - 1)
#        )
#    }

## ---- constraintsBabsimRun, eval = FALSE--------------------------------------
#  wrappedFunBab <- function(x){
#    print(SPOT::funBaBSimHospital(x, region = 5374, nCores = 1))
#  }
#  res <- spot(
#    x = x0,
#    fun = wrappedFunBab,
#    lower = a,
#    upper = b,
#    control = list(
#      funEvals = 2 * funEvals,
#      noise = TRUE,
#      designControl = list(
#        size = size,
#        retries = 1000),
#      optimizer = optimNLOPTR,
#      optimizerControl = list(
#        opts = list(algorithm = "NLOPT_GN_ISRES"),
#        eval_g_ineq = g
#      ),
#      model =  buildKriging,
#      plots = FALSE,
#      progress = TRUE,
#      directOpt = optimNLOPTR,
#      directOptControl = list(funEvals = 0),
#      eval_g_ineq = g
#    )
#  )
#  print(res)

## ---- runEIMultiGoldsteinPrice, eval=FALSE------------------------------------
#  res <- spot(
#    x = NULL,
#    fun = funGoldsteinPrice,
#    lower = rep(0,2),
#    upper = rep(1,2),
#    control = list(
#      funEvals = 20,
#      multiStart = 5,
#      model = buildKriging,
#      optimizer = optimDE,
#      modelControl = list(target = "ei"),
#      designControl = list(size=10)
#    )
#  )
#  cbind(res$xbest, res$ybest)

## ---- benchm1, eval=FALSE-----------------------------------------------------
#  ### Compare Performance
#  # rm(list=ls())
#  library(microbenchmark)
#  library(SPOT)
#  set.seed(1)
#  n <- 3
#  low = -2
#  up = 2
#  a = runif(n, low, 0)
#  b = runif(n, 0, up)
#  x0 = a + runif(n)*(b-a)
#  plot(a, type = "l", ylim=c(up,low))
#  lines(b)
#  lines(x0)
#  x0 = matrix( x0, nrow = 1)
#  
#  set.seed(1)
#    perf1 <- spot(x= x0, funSphere, a, b, control=list(time=list(maxTime = 0.25), funEvals=10*n, plots=FALSE,
#                                                       model = buildKriging, optimizer=optimNLOPTR))
#    set.seed(1)
#    perf2 <- spot(x= x0, funSphere, a, b, control=list(time=list(maxTime = 0.25), funEvals=10*n, plots=FALSE,
#                  model = buildGaussianProcess, optimizer=optimNLOPTR, directOptControl = list(funEvals=0)))
#  
#    set.seed(1)
#    perf3 <- spot(x= x0, funSphere, a, b, control=list(time=list(maxTime = 0.25), funEvals=10*n, plots=FALSE,
#                                                       model = buildGaussianProcess, optimizer=optimNLOPTR,
#                                                       directOptControl = list(funEvals=10)))

## ---- benchm2, eval=FALSE-----------------------------------------------------
#  ### Plot Repeats (Sphere Function)
#  #  rm(list=ls())
#  library(microbenchmark)
#  library(SPOT)
#  set.seed(1)
#  n <- 2
#  low = -2
#  up = 2
#  a = runif(n, low, 0)
#  b = runif(n, 0, up)
#  x0 = a + runif(n)*(b-a)
#   #plot(a, type = "l", ylim=c(up,low))
#  #  #lines(b)
#  #  #lines(x0)
#  x0 = matrix( x0, nrow = 1)
#  #
#  reps <- 10
#  end <- 10*n
#  ninit <- n
#  #
#    progSpot <- matrix(NA, nrow = reps, ncol = end)
#    for(r in 1:reps){
#      set.seed(r)
#      x0 <- a + runif(n)*(b-a)
#      x0 = matrix( x0, nrow = 1)
#      sol <- spot(x= x0, funGoldsteinPrice, a, b, control=list(funEvals=end,
#                                                       model = buildGaussianProcess,
#                                                       optimizer=optimNLOPTR,
#                                                       directOptControl = list(funEvals=0),
#                                                       designControl = list(size = ninit)))
#      progSpot[r, ] <- prepareBestObjectiveVal(sol$y, end)
#  
#  }
#  #
#  matplot(t(progSpot), type="l", col="gray", lty=1,
#            xlab="n: blackbox evaluations", ylab="best objective value", log="y")
#    abline(v=ninit, lty=2)
#    legend("topright", "seed LHS", lty=2, bty="n")

## ---- benchm4Setup, eval=FALSE------------------------------------------------
#  ### Plot Repeats (Sphere Function)
#  #  rm(list=ls())
#  library(SPOT)
#  #
#  set.seed(1)
#  n <- 2 #dim
#  repeats <- 30 # repeats
#  #
#  end <- 50 * n
#  ninit <- 5* n
#  # objective fun
#  fun <- funGoldsteinPrice
#  #
#  low = rep(-1, n)
#  up = rep(1,n)
#  x0 <- c()
#  for(i in 1:repeats){
#  x0 = rbind(x0, low + runif(n) * (up - low))
#  }

## ---- eval=FALSE--------------------------------------------------------------
#  f <- fun
#    fprime <- function(x) {
#      x <- matrix( x, 1)
#      ynew <- as.vector(f(x))
#      y <<- c(y, ynew)
#      return(ynew)
#  }
#  #
#  progOptim <- matrix(NA, nrow=nrow(x0), ncol=end)
#  for (r in 1:nrow(x0)) {
#      y <- c()
#      os <- optim(x0[r, ,drop=FALSE], fprime, lower=low, upper=up, method="L-BFGS-B")
#      progOptim[r,] <- prepareBestObjectiveVal(y, end)
#    }
#  #

## ---- benchm4BO, eval=FALSE---------------------------------------------------
#  progSpotBOEI <- matrix(NA, nrow = nrow(x0), ncol = end)
#  for (r in 1:nrow(x0)) {
#    set.seed(r)
#   sol <- spot(
#      x = x0[r, ,drop=FALSE],
#      fun=fun,
#      a,
#      b,
#      control = list(
#        funEvals = end,
#        multiStart = 2,
#        model = buildBO,
#        optimizer = optimDE,
#        modelControl = list(target = "negLog10ei"),
#        directOptControl = list(funEvals =
#                                  0),
#        designControl = list(size = ninit)
#      )
#    )
#    progSpotBOEI[r,] <- prepareBestObjectiveVal(sol$y, end)
#  }
#  #

## ---- benchm4BOY, eval=FALSE--------------------------------------------------
#  progSpotBOY <- matrix(NA, nrow = nrow(x0), ncol = end)
#  for (r in 1:nrow(x0)) {
#    set.seed(r)
#   sol <- spot(
#      x = x0[r, ,drop=FALSE],
#      fun=fun,
#      lower = low,
#      upper = up,
#      control = list(
#        funEvals = end,
#        multiStart = 2,
#        model = buildBO,
#        optimizer = optimDE,
#        modelControl = list(target = "y"),
#        directOptControl = list(funEvals =
#                                  0),
#        designControl = list(size = ninit)
#      )
#    )
#    progSpotBOY[r,] <- prepareBestObjectiveVal(sol$y, end)
#  }
#  #

## ---- benchm4BuildKrigingEi, eval=FALSE---------------------------------------
#  progSpotBuildKrigingEi <- matrix(NA, nrow = nrow(x0), ncol = end)
#  for (r in 1:nrow(x0)) {
#    set.seed(r)
#   sol <- spot(
#      x = x0[r, ,drop=FALSE],
#      fun=fun,
#     lower = low,
#      upper = up,
#      control = list(
#        funEvals = end,
#        multiStart = 2,
#        model = buildKriging,
#        optimizer = optimDE,
#        modelControl = list(target = "ei"),
#        directOptControl = list(funEvals =
#                                  0),
#        designControl = list(size = ninit)
#      )
#    )
#    progSpotBuildKrigingEi[r,] <- prepareBestObjectiveVal(sol$y, end)
#  }
#  #

## ---- benchm4BuildKrigingY, eval=FALSE----------------------------------------
#  progSpotBuildKrigingY <- matrix(NA, nrow = nrow(x0), ncol = end)
#  for (r in 1:nrow(x0)) {
#    set.seed(r)
#   sol <- spot(
#      x = x0[r, ,drop=FALSE],
#      fun=fun,
#     lower = low,
#      upper = up,
#      control = list(
#        funEvals = end,
#        multiStart = 2,
#        model = buildKriging,
#        optimizer = optimDE,
#        modelControl = list(target = "y"),
#        directOptControl = list(funEvals =
#                                  0),
#        designControl = list(size = ninit)
#      )
#    )
#    progSpotBuildKrigingY[r,] <- prepareBestObjectiveVal(sol$y, end)
#  }
#  #

## ---- eval=FALSE--------------------------------------------------------------
#  print("optim:")
#  summary(progOptim[,end])
#  print("SPOT BO EI:")
#  summary(progSpotBOEI[,end])
#  print("SPOT BO Y:")
#  summary(progSpotBOY[,end])
#  print("SPOT Kriging EI:")
#  summary(progSpotBuildKrigingEi[,end])
#  print("SPOT Kriging Y:")
#  summary(progSpotBuildKrigingY[,end])

## ---- benchm4Plot, eval=FALSE-------------------------------------------------
#  matplot(
#    colMeans(progOptim),
#    type = "l",
#    col = 1,
#    lty = 1,
#    xlab = "n: blackbox evaluations",
#    ylab = "avg best objective value",
#    ylim = c(-3,3)
#  )
#  abline(v = ninit, lty = 2)
#  lines(colMeans(progSpotBOEI, na.rm=TRUE), col = 2, lwd=2)
#  lines(colMeans(progSpotBOY, na.rm=TRUE), col = 3, lwd=2)
#  lines(colMeans(progSpotBuildKrigingEi, na.rm=TRUE), col = 4, lwd=2)
#  lines(colMeans(progSpotBuildKrigingY, na.rm=TRUE), col = 5, lwd=2)
#  legend("topright", c("optim", "BO EI", "BO Y", "Krig EI", "Krig Y", "initial design size"), col=c(1:5,1), lwd = c(rep(1,5), 1), lty = c(rep(1,5),2), bty="n")

## ---- compBox, eval=FALSE-----------------------------------------------------
#  boxplot(progOptim[,end],
#          progSpotBOEI[,end],
#          progSpotBOY[,end],
#          progSpotBuildKrigingEi[,end],
#          progSpotBuildKrigingY[,end],
#          names = c("optim", "BO EI", "BO Y", "Krig EI", "Krig Y"),
#          xlab="algorithm",
#          ylab = "best y value"
#          )

## ---- eval=FALSE--------------------------------------------------------------
#  resBench01 <- list(progOptim=progOptim, progSpotBOEI=progSpotBOEI, progSpotBOY=progSpotBOY, progSpotBuildKrigingEi=progSpotBuildKrigingEi, progSpotBuildKrigingY=progSpotBuildKrigingY)
#  usethis::use_data(resBench01)

## -----------------------------------------------------------------------------
dim = 2
lower = c(-2, -3)
upper = c(1, 2)

control <- spotControl(dimension = dim)
control$verbosity <- 0
control$designControl$size <- 10
control$funEvals <- 15
control$yImputation$handleNAsMethod <- handleNAsMean
res <- spot(x = NULL,
           fun = funError,
           lower = lower,
           upper = upper,
           control)

