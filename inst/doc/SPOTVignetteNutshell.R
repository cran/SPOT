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

## ---- spotSimple--------------------------------------------------------------
res <- spot(,funSphere,c(-2,-3),c(1,2),control=list(funEvals=15))
res$xbest

## ---- spotEI------------------------------------------------------------------
res <- spot(,funSphere,c(-2,-3),c(1,2),
    control=list(funEvals=15,modelControl=list(target="ei")))
res$xbest

## ---- spotStart---------------------------------------------------------------
res <- spot(matrix(c(0.05,0.1),1,2),funSphere,c(-2,-3),c(1,2))
res$xbest

## ---- spotLarger--------------------------------------------------------------
res <- spot(,funSphere,c(-2,-3),c(1,2),
    control=list(funEvals=50))
res$xbest

## ---- spotLocal---------------------------------------------------------------
res <- spot(,funSphere,c(-2,-3),c(1,2),
   control=list(optimizer=optimLBFGSB))
 res$xbest

## ---- spotRf------------------------------------------------------------------
res <- spot(,funSphere,c(-2,-3),c(1,2),
     control=list(model=buildRandomForest))
res$xbest

## ---- spotLM------------------------------------------------------------------
res <- spot(,funSphere,c(-2,-3),c(1,2),
     control=list(model=buildLM)) #lm as surrogate
res$xbest

## ---- spotBO------------------------------------------------------------------
res <- spot(,funSphere,c(-2,-3),c(1,2),
     control=list(model=buildBO,
     modelControl=list(target="ei"))) #BO as surrogate
res$xbest

## ---- spotLMOPT---------------------------------------------------------------
res <- spot(,funSphere,c(-2,-3),c(1,2),
   control=list(model=buildLM, optimizer=optimLBFGSB))
res$xbest

## ---- spotLasso---------------------------------------------------------------
res <- spot(,funSphere,c(-2,-3),c(1,2), 
   control=list(model=buildLasso, optimizer = optimNLOPTR))
res$xbest

## ---- spotLBFGSB--------------------------------------------------------------
res <- spot(,funSphere,c(-2,-3),c(1,2), 
   control=list(model=buildKriging, optimizer = optimLBFGSB))
res$xbest

## ---- spotNLOPTR--------------------------------------------------------------
res <- spot(,funSphere,c(-2,-3),c(1,2), 
     control=list(model=buildKriging, optimizer = optimNLOPTR))
res$xbest

## ---- spotKrigDace------------------------------------------------------------
res <- spot(,funSphere,c(-2,-3),c(1,2),
 control=list(model=buildKrigingDACE, optimizer=optimLBFGSB))
res$xbest

## ---- spotNoise---------------------------------------------------------------
# noisy objective
res1 <- spot(,function(x)funSphere(x)+rnorm(nrow(x)),c(-2,-3),c(1,2),
 		control=list(funEvals=40,noise=TRUE)) 
# noise with replicated evaluations
res2 <- spot(,function(x)funSphere(x)+rnorm(nrow(x)),c(-2,-3),c(1,2),
 		control=list(funEvals=40,noise=TRUE,replicates=2,
 		designControl=list(replicates=2))) 
# and with OCBA
res3 <- spot(,function(x)funSphere(x)+rnorm(nrow(x)),c(-2,-3),c(1,2),
 		control=list(funEvals=40,noise=TRUE,replicates=2,OCBA=TRUE,OCBABudget=1,
 		designControl=list(replicates=2))) 
# Check results with non-noisy function:
funSphere(res1$xbest)
funSphere(res2$xbest)
funSphere(res3$xbest)

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
## run spot with log
funSphereLog <- function(x){
  cbind(funSphere(x),x)
}
res2 <- spot(fun = funSphereLog,
            lower=c(0,0),
            upper=c(100,100)
)
res$logInfo
res2$logInfo

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

