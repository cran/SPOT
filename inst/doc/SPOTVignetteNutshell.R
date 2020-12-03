## ----knitrSetup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----SPOT, eval = FALSE-------------------------------------------------------
#  ## install.packages("devtools")
#  ## devtools::install_github("r-lib/devtools")
#  url <- "http://owos.gm.fh-koeln.de:8055/bartz/spot.git"
#  devtools::install_git(url = url)

## ----setup--------------------------------------------------------------------
library("SPOT")
packageVersion("SPOT")

## ---- loadSpot----------------------------------------------------------------
library("SPOT")

## ---- sphereDef---------------------------------------------------------------
sphere <- function (x){
  sum(x^2)
  }
sphere( c(1,2) )

## ---- funSphereDef------------------------------------------------------------
funSphere

## ---- plotSphere, eval FALSE--------------------------------------------------
plotFunction(funSphere)

## ---- resSANNrun--------------------------------------------------------------
set.seed(123)
resSANN <- optim(c(10,10), sphere, method="SANN",
                control=list(maxit=100, temp=10, tmax = 10))
resSANN

## ---- resSANN2----------------------------------------------------------------
set.seed(123)
resSANN <- optim(par = c(10,10), fn = sphere, method="SANN",
                control = list(maxit = 100, temp = 20, tmax = 5))
resSANN

## ---- defx0-------------------------------------------------------------------
x0 =  c(-1,1,-1) 

## ---- defMaxit----------------------------------------------------------------
maxit = 100 

## ---- sann2SPOT---------------------------------------------------------------
sann2spot <- function(algpar){
  performance <- NULL
  for (i in 1:nrow(algpar)){
    resultList <- optim(par = c(10,10),
                    fn = sphere,
                    method = "SANN",
                    control = list(maxit = 100,
                                  temp = algpar[i,1],
                                  tmax = algpar[i,2]))
    performance <- c(performance,resultList$value)
    }
return(matrix(performance,,1))
}

## ---- sann2SPOT2--------------------------------------------------------------
set.seed(123)
sann2spot(algpar = matrix(c(10,10),1))

## ---- sann2SPOT3--------------------------------------------------------------
set.seed(123)
sann2spot(algpar=matrix(c(5,20),1))

## ---- spotConfig--------------------------------------------------------------
spotConfig <- list(
  types = c("integer", "integer"), #data type of tuned parameters
  funEvals = 50, #maximum number of SANN runs
  noise = TRUE, #problem is noisy (SANN is non-deterministic)
  seedFun = 1, #RNG start seed for algorithm calls (iterated)
  replicates = 2, #2 replicates for each SANN parameterization
  seedSPOT = 1, #main RNG
  design = designLHD, #initial design: Latin Hypercube
  model = buildRandomForest, # model = buildKriging Kriging surrogate model
  optimizer = optimLHD, #Use LHD to optimize on model 
  optimizerControl = list(funEvals=100) #100 model evals in each iteration  
)

## ---- lowHigh-----------------------------------------------------------------
tempLo = 1
tempHi = 100
tmaxLo = 1
tmaxHi = 100
lower=c(tempLo,tmaxLo)
upper=c(tempHi,tmaxHi)

## ---- resRfrun----------------------------------------------------------------
# library(SPOT)
# source('~/workspace/SPOT/R/spot.R')
# source('~/workspace/SPOT/R/initialInputCheck.R')
resRf <- spot(x=NULL,
              fun=sann2spot,
              lower=lower,
              upper=upper,
              control=spotConfig)
is.null(spotConfig$optimizerControl$eval_g_ineq)

## ---- resRfStr----------------------------------------------------------------
str(resRf)

## ---- showBest----------------------------------------------------------------
cbind(resRf$xbest, resRf$ybest)

## ---- showxBest---------------------------------------------------------------
resRf$xbest[1]

## ---- showXbest2--------------------------------------------------------------
resRf$xbest[2] 

## ---- spotConf10--------------------------------------------------------------
spotConfig10 <- list(
  funEvals = 10,
  designControl = list(
    size = 6,
    replicates = 1
  ),
  noise = TRUE,
  seedFun = 1,
  seedSPOT = 1,
  replicates = 2,
  model = buildRandomForest 
)

## ---- res10-------------------------------------------------------------------
res10 <- spot( ,fun=sann2spot
              ,lower=lower
              ,upper=upper
              ,control=spotConfig10)

## ---- res10x------------------------------------------------------------------
cbind(res10$x, res10$y)

## ---- desLhd------------------------------------------------------------------
designLHD(,-1,1) 

## ---- desLhd2-----------------------------------------------------------------
designLHD(, c(-1,-2,1,0),c(1,4,9,1)
          , control=list(size=5, retries=100, types=c("numeric","integer","factor","factor")))

## ---- combDes-----------------------------------------------------------------
set.seed(123)
x1 <- designLHD(,c(-1,-1),c(1,1),control=list(size=50,retries=100))
x2 <- designLHD(x1,c(-2,-2),c(2,2),control=list(size=50,retries=100))

## ---- plotCombDes, eval=FALSE-------------------------------------------------
#  plot(x2,pch=1)
#  points(x1, pch=4)

## ---- desUni------------------------------------------------------------------
designUniformRandom(,c(-1,0),c(1,10),control=list(size=5))

## ---- braninF-----------------------------------------------------------------
# Objective function
braninFunction <- function (x) {
    (x[2]  - 5.1/(4 * pi^2) * (x[1] ^2) + 5/pi * x[1]  - 6)^2 + 10 * (1 - 1/(8 * pi)) * cos(x[1] ) + 10
}
## Create 20 design points
set.seed(1)
x <- cbind(runif(20)*15-5, runif(20)*15)
## Compute observations at design points (for Branin function)
y <- as.matrix(apply(x,1,braninFunction))
## Create model with default settings
fit <- buildKriging(x,y,control = list(algTheta=optimLHD))
## Print model parameters
print(fit)
##Define a new location
newloc <- matrix(c(1,2),nrow =1 )
##Predict at new location
predict(fit,newloc)
## True value at location
braninFunction(newloc)
## 

## ---- braninFunctionFactor----------------------------------------------------
braninFunctionFactor <- function (x) {  
y <- (x[2] - 5.1 / (4 * pi^2) * (x[1]^2) + 5 / pi * x[1] - 6)^2 + 10 * (1 - 1 / (8 * pi)) * cos(x[1]) + 10
    if(x[3] == 1)
        y <- y + 1
    else if(x[3]==2)
        y <- y - 1
    y  
}

## ---- fitDefault--------------------------------------------------------------
set.seed(1)
## Replace x with new data
x <- cbind(runif(50)*15-5,runif(50)*15,sample(1:3,50,replace=TRUE))
##
y <- as.matrix(apply(x,1,braninFunctionFactor))
fitDefault <- buildKriging(x,y,control = list(algTheta=optimLBFGSB))

## ---- fitFactor---------------------------------------------------------------
fitFactor <- buildKriging(x,y,control = list(algTheta=optimLBFGSB,types=c("numeric","numeric","factor")))

## ---- resNewData--------------------------------------------------------------
##Replace xtest with new data
xtest <- cbind(runif(200)*15-5,runif(200)*15,sample(1:3,200,replace=TRUE))
##
ytest <- as.matrix(apply(xtest,1,braninFunctionFactor))
## Predict test data with both models, and compute error
ypredDef <- predict(fitDefault,xtest)$y
ypredFact <- predict(fitFactor,xtest)$y
mean((ypredDef-ytest)^2)
mean((ypredFact-ytest)^2)

## ---- resOptimLhd-------------------------------------------------------------
resOptimumLHD <- optimLHD(,fun = funSphere,lower = c(-10,-20),upper=c(20,8))
str(resOptimumLHD)
resOptimumLHD$ybest

## ---- resOptimBfgs------------------------------------------------------------
resOptimBFGS <- optimLBFGSB(,fun = funSphere,lower = c(-10,-20),upper=c(20,8))
resOptimBFGS$ybest

## ---- control01---------------------------------------------------------------
control01 <- list(
    designControl = list(size = 5, 
                       replicates = 1),    
    funEvals = 5)
res1 <- spot(,funSphere,
             lower = c(-2,-3),
             upper = c(1,2),
             control01)
cbind(res1$x, res1$y)

## ---- control01a--------------------------------------------------------------
control01$funEvals <- 8
res2 <- spotLoop(res1$x,
                 res1$y,
                 funSphere,
                 lower = c(-2,-3),
                 upper = c(1,2),
                 control01)
cbind(res2$x, res2$y)

## ---- plotFunSphere, eval=FALSE-----------------------------------------------
#  plotFunction(funSphere, rep(-1,2), rep(1,2))

## ---- myFunc, eval=FALSE------------------------------------------------------
#  myFunction <- function (x){
#    matrix(apply(x, # matrix
#                 1, # margin (apply over rows)
#                 function(x) sum(x^3-1) # objective function
#                 ),
#           , 1) # number of columns
#    }
#  plotFunction(myFunction,
#               rep(-1,2),
#               rep(1,2),
#               color.palette = rainbow)

## ---- plotPersp2, eval = FALSE------------------------------------------------
#  plotFunction(myFunction,
#               rep(-1,2),
#               rep(1,2),
#               type="persp",
#               theta=10,
#               phi=25,
#               border = NA)

## ---- head3dex----------------------------------------------------------------
set.seed(123)
k <- 30
x.test <- designLHD(,rep(-1,3),rep(1,3), control = list(size = k)) 
y.test <- funSphere(x.test)
head( cbind(x.test, y.test))

## ---- plot3dex, eval =FALSE---------------------------------------------------
#  fit.test <- buildRSM(x.test,y.test)
#  plotModel(fit.test)

## ---- plotContour1, eval=FALSE------------------------------------------------
#  plotModel(fit.test,which=c(1,3),type="contour",pch1=24,col1="blue")

## ---- plotModel2, eval=FALSE--------------------------------------------------
#  plotModel(fit.test,which=c(1,3),type="persp",border="NA",theta=255,phi=20)

## ---- plotDataxy, eval=FALSE--------------------------------------------------
#  plotData(x.test,y.test)

## ---- plotRf, eval = FALSE----------------------------------------------------
#  plotData(x.test,y.test,type="filled.contour",cex1=1,col1="red",pch1=21,model=buildRandomForest)

## ---- plotPersp1, eval=FALSE--------------------------------------------------
#  plotData(x.test,y.test,type="persp",border=NA,model=buildLOESS)

## ---- plotRfmodel, eval=FALSE-------------------------------------------------
#  plotModel(resRf$modelFit)

## ---- plotLoess, eval=FALSE---------------------------------------------------
#  plotData(resRf$x,resRf$y,model=buildLOESS)

## ---- plotKrig, eval=FALSE----------------------------------------------------
#  plotData(resRf$x,resRf$y,model=buildKriging)

## ---- changeConfig------------------------------------------------------------
spotConfig$model = buildKriging
spotConfig$optimizer = optimLBFGSB
spotConfig$modelControl = list(algTheta=optimLBFGSB)
## Run SPOT
resK <- spot(x=NULL,
             fun=sann2spot,
             lower=lower,
             upper=upper,
             control=spotConfig)

## ---- plotKrigingRes, eval=FALSE----------------------------------------------
#  plotModel(resK$modelFit)

## ---- contRun-----------------------------------------------------------------
spotConfig$funEvals <- 100
spotConfig$model <- buildRandomForest
res100Rf <- spotLoop(resRf$x,
                     resRf$y,
                     fun=sann2spot,
                     lower=lower,
                     upper=upper,
                     control=spotConfig)

## ---- contRun100--------------------------------------------------------------
spotConfig$model = buildKriging
spotConfig$optimizer = optimLBFGSB
spotConfig$modelControl = list(algTheta=optimLBFGSB)
res100K <- spotLoop(resK$x,
                    resK$y,
                    fun=sann2spot,
                    lower=lower,
                    upper=upper,
                    control=spotConfig)

## ---- plotRes100K, eval=FALSE-------------------------------------------------
#  plotModel(res100Rf$modelFit)

## ---- plotResKriging, eval=FALSE----------------------------------------------
#  plotModel(res100K$modelFit)

## ---- desUnifRand-------------------------------------------------------------
x <- designUniformRandom(lower=rep(-5,2), 
                         upper=rep(15,2), 
                         control=list(size=20))
y <- funSphere(x)

## ---- buildRsm1---------------------------------------------------------------
fit <- buildRSM(x,y)

## ---- predictRsm1-------------------------------------------------------------
predict(fit,cbind(1,2))

## ---- showReal----------------------------------------------------------------
sphere(c(1,2))

## ---- descentSpot-------------------------------------------------------------
descentSpotRSM(fit)

## ---- plotFitSurf, eval=FALSE-------------------------------------------------
#  plot(fit)

## ---- res100Kfit--------------------------------------------------------------
rsm100K <- buildRSM(x=res100K$x,
                    y=res100K$y) 
summary(rsm100K$rsmfit)

## ---- res100Kdesc-------------------------------------------------------------
(xSteep <- descentSpotRSM(rsm100K) )

## ---- xnew8-------------------------------------------------------------------
xNew <- xSteep$x[8,]

## ---- xnew8Function-----------------------------------------------------------
(yNew <- sann2spot(xNew))

## ---- res100KrsmFit-----------------------------------------------------------
x101 <- rbind(res100K$x, xNew)
y101 <- rbind(res100K$y, yNew)
rsm101K <- buildRSM(x=x101,
                    y=y101) 
summary(rsm101K$rsmfit)

## ---- plotRes101K, eval=FALSE-------------------------------------------------
#  plot(rsm101K)

## ---- desc101Kres-------------------------------------------------------------
descentSpotRSM(rsm101K)

## ---- spotConf101-------------------------------------------------------------
spotConfig$model = buildKriging
spotConfig$optimizer = optimLBFGSB
spotConfig$modelControl = list(algTheta=optimLBFGSB)
spotConfig$funEvals <- 110
res110K <- spotLoop(x=x101,
                    y=y101,
                    fun=sann2spot,
                    lower=lower,
                    upper=upper,
                    control=spotConfig)

## ---- plotRes101Kfit, eval=FALSE----------------------------------------------
#  plotModel(res110K$modelFit)

## ---- plotParty, eval=FALSE---------------------------------------------------
#  tmaxtempz.df <- data.frame(res100K$x[,1], res100K$x[,2], res100K$y)
#  names(tmaxtempz.df) <- c("tmax", "temp", "y")
#  tmaxtempz.tree <- party::ctree(y ~ ., data=tmaxtempz.df)
#  plot(tmaxtempz.tree, type="simple")

## ---- lm100K------------------------------------------------------------------
xyz100K.df <- data.frame(res100K$x[,1], res100K$x[,2], res100K$y)
names(xyz100K.df) <- c("x", "y", "z")
lm100K <- lm(z ~ x*y, data=xyz100K.df)
summary(lm100K)

## ---- plotLm100K, eval=FALSE--------------------------------------------------
#  plot(lm100K)

## ---- plotTerm, eval=FALSE----------------------------------------------------
#  par(mfrow=c(1,2))
#  termplot(lm100K, partial = TRUE, smooth = panel.smooth, ask=FALSE)
#  par(mfrom=c(1,1))

## ---- avPlot, eval=FALSE------------------------------------------------------
#  par(mfrow=c(1,3))
#  car::avPlots(lm100K,ask=F)
#  par(mfrow=c(1,1))

## ---- spot55------------------------------------------------------------------
res <- spot(,funSphere,c(-5,-5),c(5,5), control=list(optimizer=optimLBFGSB)) 

## ---- res55-------------------------------------------------------------------
res$xbest
res$ybest

## ---- fitstack, eval=TRUE-----------------------------------------------------
fit.stack <- buildEnsembleStack(x.test, y.test)

## ---- plotStack, eval=FALSE---------------------------------------------------
#  plotModel(fit.stack)

## ---- predictStack------------------------------------------------------------
xNew <-  cbind(1,1,1)
predict(fit.stack, xNew)
funSphere(xNew)

