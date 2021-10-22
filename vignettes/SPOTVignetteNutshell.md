---
title: "SPOTVignetteNutshell"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{SPOTVignetteNutshell}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---






* Package version SPOT should be larger than `2.3.0`.


```r
library("SPOT")
packageVersion("SPOT")
#> [1] '2.4.0'
```

## Introduction

* The performance of modern search heuristics relies crucially 
on their parameterizations---or, statistically speaking, on their factor settings. 
* Finding good parameter settings for an optimization algorithm will be referred to as *tuning*.
* The version of `SPOT` presented in this article is implemented in `R`.
* It can be used as an algorithm tuner or directly as an optimizer.
* This vignette is an abbreviated version of a the technical report "In a Nutshell-The Sequential Parameter Optimization Toolbox".
* It illustrates how `spot` can be called. 
* The extended version of this vignette is available on [arXiv](https://arxiv.org/abs/1712.04076) and will be updated regularly.

## Sequential Parameter Optimization Examples: How to Call SPOT

### Most simple example: Kriging + LHS + predicted  mean optimization (not expected improvement)


```r
res <- spot(,funSphere,c(-2,-3),c(1,2),control=list(funEvals=15))
res$xbest
#>            [,1]      [,2]
#> [1,] -0.1086201 0.1184503
```

### With expected improvement


```r
res <- spot(,funSphere,c(-2,-3),c(1,2),
    control=list(funEvals=15,modelControl=list(target="ei")))
res$xbest
#>            [,1]      [,2]
#> [1,] -0.1086201 0.1184503
```

### With additional start point:


```r
res <- spot(matrix(c(0.05,0.1),1,2),funSphere,c(-2,-3),c(1,2))
res$xbest
#>             [,1]        [,2]
#> [1,] -0.06104759 -0.05040567
```

### Larger budget:


```r
res <- spot(,funSphere,c(-2,-3),c(1,2),
    control=list(funEvals=50))
res$xbest
#>            [,1]        [,2]
#> [1,] 0.02088315 -0.03783177
```

### Use local optimization instead of LHS


```r
res <- spot(,funSphere,c(-2,-3),c(1,2),
   control=list(optimizer=optimLBFGSB))
 res$xbest
#>             [,1]         [,2]
#> [1,] -0.01573496 -0.008860252
```

### Random Forest instead of Kriging


```r
res <- spot(,funSphere,c(-2,-3),c(1,2),
     control=list(model=buildRandomForest))
res$xbest
#>           [,1]      [,2]
#> [1,] 0.1531584 0.3294388
```

### LM instead of Kriging


```r
res <- spot(,funSphere,c(-2,-3),c(1,2),
     control=list(model=buildLM)) #lm as surrogate
res$xbest
#>           [,1]      [,2]
#> [1,] 0.1531584 0.3294388
```

### Bayesian Optimization


```r
res <- spot(,funSphere,c(-2,-3),c(1,2),
     control=list(model=buildBO)) #BO as surrogate
res$xbest
#>              [,1]        [,2]
#> [1,] -0.009619762 -0.01032117
```



### LM and local optimizer (which for this simple example is perfect)

```r
res <- spot(,funSphere,c(-2,-3),c(1,2),
   control=list(model=buildLM, optimizer=optimLBFGSB))
res$xbest
#>             [,1]       [,2]
#> [1,] -0.02651579 -0.1091904
```

### Lasso and local optimizer NLOPTR


```r
res <- spot(,funSphere,c(-2,-3),c(1,2), 
   control=list(model=buildLasso, optimizer = optimNLOPTR))
#> Warning: Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per
#> fold

#> Warning: Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per
#> fold

#> Warning: Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per
#> fold

#> Warning: Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per
#> fold

#> Warning: Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per
#> fold

#> Warning: Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per
#> fold

#> Warning: Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per
#> fold

#> Warning: Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per
#> fold

#> Warning: Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per
#> fold

#> Warning: Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per
#> fold
res$xbest
#>           [,1]      [,2]
#> [1,] 0.1531584 0.3294388
```

### Kriging and local optimizer LBFGSB 


```r
res <- spot(,funSphere,c(-2,-3),c(1,2), 
   control=list(model=buildKriging, optimizer = optimLBFGSB))
res$xbest
#>             [,1]         [,2]
#> [1,] -0.01573496 -0.008860252
```

### Kriging and local optimizer NLOPTR 


```r
res <- spot(,funSphere,c(-2,-3),c(1,2), 
     control=list(model=buildKriging, optimizer = optimNLOPTR))
res$xbest
#>             [,1]         [,2]
#> [1,] -0.01440329 0.0006858711
```

### Or a different Kriging model:


```r
res <- spot(,funSphere,c(-2,-3),c(1,2),
 control=list(model=buildKrigingDACE, optimizer=optimLBFGSB))
res$xbest
#>             [,1]         [,2]
#> [1,] -0.03586733 -0.004407048
```

### With noise: (this takes some time)


```r
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
#>           [,1]
#> [1,] 0.9811919
funSphere(res2$xbest)
#>         [,1]
#> [1,] 1.74417
funSphere(res3$xbest)
#>          [,1]
#> [1,] 1.084404
```

### Random number seed handling

The following is for demonstration only, to be used for random number
seed handling in case of external noisy target functions.


```r
res1a <- spot(,function(x,seed){set.seed(seed);funSphere(x)+rnorm(nrow(x))},
     c(-2,-3),c(1,2),control=list(funEvals=25,noise=TRUE,seedFun=1))
```


```r
res1b <- spot(,function(x,seed){set.seed(seed);funSphere(x)+rnorm(nrow(x))},
     c(-2,-3),c(1,2),control=list(funEvals=25,noise=TRUE,seedFun=1))
```


```r
res2 <- spot(,function(x,seed){set.seed(seed);funSphere(x)+rnorm(nrow(x))},
     c(-2,-3),c(1,2),control=list(funEvals=25,noise=TRUE,seedFun=2))
```



```r
sprintf("Should be equal: %f = %f. Should be different:  %f", res1a$ybest, res1b$ybest, res2$ybest)
#> [1] "Should be equal: -1.296329 = -1.296329. Should be different:  -0.889934"
```




### Handling factor variables

Note: factors should be coded as integer values, i.e., 1,2,...,n
First, we create a test function with a factor variable:


```r
braninFunctionFactor <- function (x) {
   y <- (x[2]  - 5.1/(4 * pi^2) * (x[1] ^2) + 5/pi * x[1]  - 6)^2 +
     10 * (1 - 1/(8 * pi)) * cos(x[1] ) + 10
   if(x[3]==1)
     y <- y +1
   else if(x[3]==2)
     y <- y -1
   return(y)
}
```

Vectorize the test function.


```r
objFun <- function(x){apply(x,1,braninFunctionFactor)}
```

Run `spot`.


```r
set.seed(1)
res <- spot(fun=objFun,lower=c(-5,0,1),upper=c(10,15,3),
            control=list(model=buildKriging,
                         types= c("numeric","numeric","factor"),
                         optimizer=optimLHD))
 res$xbest
#>          [,1]     [,2] [,3]
#> [1,] 2.619386 2.725642    2
 res$ybest
#>           [,1]
#> [1,] 0.6777176
```

### High dimensional problem 


```r
n <- 10
a <- rep(0,n)
b <- rep(1,n)
```


First, we consider the default `spot` setting with `buildKriging()`.


```r
tic <- proc.time()[3]
res0 <- spot(x=NULL, funSphere, lower = a, upper = b, 
             control=list(funEvals=30))
toc <- proc.time()[3]
sprintf("value: %f, time: %f",  res0$ybest, toc-tic)
#> [1] "value: 0.332121, time: 9.393000"
```

Then, we use the `buildGaussianProcess()` model.


```r
tic <- proc.time()[3]
res1 <-  spot(x=NULL, funSphere, lower = a, upper = b, 
             control=list(funEvals=30, 
                          model = buildGaussianProcess))
toc <- proc.time()[3]
sprintf("value: %f, time: %f",  res1$ybest, toc-tic)
#> [1] "value: 0.530886, time: 0.650000"
```



### Run SPOT with logging


```r
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
#> [1] NA
res2$logInfo
#>            [,1]       [,2]
#>  [1,]  8.648076 81.4784571
#>  [2,] 71.771945 66.5887761
#>  [3,] 44.933187 41.8506996
#>  [4,] 14.297134 39.5437814
#>  [5,] 25.642638 58.9784849
#>  [6,] 56.561623 79.4369705
#>  [7,] 69.785541 27.2369075
#>  [8,] 92.321611 93.7035707
#>  [9,] 32.408116  7.8101754
#> [10,] 87.968361 10.1114951
#> [11,]  3.152969  3.1352284
#> [12,] 35.979045 83.1545726
#> [13,]  1.401563  0.3142767
#> [14,] 32.165836 18.4191631
#> [15,] 27.534938 83.0152530
#> [16,] 34.745003 77.4768640
#> [17,] 21.418092 95.5431781
#> [18,] 42.504694 61.7054862
#> [19,] 87.533618 15.9641532
#> [20,] 74.720086 84.2995194
```

## Hybrid optimization


```r
res <- spot(fun = funSphere, lower = c(-5,-5),
                upper = c(5,5), 
                control = list(funEvals = 20,
                directOpt = optimNLOPTR,
                directOptControl = list(funEvals = 10)
                ))
str(res)
#> List of 9
#>  $ xbest   : num [1, 1:2] 0 0
#>  $ ybest   : num 0
#>  $ x       : num [1:33, 1:2] -4.135 2.177 -0.507 -3.57 -2.436 ...
#>  $ y       : num [1:33, 1] 27.009 7.492 0.921 13.84 6.739 ...
#>  $ logInfo : logi NA
#>  $ count   : int 20
#>  $ msg     : chr "budget exhausted"
#>  $ modelFit:List of 33
#>   ..$ thetaLower      : num 1e-04
#>   ..$ thetaUpper      : num 100
#>   ..$ types           : chr [1:2] "numeric" "numeric"
#>   ..$ algTheta        :function (x = NULL, fun, lower, upper, control = list(), ...)  
#>   .. ..- attr(*, "srcref")= 'srcref' int [1:8] 47 12 107 1 12 1 47 107
#>   .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x7fb773e99ab8> 
#>   ..$ budgetAlgTheta  : num 200
#>   ..$ optimizeP       : logi FALSE
#>   ..$ useLambda       : logi TRUE
#>   ..$ lambdaLower     : num -6
#>   ..$ lambdaUpper     : num 0
#>   ..$ startTheta      : NULL
#>   ..$ reinterpolate   : logi TRUE
#>   ..$ target          : chr "y"
#>   ..$ modelInitialized: logi TRUE
#>   ..$ x               : num [1:19, 1:2] -4.135 2.177 -0.507 -3.57 -2.436 ...
#>   ..$ y               : num [1:19, 1] 27.009 7.492 0.921 13.84 6.739 ...
#>   ..$ normalizeymin   : num 0
#>   ..$ normalizeymax   : num 1
#>   ..$ scaledx         : num [1:19, 1:2] 0 0.7544 0.4337 0.0675 0.2031 ...
#>   ..$ normalizexmin   : num [1:2] -4.14 -4.22
#>   ..$ normalizexmax   : num [1:2] 4.23 4.55
#>   ..$ dmodeltheta     : num [1:2] 0.122 0.141
#>   ..$ Lambda          : num -5.96
#>   ..$ dmodellambda    : num 1.09e-06
#>   ..$ Theta           : num [1:2] -0.915 -0.852
#>   ..$ yonemu          : num [1:19, 1] -323 -342 -349 -336 -343 ...
#>   ..$ ssq             : num 14020
#>   ..$ mu              : num 350
#>   ..$ Psi             : num [1:19, 1:19] 1 0.929 0.95 0.968 0.986 ...
#>   ..$ Psinv           : num [1:19, 1:19] 77206 -36217 35306 -27605 -92449 ...
#>   ..$ nevals          : num 1200
#>   ..$ like            : num [1, 1] 1.96
#>   ..$ returnCrossCor  : logi FALSE
#>   ..$ min             : num 0.0988
#>   ..- attr(*, "class")= chr "kriging"
#>  $ ybestVec: num [1:20] 0.921 0.921 0.921 0.921 0.921 ...
```



## Handling constraints

* Definitions and constraints


```r
library(babsim.hospital)
n <- 29 
reps <- 2
funEvals <- 3*n 
size <- 2*n
x0 <- matrix(as.numeric(babsim.hospital::getParaSet(5374)[1,-1]),1,)
bounds <- getBounds()
a <- bounds$lower
b <- bounds$upper
g <- function(x) {
      return(rbind(a[1] - x[1], x[1] - b[1], a[2] - x[2], x[2] - b[2], 
                   a[3] - x[3], x[3] - b[3], a[4] - x[4], x[4] - b[4], 
                   a[5] - x[5], x[5] - b[5], a[6] - x[6], x[6] - b[6], 
                   a[7] - x[7], x[7] - b[7], a[8] - x[8], x[8] - b[8], 
                   a[9] - x[9], x[9] - b[9], a[10] - x[10], x[10] - b[10],
                   a[11] - x[11], x[11] - b[11], a[12] - x[12],  x[12] - b[12],
                   a[13] - x[13], x[13] - b[13], a[14] - x[14],  x[14] - b[14],
                   a[15] - x[15], x[15] - b[15], a[16] - x[16],  x[16] - b[16],
                   a[17] - x[17], x[17] - b[17], a[18] - x[18],  x[18] - b[18],
                   a[19] - x[19], x[19] - b[19], a[20] - x[20],  x[20] - b[20],
                   a[21] - x[21], x[21] - b[21], a[22] - x[22],  x[22] - b[22],
                   a[23] - x[23], x[23] - b[23], a[24] - x[24],  x[24] - b[24],
                   a[25] - x[25], x[25] - b[25], a[26] - x[26],  x[26] - b[26],
                   a[27] - x[27], x[27] - b[27], x[15] + x[16] - 1, 
                   x[17] + x[18] + x[19] - 1, x[20] + x[21] - 1, x[23] + x[29] - 1)
      )
  }
```



```r
res <- spot(
  x = x0,
  fun = funBaBSimHospital,
  lower = a,
  upper = b,
  verbosity = 0,
  control = list(
    funEvals = 2 * funEvals,
    noise = TRUE,
    designControl = list(# inequalityConstraint = g,
      size = size,
      retries = 1000),
    optimizer = optimNLOPTR,
    optimizerControl = list(
      opts = list(algorithm = "NLOPT_GN_ISRES"),
      eval_g_ineq = g
    ),
    model =  buildKriging,
    plots = FALSE,
    progress = TRUE,
    directOpt = optimNLOPTR,
    directOptControl = list(funEvals = 0),
    eval_g_ineq = g
  )
)
print(res)
```



