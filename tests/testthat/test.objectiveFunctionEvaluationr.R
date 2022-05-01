context("objectiveFunctionEvaluation")

test_that("random seed handling", {
  
  res1a <- spot(x = NULL,
                fun = function(x,seed){set.seed(seed);funSphere(x)+rnorm(nrow(x))},
                  lower= c(-2,-3),
                  upper = c(1,2),
                  control=list(funEvals=15,noise=TRUE,seedFun=1)
                  )
  res1b <- spot(x=NULL,
                fun=function(x,seed){
                  set.seed(seed)
                  funSphere(x)+rnorm(nrow(x))},
                lower=c(-2,-3),
                upper=c(1,2),
                control=list(funEvals=15,noise=TRUE,seedFun=1))

  res2 <- spot(x=NULL,
                 function(x,seed){set.seed(seed)
                   funSphere(x)+rnorm(nrow(x))},
                 lower=c(-2,-3),
                 upper=c(1,2),
                 control=list(funEvals=15,noise=TRUE,seedFun=2))
    expect_true(identical(res1a$ybest, res1b$ybest))
    expect_false(identical(res1a$ybest, res2$ybest))
})
