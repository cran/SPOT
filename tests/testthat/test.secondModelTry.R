context("evaluateModel")

test_that("test evaluate model", {
    set.seed(1)

    lower = c(-2, -3)
    upper = c(1, 2)

    modelFun <- function(x , y, control){
        res <- buildKriging(x, y, control)
        class(res) <- "failureModel"
        return(res)
    }

    failureCount <- 1
    predict.failureModel <<- function(object, newdata, ...){
        failureCount <<- failureCount + 1
        if(failureCount > 5){
            failureCount <<- 1
        }
        if(failureCount == 5){
            stop("simulated error in predicting")
        }
        return(predict.kriging(object, newdata, ...))
    }
    expect_message(
    res <- spot(x = NULL, fun = funSphere,
         lower = lower, upper = upper,
         control = list(
             model = modelFun,
             verbosity = 0
         )))
})
