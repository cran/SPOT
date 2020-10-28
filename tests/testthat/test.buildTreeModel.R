context("test buildTreeModel")

test_that("tree based model on simple step fun should behave deterministically", {
	skip_on_cran() 
    set.seed(1)
    x <- seq(-1,1,1e-2)
    y0 <- c(-10,10)
    sfun0  <- stepfun(0, y0, f = 0)
    y <- sfun0(x)
    fit <- buildTreeModel(x,y)
    yhat <- predict(fit, newdata = 1)
    expect_true(all(yhat$y == 10))
})





