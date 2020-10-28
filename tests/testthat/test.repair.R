context("repair")

test_that("duplicateAndReplicateHandling", {
    dim = 2
    lower = c(-2, -3)
    upper = c(1, 2)
    fun = funSphere
    control <- spotControl(dimension = dim)
    control$funEvals = 2
    control$designControl$size = 2
    res <- spot(,
                fun = fun,
                lower = lower,
                upper = upper,
                control)
    x <- res$x
    y <- res$y
    xnew <- matrix(runif(2), 1, 2)
    if ((!isTRUE(all.equal(xnew, x[1, ]))) &
        (!isTRUE(all.equal(xnew, x[2, ])))) {
        ##  Here, xrepaired == xnew (no duplicate => no repair)
        xrepaired <- duplicateAndReplicateHandling(xnew = xnew,
                                                   x = x,
                                                   lower, upper, control)
        expect_equal(xnew, xrepaired)
    }
    ## Here, xrepaired != xidentical, because xidentical == x[1,]
    xidentical <- x[1, , drop = FALSE]
    xrepaired <-
        duplicateAndReplicateHandling(xnew = xidentical, x = x
                                      , lower, upper, control)
    expect_false(isTRUE(all.equal(xidentical, xrepaired)))
})
