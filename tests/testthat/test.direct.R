context("direct")

test_that("check that direct uses correct amount of funEvals: here zero", {
    res <- spot(fun = funSphere, lower = c(-5,-5),
                upper = c(5,5), 
                control = list(funEvals = 20,
                directOpt = optimNLOPTR,
                directOptControl = list(funEvals = 0)
                ))
    expect_equal(nrow(res$x) , 20)
})
