context("direct")

test_that("check that direct uses correct amount of funEvals", {
    res <- spot(fun = funSphere, lower = c(-5,-5),
                upper = c(5,5), control = list(
                    funEvals = 170,
                    direct = T
                ))
    expect_equal(nrow(res$x) , 170)
})
