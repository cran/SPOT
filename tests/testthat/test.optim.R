context("Optim")

test_that("check optimizer are evaluated without errors", {
    ## test optimDE.R: it should generate the same results as the original DEoptim() function
    skip_on_cran() 
    Rosenbrock <- function(x) {
        100 * (x[2] - x[1] * x[1]) ^ 2 + (1 - x[1]) ^ 2
    }
    set.seed(1234)
    resDE <-
        DEoptim(
            Rosenbrock,
            lower = c(-10,-10),
            upper = c(10, 10),
            DEoptim.control(
                VTR = -Inf,
                strategy = 2,
                bs = FALSE,
                NP = 20,
                itermax = 28,
                CR = 0.7,
                F = 1.2,
                trace = FALSE,
                p = 0.2,
                c = 0,
                reltol = sqrt(.Machine$double.eps),
                steptol = 200,
                parallelType = 0
            )
        )
    set.seed(1234)
    resSPOT <- optimDE(
        ,
        fun = funRosen,
        lower = c(-10,-10),
        upper = c(10, 10),
        control = list(
            populationSize = 20,
            funEvals = 580,
            F = 1.2,
            CR = 0.7
        )
    )
    expect_equal(resDE$optim$bestval , as.double(resSPOT$ybest))
})