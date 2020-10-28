context("verbosity")

test_that("test that warnings will correctly be ignored / shown when setting verbosity accordingly", {
    ## Optim DE will try to show a warning in the following examples, by default it should be ignored
    
    ## Default setting shouldnt show warning
    expect_warning(
        spot(NULL, fun = funSphere, lower = c(-2,-2), upper = c(2,2), 
             control = list(optimizerControl = list("populationSize" = 10, "funEvals" = 30), 
                            model = buildRandomForest, optimizer = optimDE))
        , NA)
    
    ## Verbosity 1 should show warning
    expect_warning(
        spot(NULL, fun = funSphere, lower = c(-2,-2), upper = c(2,2), 
             control = list(optimizerControl = list("populationSize" = 10, "funEvals" = 30), 
                            model = buildRandomForest, optimizer = optimDE,
                            verbosity = 1)))
    
    ## Verbosity 0 shouldnt show warning
    expect_warning(
        spot(NULL, fun = funSphere, lower = c(-2,-2), upper = c(2,2), 
             control = list(optimizerControl = list("populationSize" = 10, "funEvals" = 30), 
                            model = buildRandomForest, optimizer = optimDE,
                            verbosity = 0))
        , NA)
})

test_that("SPOT warns if wrong level is given for verbosity", {
    ## Optim DE will try to show a warning in the following examples, by default it should be ignored
    
    ## SPOT doesnt warn if no level is given
    expect_warning(
        spot(NULL, fun = funSphere, lower = c(-2,-2), upper = c(2,2), control = list())
        , NA)
    
    ## SPOT doesnt warn if correct level is given
    expect_warning(
        spot(NULL, fun = funSphere, lower = c(-2,-2), upper = c(2,2), control = list(verbosity = 0))
        , NA)
    
    ## SPOT Fails if wrong level is given
    expect_error(
        spot(NULL, fun = funSphere, lower = c(-2,-2), upper = c(2,2), control = list(verbosity = "notALevel")))
})
