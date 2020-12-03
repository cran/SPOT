context("verbosity")

test_that("test that warnings will correctly be ignored / shown when setting verbosity accordingly", {
    ## Optim DE will try to show a warning in the following examples, by default it should be ignored
    
    ## Default setting show warning:
    ## For many problems it is best to set 'NP' (in 'control') to be 
    ## at least ten times the length of the parameter vector.
    expect_warning(
        spot(NULL, fun = funSphere, lower = c(-2,-2), upper = c(2,2), 
             control = list(optimizerControl = list("populationSize" = 10, "funEvals" = 30), 
                            model = buildRandomForest, optimizer = optimDE)))
    
})

test_that("SPOT warns if wrong level is given for verbosity", {
    ## SPOT Fails if wrong level is given
    expect_error(
        spot(NULL, fun = funSphere, lower = c(-2,-2), upper = c(2,2), control = list(verbosity = "notALevel")))
})
