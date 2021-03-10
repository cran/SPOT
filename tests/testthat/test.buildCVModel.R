context("test buildCVModel")

test_that("calling with different models results in different stuff", {
		skip_on_cran() 
    if(getOption("spot.run.full.test")){
    set.seed(1)
    resA <- spot(,funSphere,c(-2,-3),c(1,2), control = list(optimizer=optimDE, model = buildCVModel, modelControl = 
                                                        list(modellingFunction = buildKriging)))
    set.seed(1)
    resB <- spot(,funSphere,c(-2,-3),c(1,2), control = list(optimizer=optimDE, model = buildCVModel))
    
    set.seed(1)
    suppressWarnings(resC <- spot(,funSphere,c(-2,-3),c(1,2), control = list(optimizer=optimDE, model = buildCVModel, modelControl = 
                                                        list(modellingFunction = buildRandomForest))))
    
    expect_true(all(resA$x == resB$x))
    expect_true(!all(resA$x == resC$x))
    }
})