context("SPOT (S-0) Setup test")
test_that("S-0 Setup with default settings", {
  fun <- funNoise
  lower <- c(-1,-1)
  upper <- c(1,1)
  control <- list(OCBA=TRUE,
                  OCBABudget=3,
                  replicates=2,
                  noise=TRUE,
                  multiStart = 2,
                  designControl = list(replicates=2))
  
  control_spot <- spotFillControlList(control, lower, upper)
  
  expect_equal(control$OCBA, control_spot$OCBA)
  expect_equal(control$OCBABudget, control_spot$OCBABudget)
  expect_equal(control$replicates, control_spot$replicates)
  expect_equal(control$noise, control_spot$noise)
  expect_equal(control$multiStart, control_spot$multiStart)
  
  #expect_equal(control$designControl, control_spot$designControl) FAIL
  expect_equal(control$designControl$replicates, control_spot$designControl$replicates) 
  expect_equal(control_spot$lower, lower)
  expect_equal(control_spot$upper, upper)
})

# Q_1: We have two 'replicates' in control list and in designControl list. Should they be the same? 


test_that("S-0 Setup with random settings", {
  fun <- funNoise
  
  # Define random dimensions number
  dimension <- sample(3:20, size = 1)
  
  lower <- sample(-5:0, size = dimension, replace =  TRUE )
  upper <- sample( 1:5, size = dimension, replace =  TRUE )
  
  # All control parameters defined randomly (the sample() function is used)
  control <- list(OCBA=as.logical(sample(0:1,1)),
                  OCBABudget= sample(3:20, size = 1),
                  replicates= sample(3:20, size = 1),
                  noise=as.logical(sample(0:1,1)),
                  multiStart = sample(3:20, size = 1),
                  designControl = list(replicates=sample(3:20, size = 1)))
  
  control_spot <- spotFillControlList(control, lower, upper)
  
  expect_equal(control$OCBA, control_spot$OCBA)
  expect_equal(control$OCBABudget, control_spot$OCBABudget)
  expect_equal(control$replicates, control_spot$replicates)
  expect_equal(control$noise, control_spot$noise)
  expect_equal(control$multiStart, control_spot$multiStart)
  
  expect_equal(control$designControl$replicates, control_spot$designControl$replicates) 
  expect_equal(control_spot$lower, lower)
  expect_equal(control_spot$upper, upper)
})


