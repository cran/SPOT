context("SPOT (S-1) Initial design test")

test_that("S-1 Initial design with default settings", {
  ## (S-0) Setup:
  fun <- funNoise
  lower <- c(-1,-1)
  upper <- c(1,1)
  control <- list(OCBA=TRUE,
                  OCBABudget=3,
                  replicates=2,
                  noise=TRUE,
                  multiStart = 2,
                  designControl = list(replicates=2)) #, size =10
  
  control <- spotFillControlList(control, lower, upper)
  
  ## (S-1) Initial design:
  set.seed(control$seedSPOT)
  x <- control$design(
    x = NULL,
    lower = lower,
    upper = upper,
    control = control$designControl
  )
  # Test code:
  dimension <- length(lower)
  expect_equal(dim(x), c(10*control$designControl$replicates, dimension))

  # x <- repairNonNumeric(x, control$types)
})


# Q_1: designControl$replicates = 0 should not be allowed? It ruins this test!!!

# Q_2: How to test repairNonNumeric function? Is this function already tested? 

test_that("S-1 Initial design with random designControl", {
  ## (S-0) Setup:
  fun <- funNoise
  
  # Define random dimensions number
  dimension <- sample(1:20, size = 1)
  # Random boundaries
  lower <- sample(-5:0, size = dimension, replace =  TRUE )
  upper <- sample( 1:5, size = dimension, replace =  TRUE )
  
  # Random designControl (replicates and size)
  control <- list(OCBA=TRUE,
                  OCBABudget=3, 
                  replicates=2,
                  noise=TRUE,
                  multiStart = 2,
                  designControl = list(replicates= sample(1:20, size = 1),
                                       size =      sample(5:30, size = 1))) #, size =10
  
  control <- spotFillControlList(control, lower, upper)
  
  ## (S-1) Initial design:
  set.seed(control$seedSPOT)
  x <- control$design(
    x = NULL,
    lower = lower,
    upper = upper,
    control = control$designControl
  )
  
  # Test code:
  expect_equal(dim(x), c(control$designControl$size*control$designControl$replicates, dimension))
  
  # x <- repairNonNumeric(x, control$types)
})



# Run test with random design 100 times
# for (i in c(1:100)){
#   print(i)
# }