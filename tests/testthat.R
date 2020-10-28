library(testthat)
library(SPOT)

options(spot.run.full.test = TRUE)

test_check("SPOT")

options(spot.run.full.test = NULL)