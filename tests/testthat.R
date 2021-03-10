library(testthat)
library(SPOT)

options(spot.run.full.test = FALSE)

test_check("SPOT")

options(spot.run.full.test = NULL)