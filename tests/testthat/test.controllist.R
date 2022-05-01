context("SPOT controllist")

test_that("check that control is handled without problems", {
  ## Generate one initial control list using spot()
  result1 <- spot(
    x = NULL,
    fun = funSphere,
    lower = c(0, 0),
    upper = c(1, 1)
  )
  control1 <- result1$control
  ## Use this control list as an input to spot()
  result2 <- spot(
    x = NULL,
    fun = funSphere,
    lower = c(0, 0),
    upper = c(1, 1),
    control = control1
  )
  control2 <- result2$control
  control1$time <- control2$time <- NULL
  expect_true(all.equal(control1, control2))
  ## Now we change one setting:
  control3 <- control1
  control3$types <- c("integer", "integer")
  result3 <- spot(
    x = NULL,
    fun = funSphere,
    lower = c(0, 0),
    upper = c(1, 1),
    control = control3
  )
  expect_equal(control3$types, result3$control$types)
  ## Now we change one list:
  control4 <- control1
  control4$types <- c("integer", "integer")
  control4$designControl <- list(
    size = 6,
    replicates = 2,
    types = c("integer",
              "integer")
  )
  control4$yImputation$penaltyImputation <- 123
  result4 <- spot(
    x = NULL,
    fun = funSphere,
    lower = c(0, 0),
    upper = c(1, 1),
    control = control4
  )
  expect_equal(control4$designControl,
               result4$control$designControl)
  expect_equal(
    control4$yImputation$penaltyImputation,
    result4$control$yImputation$penaltyImputation
  )
})


test_that("check that control is generated without problems", {
  fun <- funSphere
  lower <- c(0,0)
  upper <- c(1,1)
  control <- list()
  control <- spotFillControlList(control, lower, upper)
  cl <- spotControl(dimension = length(lower))
  expect_equal(cl$types, control$types)
})
