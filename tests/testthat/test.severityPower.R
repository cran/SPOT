context("severity")

test_that("Compare severity to Mayo18a, p.345", {
  alpha <- 0.025
  n <- 200
  sigma <- 450
  mu0 <- 0
  mu1 <- 200
  tdist <- FALSE
  paired <- FALSE
  xbar <- 90
  res <-
    spotSeverity(xbar, mu0, mu1, n, sigma, alpha, tdist, paired)
  real <- 0.007253771
  expect_equal(res$severity, real)
  #
  mu1 <- 100
  res <-
    spotSeverity(xbar, mu0, mu1, n, sigma, alpha, tdist, paired)
  real <- 0.4120704
  expect_equal(res$severity, real, tolerance = 1e-7)
  #
  mu1 <- 50
  res <-
    spotSeverity(xbar, mu0, mu1, n, sigma, alpha, tdist, paired)
  real <- 0.8129686
  expect_equal(res$severity, real, tolerance = 1e-7)
  #
  mu1 <- 10
  res <-
    spotSeverity(xbar, mu0, mu1, n, sigma, alpha, tdist, paired)
  real <- 0.9622798
  expect_equal(res$severity, real, tolerance = 1e-7)
})

test_that("Compare severity to Mayo18a, p.348", {
  alpha <- 0.025
  n <- 100
  sigma <- 10
  mu0 <- 0
  mu1 <- 0.5
  tdist <- FALSE
  paired <- TRUE
  xbar <- 1.5*sigma/sqrt(n)+mu0
  res <-
    spotSeverity(xbar, mu0, mu1, n, sigma, alpha, tdist, paired)
  real <- 0.1586553 ## Mayo reports 0.16
  expect_equal(res$severity, real, tolerance = 1e-7)
  #
  mu1 <- 2.5
  res <-
    spotSeverity(xbar, mu0, mu1, n, sigma, alpha, tdist, paired)
  real <- 0.8413447 ## Mayo18a, p. 349,  reports 0.84
  expect_equal(res$severity, real, tolerance = 1e-7)
  
  
})

test_that("Paired setting: identical to t.test", {
  # Example from Vena02a, p. 117
  # library("BHH2")
  # data(shoes.data)
  # A <- shoes.data$matA
  # B <- shoes.data$matB
  A <- c(13.2, 8.2, 10.9, 14.3, 10.7, 6.6, 9.5, 10.8, 8.8, 13.3)
  B <- c(14, 8.8, 11.2, 14.2, 11.8, 6.4, 9.8, 11.3, 9.3, 13.6)
  t.paired <-
    t.test(
      x = A,
      y = B,
      var.equal = TRUE,
      paired = TRUE,
      alternative = "greater",
      conf.level = 0.95
    )
  xbar <- mean(A - B)
  n <- length(A) 
  sigma <- sd(A - B)
  s.paired <-
    spotSeverity(
      xbar = xbar,
      mu0 = 0,
      mu1 = 1,
      n = n,
      sigma = sigma,
      alpha = 0.05,
      paired = TRUE,
      tdist = TRUE
    )
  expect_equal(s.paired$statistic, as.numeric(t.paired$statistic))
  expect_equal(s.paired$df, as.numeric(t.paired$parameter))
  expect_equal(s.paired$p.value, as.numeric(t.paired$p.value))
  expect_equal(xbar, as.numeric(t.paired$estimate))
  expect_equal(s.paired$mu0, as.numeric(t.paired$null.value))
  expect_equal(s.paired$stderr, as.numeric(t.paired$stderr))
  expect_equal(s.paired$alternative, as.character(t.paired$alternative))
  expect_equal(s.paired$method, as.character(t.paired$method))
})

test_that("Independent (not paired) setting: identical to t.test (equal variance)", {
  # Example from Vena02a, p. 117
  # library("BHH2")
  # data(shoes.data)
  # A <- shoes.data$matA
  # B <- shoes.data$matB
  A <- c(13.2, 8.2, 10.9, 14.3, 10.7, 6.6, 9.5, 10.8, 8.8, 13.3)
  B <- c(14, 8.8, 11.2, 14.2, 11.8, 6.4, 9.8, 11.3, 9.3, 13.6)
  t.indep <-
    t.test(
      x = A,
      y = B,
      var.equal = TRUE,
      paired = FALSE,
      alternative = "greater",
      conf.level = 0.95
    )
  xbar <- mean(A - B)
  n <- length(A) 
  sigma <- sqrt(mean(var(A), var(B)))
  s.indep <-
    spotSeverity(
      xbar = xbar,
      mu0 = 0,
      mu1 = 1,
      n = n,
      sigma = sigma,
      alpha = 0.025,
      tdist = TRUE,
      paired = FALSE
    )
  expect_equal(s.indep$statistic, as.numeric(t.indep$statistic), tolerance = 1e-2)
  expect_equal(s.indep$df, as.numeric(t.indep$parameter))
  expect_equal(s.indep$p.value, as.numeric(t.indep$p.value), tolerance = 1e-2)
  expect_equal(xbar, as.numeric( t.indep$estimate[1] - t.indep$estimate[2]))
  expect_equal(s.indep$mu0, as.numeric(t.indep$null.value))
  expect_equal(s.indep$stderr, as.numeric(t.indep$stderr), tolerance = 1e-1)
  expect_equal(s.indep$alternative, as.character(t.indep$alternative))
  expect_equal(s.indep$method, as.character(t.indep$method))
})


test_that("Power calculation", {
  # Example from Span19a, p. 582
  xbar <- 10
  n <- 100
  sigma <- 1
  mu0 = 10
  mu1 = 10.2
  alpha = 0.05
  tdist <- FALSE
  sev <-
    spotSeverity(
      xbar = xbar,
      mu0 = mu0,
      mu1 = mu1,
      n = n,
      sigma = sigma,
      alpha = alpha,
      tdist = tdist,
      paired = TRUE
    )
  expect_equal(sev$pow, 0.63876,tolerance = 1e-6 )
})

test_that("Power t-test calculation", {
  xbar <- 10
  n <- 100
  sigma <- 1
  mu0 = 10
  mu1 = 10.2
  alpha = 0.05
  tdist <- TRUE
  sev <-
    spotSeverity(
      xbar = xbar,
      mu0 = mu0,
      mu1 = mu1,
      n = n,
      sigma = sigma,
      alpha = alpha,
      tdist = tdist,
      paired = TRUE
    )
  expect_equal(sev$pow, as.double(
    power.t.test(
      n = n,
      delta = mu1 - mu0,
      alternative = "one.sided",
      type = "paired",
      sd = sigma,
      sig.level = alpha
    )$power
  ), tolerance = 1e-6)
  ##
  ncp <- (mu1 - mu0) / (sigma / sqrt(n))
  expect_equal(sev$pow, 1 - pt(qt(1 - alpha, df = n - 1), df = n - 1, ncp = ncp),
               tolerance = 1e-6)
})



