context("Fun")

test_that("check test functions are evaluated without errors", {
    ## 
    ##  squared sum of zeros should be zero
    ## 
    n <- 100
    x <- matrix(rep(0, n),1,)
    y <- as.double( funSphere(x))
    expect_equal( y , 0)
    ##
    ##  squared sum of n ones should be n
    x <- matrix(rep(1, n),1,)
    y <- as.double( funSphere(x))
    expect_equal( y , n)
    
    ##  fun shifted sphere should have min at x=-a, i.e., f(-a) = 0
    a = 1.23
    x <- matrix(rep(a, n),1,)
    y <- as.double( funShiftedSphere(x, a))
    expect_equal( y , 0)
    
    ##
    ## Rosenbrock function: optimum at (1,1) with fmin = 0
    x <- matrix(rep(1, 2),1,)
    y <- as.double( funRosen(x))
    expect_equal( y , 0)
    ## Branin function: 3 optima at 
    ## x1 = (-pi,12.275), x2 = (pi, 2.275) and x3 = (9.42478, 2.475) 
    ## with fmin = 0.3978874
    ## Test for equality of y1 and y2:
    x1 <- matrix(c(-pi, 12.275),1,)
    y1 <- as.double( funBranin(x1))
    x2 <- matrix(c(pi, 2.275),1,)
    y2 <- as.double( funBranin(x2))
    expect_equal( y1 , y2)
    
    ## Eval rosen at (1,1):
    x1 <- matrix(c(1,1),1,)
    y <- as.double(funRosen(x1))
    expect_equal( y , 0)
    
})