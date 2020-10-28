context("Select")

test_that("check select functions are evaluated without errors", {
    ## 
    ##  squared sum of zeros should be zero
    ## 
    n <- 100
    x <- matrix(1:(2*n),n,2)
    y <- matrix(rep(0, n),n,1)
    res <- selectAll(x,y)
    expect_equal( x , res$x)
    expect_equal( y , res$y)
    ##
    cList <- spotControl()
    cList$subsetControl <- list(N=1)
    res <- selectN(x=x,y=y,control = cList$subsetControl)
    expect_equal( matrix( c(100,200), 1, 2) , res$x)
    expect_equal( matrix( 0         , 1, 1) , res$y)
    }
)