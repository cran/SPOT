context("Duplicates and Replicates")

test_that("check duplicate and replicate handling", {
  k <- 2 ## k = problem dim
  n <- 3 ## row number (number of solutions)
  lower <- rep(-1, k)
  upper <- rep(1, k)
  
  control <- SPOT::spotControl(k)
  control$verbosity <- 0
  control$noise <- TRUE
  control$replicates <- 1
  ## 1st case: (x == xnew), new solutions were already evaluated, noise == TRUE
  
  ## 1st case: 1 replicate is required/allowed
    x <- matrix(1:(k*n),n,k, byrow = TRUE)
    # (1,2)
    # (3,4)
    # (5,6)
    xnew <- x
    znew <- duplicateAndReplicateHandling(xnew, x, lower, upper, control)
    ## znew should be identical to xnew (no changes):
    expect_equal(znew, xnew)
  ## 2nd case: now, 3 replicates are required/allowed
    control$replicates <- 3
    znew <- duplicateAndReplicateHandling(xnew, x, lower, upper, control)
    ## as in case 1: znew should be identical to xnew (no changes):
    expect_equal(znew, xnew)
  ## 3rd case: (x != xnew)
    control$replicates <- 3
    ## xnew is completely new and should be repeated 3 times:
    xnew <- matrix((k*n+1):(2*k*n),n,k, byrow = TRUE)
    znew <- duplicateAndReplicateHandling(xnew, x, lower, upper, control)
    ## matrices have the same elements, but in different orders,
    ## so we use sort() for testing of equality:
    expect_equal(sort(znew), sort(matrix(rep(xnew,3), ncol = k, byrow = TRUE)))
  ## 4th case: no noise
    control$noise <- FALSE
    xnew <- x
    znew <- duplicateAndReplicateHandling(xnew, x, lower, upper, control)
    ## values are NOT identical, but dimensions should be
    expect_equal(dim(znew), dim(xnew))
    }
)

test_that("Does apply(X, 1, identical, x) work as expected?",
{
  k <- 2 ## k = problem dim
  n <- 3 ## row number (number of solutions)
  A <- matrix(1:(k*n),n,k, byrow = TRUE)
  X <- rbind(A,A,A)
  U <- X[!duplicated(X), ]
  x <- A[1,]

  rowsInMatrix <- function(x, X){
      n <- dim(X)[1]
      r <- rep(0,n)
      for(i in (1:n)){
        if(identical(x,  X[i,])) r[i] <-1
      }
      return(r)
    }
  r1 <- rowsInMatrix(x,X)
  r2 <- apply(X, 1, identical, x)
  expect_equal(r1>0, r2)
 }
)    