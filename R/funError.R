#' @title funError
#' 
#' @description Simulate NAs, Infs, NaNs in results from objective function evaluations
#' 
#' @details Results from \code{\link{funSphere}} are replaced with \code{NA}, \code{NaN}, and \code{Inf}
#' values. 
#' 
#' @seealso \code{\link{is.finite}}
#'
#' @param x input vector or matrix of candidate solution
#' @param prob error probability (0<prob<1). Default: 0.1
#' @param errorList list with error types. Default: \code{list(NA, Inf, NaN)}
#' @param outDim dimension of the output matrix (number of columns)
#'
#' @return vector of objective function values
#' 
#' @examples 
#' set.seed(123)
#' require(SPOT)
#' x <- matrix(1:10, 5,2)
#' y <- funError(x)
#' any(is.na(y))
#' ## two-dim output
#' funError(x,outDim=2)
#' funError(x,outDim=2, prob=0.1)
#' 
#' 
#' @export
#' 
funError <- function(x, prob = 0.1, errorList = list(NA, Inf, NaN), outDim = 1){
  y <- matrix(NA, nrow=nrow(x), ncol=outDim)
  for (j in 1:outDim){
  y[,j] <- funSphere(x)
  for (errorType in errorList){
  errorMarker <- sample(c(TRUE, FALSE), nrow(x), replace = TRUE, prob = c(prob, 1-prob))
  if(sum(errorMarker) > 0){
    y[errorMarker,] <- errorType
  }
  }
}
  return(y)
}


