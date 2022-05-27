#' @title funNoise
#' 
#' @description noise function
#' 
#' 
#'
#' @param x input matrix of candidate solution
#' @param fun objective function. Default: \code{funSphere}
#' @param mean error mean. Default: 1
#' @param sigma error sd. Default: 1
#'
#' @return vector of noisy objective function values
#' 
#' @examples 
#' set.seed(123)
#' require(SPOT)
#' x <- matrix(1:10, 5,2)
#' funNoise(x)
#' 
#' @export
#' 
funNoise <- function(x, fun=funSphere, mean=0, sigma=1){
fun(x) + rnorm(dim(x)[1])
}


