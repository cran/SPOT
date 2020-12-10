#' @title diff0
#' 
#' @family 
#' spotTools
#'
#' @description 
#' Calculate differences 
#' @details 
#' Input vector length = output vector length
#'
#' @param x input vector
#' 
#' @return vector of differences
#' 
#' @examples
#' x <- 1:10
#' diff0(x)
#' @export
diff0 <- function(x){
  diff( c(0,x))
}
