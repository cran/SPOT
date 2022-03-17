#' funMultiObjectiveOptimization 
#'
#' A multiobjective dummy testfunction 
#'
#' @title funMoo 
#'
#' @description  Multi-objective Test Function
#'
#' @param x matrix of points to evaluate with the function. Rows for points and columns for dimension.
#'
#'
#' @return n-column matrix with resulting function values
#'
#' @examples
#' x1 <- matrix(c(-pi, 12.275),1,)
#' funMoo(x1)
#' x <- matrix(c(1,1,2), ncol=3 )
#  funMoo(x)
#'
#' @export
#'
funMoo <- function(x) {
  matrix(apply(x, # matrix
               1, # margin (apply over rows)
               function(x) {
                 y <- matrix(c(sum(x),-sum(x)), ncol=2 ) 
                 return(y)
               }),
         , 2) # number of columns
}

