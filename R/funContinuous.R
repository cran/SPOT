#' funBranin
#'
#' Branin Test Function
#'
#' @param x matrix of points to evaluate with the function. Rows for points and columns for dimension.
#'
#' @return 1-column matrix with resulting function values
#'
#' @examples
#' x1 <- matrix(c(-pi, 12.275),1,)
#' funBranin(x1)
#'
#' @export
#' 
funBranin <- function (x) {
  matrix(apply(x, # matrix
               1, # margin (apply over rows)
               function(x){
                 (x[2]  - 5.1/(4 * pi^2) * (x[1] ^2) + 5/pi * x[1]  - 6)^2 + 10 * (1 - 1/(8 * pi)) * cos(x[1] ) + 10  # objective function
               }
  ),
  , 1) # number of columns
}
###################################################################################################
#' funRosen
#'
#' Rosenbrock Test Function
#'
#' @param x matrix of points to evaluate with the function. Rows for points and columns for dimension.
#'
#' @return 1-column matrix with resulting function values
#'
#' @examples
#' x1 <- matrix(c(-pi, 12.275),1,)
#' funRosen(x1)
#'
#' @export
#' 
funRosen <- function (x) {
  matrix(apply(x, # matrix
               1, # margin (apply over rows)
               function(x){
                 100 * (x[2] - x[1] * x[1]) ^ 2 + (1 - x[1]) ^ 2  # objective function
               }
  ),
  , 1) # number of columns
}
###################################################################################################
#' funSphere
#'
#' Sphere Test Function
#'
#' @param x matrix of points to evaluate with the function. Rows for points and columns for dimension.
#'
#' @return 1-column matrix with resulting function values
#'
#' @examples
#' x1 <- matrix(c(-pi, 12.275),1,)
#' funSphere(x1)
#'
#' @export
#' 
###################################################################################################
funSphere <- function (x) {
  matrix(apply(x, # matrix
               1, # margin (apply over rows)
               function(x){
                sum(x^2)  # objective function
               }
  ),
  , 1) # number of columns
}