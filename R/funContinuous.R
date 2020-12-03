#' @title funBranin
#'
#' @description Branin Test Function
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

#' funRosen2
#'
#' Rosenbrock Test Function (2-dim)
#'
#' @param x matrix of points to evaluate with the function. Rows for points and columns for dimension.
#'
#' @return 1-column matrix with resulting function values
#'
#' @examples
#' x1 <- matrix(c(-pi, 12.275),1,)
#' funRosen2(x1)
#'
#' @export
#' 
funRosen2 <- function (x) {
  matrix(apply(x, # matrix
               1, # margin (apply over rows)
               function(x){
                 100 * (x[2] - x[1] * x[1]) ^ 2 + (1 - x[1]) ^ 2  # objective function
               }
  ),
  , 1) # number of columns
}

#' funRosen
#'
#' Rosenbrock Test Function
#'
#' @param x matrix of points to evaluate with the function. Rows for points and columns for dimension.
#'
#' @return 1-column matrix with resulting function values
#' 
#' @references
#' More', J. J., Garbow, B. S., & Hillstrom, K. E. (1981).
#' Testing unconstrained optimization software.
#' \emph{ACM Transactions on Mathematical Software (TOMS)}, \emph{7}(1), 17-41.
#' \url{https://doi.org/10.1145/355934.355936}
#'
#' Rosenbrock, H. (1960).
#' An automatic method for finding the greatest or least value of a function.
#' \emph{The Computer Journal}, \emph{3}(3), 175-184.
#' \url{https://doi.org/10.1093/comjnl/3.3.175}
#'
#' @examples
#' x1 <- matrix(c(1,1),1,)
#' funRosen(x1)
#'
#' @export
#' 
funRosen <- function (x) {
  rosen <- function(par) {
        x1 <- par[1]
        x2 <- par[2]
        100 * (x2 - x1 * x1) ^ 2 + (1 - x1) ^ 2
      }
  matrix(apply(x, # matrix
               1, # margin (apply over rows)
               rosen),
  , 1) # number of columns
}

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
funSphere <- function (x) {
  matrix(apply(x, # matrix
               1, # margin (apply over rows)
               function(x){
                sum(x^2)  # objective function
               }
  ),
  , 1) # number of columns
}