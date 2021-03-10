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
               function(x) {
                 (x[2]  - 5.1 / (4 * pi ^ 2) * (x[1] ^ 2) + 5 / pi * x[1]  - 6) ^ 2 + 10 * (1 - 1 /
                                                                                              (8 * pi)) * cos(x[1]) + 10  # objective function
               }),
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
               function(x) {
                 100 * (x[2] - x[1] * x[1]) ^ 2 + (1 - x[1]) ^ 2  # objective function
               }),
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
#' \doi{10.1145/355934.355936}
#'
#' Rosenbrock, H. (1960).
#' An automatic method for finding the greatest or least value of a function.
#' \emph{The Computer Journal}, \emph{3}(3), 175-184.
#' \doi{10.1093/comjnl/3.3.175}
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
               function(x) {
                 sum(x ^ 2)  # objective function
               }),
         , 1) # number of columns
}


#' @title Goldstein-Price Test Function
#'
#' @description  An implementation of Booker et al.’s method on
#' a re-scaled/coded version of the 2-dim Goldstein–Price function
#'
#' @param x (\code{m,2})-matrix of points to evaluate with the function.
#' Rows for points and columns for dimension.
#'
#' @return 1-column matrix with resulting function values
#'
#' @examples
#' x1 <- matrix(c(-pi, 12.275),1,)
#' funGoldsteinPrice(x1)
#'
#' @export
#'
funGoldsteinPrice <- function (x) {
  matrix(apply(x, # matrix
               1, # margin (apply over rows)
               function(par) {
                 m <- 8.6928
                 s <- 2.4269
                 x1 <- 4 * par[1] - 2
                 x2 <- 4 * par[2] - 2
                 a <- 1 + (x1 + x2 + 1) ^ 2 *
                   (19 - 14 * x1 + 3 * x1 ^ 2 - 14 * x2 + 6 * x1 * x2 + 3 *
                      x2 ^ 2)
                 b <- 30 + (2 * x1 - 3 * x2) ^ 2 *
                   (18 - 32 * x1 + 12 * x1 ^ 2 + 48 * x2 - 36 * x1 * x2 + 27 *
                      x2 ^ 2)
                 f <- log(a * b)
                 (f - m) / s
               }),
         , 1) # number of columns
}

#' @title Sobol and Levitan Test Function
#'
#' @description  An implementation of the Sobol-Levitan function. 
#' 
#' f(x) = exp(sum b_i x_i) - I_d + c_0, where
#'        I_d = prod( (exp(b_i) -1) / b_i) 
#' 
#' The value of the elements in the b-vector (b1, ..., bd) affect the importance 
#' of the corresponding x-variables.
#' Sobol' & Levitan (1999) use two different b-vectors:
#'   (1.5, 0.9, 0.9, 0.9, 0.9, 0.9), for d = 6, and
#'   (0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.4, 0.4, 0.4, 0.4, 0.4,
#'    0.4, 0.4, 0.4, 0.4, 0.4), for d = 20. 
#' Our implementation uses the default b vector:
#'  b = c(0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 
#'      0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4) (when d<=20).    
#'   
#' Moon et al. (2012) scale the output to have a variance of 100. 
#' For d = 20, they use three different b-vectors:
#'   (2, 1.95, 1.9, 1.85, 1.8, 1.75, 1.7, 1.65, 0.4228, 0.3077, 0.2169, 0.1471, 
#'    0.0951, 0.0577, 0.0323, 0.0161, 0.0068, 0.0021, 0.0004, 0),
#'   (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), and
#'   (2.6795, 2.2289, 1.8351, 1.4938, 1.2004, 0.9507, 0.7406, 0.5659, 0.4228, 
#'    0.3077, 0.2169, 0.1471, 0.0951, 0.0577, 0.0323, 0.0161, 0.0068, 0.0021, 0.0004, 0). 
#' 
#' The generally used value of c0 is c0 = 0. 
#' The function is evaluated on xi in [0, 1], for all i = 1, ..., d. 
#' 
#' @references 
#' Moon, H., Dean, A. M., & Santner, T. J. (2012). 
#' Two-stage sensitivity-based group screening in computer experiments. 
#' Technometrics, 54(4), 376-387.
#' 
#' Sobol', I. M., & Levitan, Y. L. (1999). On the use of variance reducing 
#' multipliers in Monte Carlo computations of a global sensitivity index. 
#' Computer Physics Communications, 117(1), 52-61.
#' 
#' @param x (\code{m,2})-matrix of points to evaluate with the function.
#' Values should be >= 0 and <= 1, i.e., x_i in [0,1]. 
#' 
#' @param b d-dimensional vector (optional), with default value
#'      b = c(0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 
#'      0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4) (when d<=20)
#'      
#' @param c0 constant term (optional), with default value 0          
#'
#' @return 1-column matrix with resulting function values
#'
#' @examples
#' x1 <- matrix(c(-pi, 12.275),1,)
#' funSoblev99(x1)
#'
#' @export
#'
funSoblev99 <- function(x,
                        b = c(rep(0.6,10),
                              rep(0.4,10)),
                        c0 = 0)
{
  d <- length(x)
  db <- length(b)
  
  if (d > db) {
    stop('Value of the d-dimensional vector b is required.')
  }
  matrix(apply(x, # matrix
               1, # margin (apply over rows)
               function(par) {
                 Id <- 1
                 for (ii in 1:d) {
                   bi  <- b[ii]
                   new <- (exp(bi) - 1) / bi
                   Id  <- Id * new
                 }
                 sum <- 0
                 for (ii in 1:d) {
                   bi  <- b[ii]
                   xi  <- par[ii]
                   sum <- sum + bi * xi
                 }
                exp(sum) - Id + c0
               }),
         , 1) # number of columns
}

#' @title Ishigami Test Function
#'
#' @description  An implementation of the 3-dim Ishigami function. 
#' 
#' f(x) = sin(x_1) + a sin^2(x_2) + b x_3^4sin(x_1)
#' 
#' The Ishigami function of Ishigami & Homma (1990) is used as an example for 
#' uncertainty and sensitivity analysis methods, 
#' because it exhibits strong nonlinearity and nonmonotonicity. 
#' It also has a peculiar dependence on x_3, as described by Sobol' & Levitan (1999). 
#' The independent distributions of the input random variables are usually: 
#' x_i ~ Uniform[-pi, pi ], for all i = 1, 2, 3.
#' 
#' @references 
#' Ishigami, T., & Homma, T. (1990, December). 
#' An importance quantification technique in uncertainty analysis for computer models. 
#' In Uncertainty Modeling and Analysis, 1990. Proceedings., 
#' First International Symposium on (pp. 398-403). IEEE.
#' 
#' Sobol', I. M., & Levitan, Y. L. (1999). On the use of variance reducing 
#' multipliers in Monte Carlo computations of a global sensitivity index. 
#' Computer Physics Communications, 117(1), 52-61.
#' 
#' @param x (\code{m,2})-matrix of points to evaluate with the function.
#' Values should be >= 0 and <= 1, i.e., x_i in [0,1]. 
#' 
#' @param a coefficient (optional), with default value 7
#' @param b coefficient (optional), with default value 0.1  
#' 
#' @return 1-column matrix with resulting function values
#'
#' @examples
#' x1 <- matrix(c(-pi, 0, pi),1,)
#' funIshigami(x1)
#'
#' @export
#'
funIshigami <- function(x,
                        a = 7,
                        b = 0.1)
{
  matrix(apply(x, # matrix
               1, # margin (apply over rows)
               function(par) {
                 x1 <- x[1]
                 x2 <- x[2]
                 x3 <- x[3]
                 sin(x1) + a * (sin(x2))^2 +  b * x3^4 * sin(x1)
               }),
         , 1) # number of columns
}



