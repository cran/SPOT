#' @title makeSpotFunList
#'
#' @description Generate a list of spot functions
#'
#' @param vector2Matrix logical. Convert vector input to matrix.
#' Default: TRUE, so it can be used with \code{\link[stats]{optim}}.
#'
#' @return list of functions
#'
#' @importFrom stats optim
#'
#' @examples
#' fr <- makeSpotFunList()
#' optim(c(-1.2,1), fr[[1]])
#'
#' @export
#'
makeSpotFunList <- function(vector2Matrix = TRUE) {
  funList <- list()
  if (vector2Matrix == FALSE){
   funList[[1]] <- funSphere
    funList[[2]] <- funShiftedSphere
    funList[[3]] <- funBranin
    funList[[4]] <- funGoldsteinPrice
    funList[[5]] <- funSoblev99
    funList[[6]] <- funIshigami
return(funList)
  }
  
  funList[[1]] <- function(x) {
    funSphere(x = matrix(x, 1,))
  }
  funList[[2]] <- function(x) {
    funShiftedSphere(x = matrix(x, 1,),
                     a = 1)
  }
  funList[[3]] <- function(x) {
    funBranin(x = matrix(x, 1,))
  }
 
  funList[[4]] <- function(x) {
    funGoldsteinPrice(x = matrix(x, 1,))
  }
  funList[[5]] <- function(x) {
    funSoblev99(x = matrix(x, 1,))
  }
  funList[[6]] <- function(x) {
    funIshigami(x = matrix(x, 1,),
                a = 7,
                b = 0.1)
  }
  return(funList)
}

#' @title makeMoreFunList
#'
#' @description Generate a list of benchmark functions.
#' Based on the More(1981) paper.
#' Contains the first 13 function from the paper.
#' Function numbers are the same as in the paper.
#'
#' @param vector2Matrix logical. Convert vector input to matrix.
#' Default: TRUE, so it can be used with \code{\link[stats]{optim}}.
#'
#' @return list of functions with starting points and optimum points.
#' 
#' @references
#' More, J. J., Garbow, B. S., and  Hillstrom, K. E. (1981).
#' Testing unconstrained optimization software.
#' \emph{ACM Transactions on Mathematical Software (TOMS)}, \emph{7}(1), 17-41.
#' \doi{10.1145/355934.355936}
#'
#' @importFrom stats optim
#'
#' @examples
#' # Generate function list.
#' # Here we use the default setting \code{vector2Matrix = TRUE},
#' # so the function list can be passed to \code{\link[stats]{optim}}.
#' 
#' fl <- makeMoreFunList()
#' optim(par=c(-1.2,1), fn=fl$funList[[1]])
#' optim(par=fl$startPointList[[1]], fn=fl$funList[[1]])$value
#' optim(par=fl$startPointList[[1]], fn=fl$funList[[1]],NULL, method = "CG", hessian = FALSE)$value
#' optim(fl$startPointList[[1]], fl$funList[[1]],NULL, method = "BFGS", hessian = FALSE)$value
#' optim(fl$startPointList[[1]], fl$funList[[1]],NULL, method = "L-BFGS-B", hessian = FALSE)$value
#'
#' @export
#'
makeMoreFunList <- function(vector2Matrix = TRUE) {
  startPointList <- list( c(-1.2,1),
                          c(0.5, -2),
                          c(0,1),
                          c(1,1),
                          c(1,1),
                          c(0.3, 0.4),
                          c(-1,0,0),
                          c(1,1,1),
                          c(0.4,1,0),
                          c(0.02,4000,250),
                          c(5,2.5,0.15),
                          c(0,10,20),
                          c(3,-1,0,1)
                          )
  dimVec <- sapply(X=startPointList, FUN=length)
  #c(2,2,2,2,2,2,3,3,3,3,3,3,4)
  
  optimPointList <- list( c(1, 1),
                              c(5, 4),
                              c(1.098*1e-5, 9.106),
                              c(1e6, 2e-6),
                              c(3, 0.5),
                              c(0.2578, 0.2578), #m=10
                              c(1,0,0),
                              c(NA,NA,NA),
                              c(NA,NA,NA),
                              c(NA,NA,NA),
                              c(50, 25, 1.5),
                              c(1,10,1), # and: x_1=x_2 and x_3=0
                              c(0, 0, 0 )
  )
  
  
  funList <- list()
  if (vector2Matrix == FALSE){
    funList[[1]] <- funRosen
    funList[[2]] <- funFreudRoth
    funList[[3]] <- funPowellBs
    funList[[4]] <- funBrownBs
    funList[[5]] <- funBeale
    funList[[6]] <- funJennSamp
    funList[[7]] <- funHelical
    funList[[8]] <- funBard
    funList[[9]] <- funGauss
    funList[[10]] <- funMeyer
    funList[[11]] <- funGulf
    funList[[12]] <- funBox3d
    funList[[13]] <- funPowellS
    return(list(funList=funList,
                dimVec=dimVec,
                startPointList=startPointList,
                optimPointList=optimPointList))
  }
  
  funList[[1]] <- function(x) {
    funRosen(x = matrix(x, 1,))
  }
  funList[[2]] <- function(x) {
    funFreudRoth(x = matrix(x, 1,))
  }
  funList[[3]] <- function(x) {
    funPowellBs(x = matrix(x, 1,))
  }
  funList[[4]] <- function(x) {
    funBrownBs(x = matrix(x, 1,))
  }
  funList[[5]] <- function(x) {
    funBeale(x = matrix(x, 1,))
  }
  funList[[6]] <- function(x) {
    funJennSamp(x = matrix(x, 1,))
  }
  funList[[7]] <- function(x) {
    funHelical(x = matrix(x, 1,))
  }
  funList[[8]] <- function(x) {
    funBard(x = matrix(x, 1,))
  }
  funList[[9]] <- function(x) {
    funGauss(x = matrix(x, 1,))
  }
  funList[[10]] <- function(x) {
    funMeyer(x = matrix(x, 1,))
  }
  funList[[11]] <- function(x) {
    funGulf(x = matrix(x, 1,))
  }
  funList[[12]] <- function(x) {
    funBox3d (x = matrix(x, 1,))
  }
  funList[[13]] <- function(x) {
    funPowellS(x = matrix(x, 1,))
  }
  return(list(funList=funList,
              dimVec=dimVec,
              startPointList=startPointList,
              optimPointList=optimPointList))
}



#' @title funBranin (No. 1)
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

#' @title  funRosen2 (No. 2a)
#'
#' @description  Rosenbrock Test Function (2-dim)
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

#' @title funRosen (No. 2, More No. 1)
#'
#' @description  Rosenbrock Test Function
#'
#' @param x matrix of points to evaluate with the function. Rows for points and columns for dimension.
#'
#' @return 1-column matrix with resulting function values
#'
#' @references
#' More, J. J., Garbow, B. S., and  Hillstrom, K. E. (1981).
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

#' @title funSphere (No. 3)
#'
#' @description  Sphere Test Function
#'
#' @param x matrix of points to evaluate with the function. Rows for points and columns for dimension.
#'
#' @seealso \code{\link{funShiftedSphere}}
#'
#' @return 1-column matrix with resulting function values
#'
#' @examples
#' x1 <- matrix(c(-pi, 12.275),1,)
#' funSphere(x1)
#'
#' @export
#'
funSphere <- function(x) {
  matrix(apply(x, # matrix
               1, # margin (apply over rows)
               function(x) {
                 sum(x ^ 2)  # objective function
               }),
         , 1) # number of columns
}

#' @title funShiftedSphere (No. 4)
#'
#' @description  Shifted Sphere Test Function with optimum at x_opt = a and f(x_opt) = 0
#'
#' @seealso \code{\link{funSphere}}
#'
#' @param x matrix of points to evaluate with the function. Rows for points and columns for dimension.
#'
#' @param a offset added, i.e., f = sum (x-a)^2. Default: \code{1}.
#'
#' @return 1-column matrix with resulting function values
#'
#' @examples
#' x1 <- matrix(c(-pi, 12.275),1,)
#' a <- 1
#' funShiftedSphere(x1, a)
#'
#' @export
#'
funShiftedSphere <- function (x, a = 1) {
  f <- function(x, a) {
    sum((x - a) ^ 2)  # objective function
  }
  matrix(apply(x, # matrix
               1, # margin (apply over rows)
               f,
               a),
         , 1) # number of columns
}


#' @title Goldstein-Price Test Function (No. 5)
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

#' @title Sobol and Levitan Test Function (No. 6)
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
                        b = c(rep(0.6, 10),
                              rep(0.4, 10)),
                        c0 = 0)
{
  d <- dim(x)[2]
  db <- length(b)
  
  if (d > db) {
    print(d)
    print(db)
    stop('Stoped because value of the d-dimensional vector b is missing or wrong dimension for b.')
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

#' @title Ishigami Test Function (No. 7)
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
#' @param x (\code{m,3})-matrix of points to evaluate with the function.
#' Values should be >= -pi and <= pi, i.e., x_i in [-pi,pi].
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
                 sin(x1) + a * (sin(x2)) ^ 2 +  b * x3 ^ 4 * sin(x1)
               }),
         , 1) # number of columns
}

#' @title funFreudRoth (No. 8, More No. 2)
#'
#' @description  2-dim Freudenstein and Roth Test Function
#'
#' @param x matrix of points to evaluate with the function.
#' Rows for points and columns for dimension.
#'
#' @return 1-column matrix with resulting function values
#'
#' @references
#' More, J. J., Garbow, B. S., and  Hillstrom, K. E. (1981).
#' Testing unconstrained optimization software.
#' \emph{ACM Transactions on Mathematical Software (TOMS)}, \emph{7}(1), 17-41.
#' \doi{10.1145/355934.355936}
#'
#' B. Freudenstein, F., and Roth, B. (Oct. 1963).
#' Numerical solutions of systems of nonlinear equations.
#' \emph{The ACM Journal}, \emph{3}(3), 550-556.
#' \url{https://dl.acm.org/doi/10.1145/321186.321200}
#'
#' @examples
#' x1 <- matrix(c(1,1),1,)
#' funFreudRoth(x1)
#'
#' # Running SPOT with 20 function evaluations with default configurations
#' res <- spot(,funFreudRoth,c(0,0),c(10,10),control=list(funEvals=20))
#' plotModel(res$model)
#'
#' @export
#'
funFreudRoth <- function (x) {
  freudRoth <- function(par) {
    x <- par[1]
    y <- par[2]
    
    f1 <- -13 + x + ((5 - y) * y - 2) * y
    f2 <- -29 + x + ((y + 1) * y - 14) * y
    
    return(f1 * f1 + f2 * f2)
  }
  matrix(apply(x, # matrix
               1, # margin (apply over rows)
               freudRoth),
         , 1) # number of columns
}

#' @title funPowellBs (No. 9, More No. 3)
#'
#' @description  2-dim Powell Badly Scaled Test Function
#'
#' @param x matrix of points to evaluate with the function.
#' Rows for points and columns for dimension.
#'
#' @return 1-column matrix with resulting function values
#'
#' @references
#' More, J. J., Garbow, B. S., and  Hillstrom, K. E. (1981).
#' Testing unconstrained optimization software.
#' \emph{ACM Transactions on Mathematical Software (TOMS)}, \emph{7}(1), 17-41.
#' \doi{10.1145/355934.355936}
#'
#' Powell, M.J.D. (1970).
#' A hybrid method for nonlinear equations.
#' In Numerical methods for Nonlinear Algebraic Equations,
#' P. Rabinowitz (Ed),
#' \emph{Gordon & Breach, New York.}, \emph{3}(3), 87-114.
#'
#' @examples
#' x1 <- matrix(c(-1,1),1,)
#' funPowellBs(x1)
#'
#' # Running SPOT with 20 function evaluations with default configurations
#' res <- spot(,fun=funPowellBs,c(-10,-10),c(10,10),control=list(funEvals=20))
#' plotModel(res$model, points = rbind(c(res$xbest[1], res$xbest[2]),c(1.098e-5,9.106)))
#'
#' @export
#'

funPowellBs <- function (x) {
  powellBs <- function(par) {
    x <- par[1]
    y <- par[2]
    
    f1 <- 1e4 * x * y - 1
    f2 <- exp(-x) + exp(-y) - 1.0001
    
    return(f1 * f1 + f2 * f2)
  }
  matrix(apply(x, # matrix
               1, # margin (apply over rows)
               powellBs),
         , 1) # number of columns
}

#' @title funbrownBs (No. 10, More No. 4)
#'
#' @description  2-dim Brown badly scaled Test Function
#'
#' @details
#' n=2, m=3
#' x0 = (1,1)
#' f=0 at (1e6, 2e-6)
#'
#' @param x matrix of points to evaluate with the function.
#' Rows for points and columns for dimension.
#'
#' @return 1-column matrix with resulting function values
#'
#' @references
#' More, J. J., Garbow, B. S., and  Hillstrom, K. E. (1981).
#' Testing unconstrained optimization software.
#' \url{https://www.osti.gov/servlets/purl/6650344}
#'
#'
#' @examples
#' x1 <- matrix(c(1,1),1,)
#' funBrownBs(x1)
#'
#' res <- spot(,fun=funBrownBs,c(-10,-10),c(10,10),control=list(funEvals=20))
#' plotModel(res$model, points = rbind(c(res$xbest[1], res$xbest[2]),c(1.098e-5,9.106)))
#'
#' @export
#'
funBrownBs <- function (x) {
  brownBs <- function(par) {
    x <- par[1]
    y <- par[2]
    f1 <- x - 1e6
    f2 <- y - 2e-6
    f3 <- x * y - 2
    
    return(f1 * f1 + f2 * f2 + f3 * f3)
  }
  matrix(apply(x, # matrix
               1, # margin (apply over rows)
               brownBs),
         , 1) # number of columns
}

#' @title funBeale (No.11, More No. 5)
#'
#' @description  2-dim Beale Test Function
#'
#' @param x matrix of points to evaluate with the function.
#' Rows for points and columns for dimension.
#'
#' @return 1-column matrix with resulting function values
#'
#' @references
#' Beale, E.M.L. On an interactive method of finding a local minimum of a function of more than
#' one variable. Tech. Rep. No. 25, Statistical Techniques Research Group, Princeton Univ.,
#' Princeton, N.J., 1958.
#'
#' Rosenbrock, H. (1960).
#' An automatic method for finding the greatest or least value of a function.
#' \emph{The Computer Journal}, \emph{3}(3), 175-184.
#' \doi{10.1093/comjnl/3.3.175}
#'
#' @examples
#' x1 <- matrix(c(1,1),1,)
#' funBeale(x1)
#'
#' res <- spot(,funBeale,c(1,-1),c(5,2),control=list(funEvals=15))
#' plotModel(res$model)
#'
#' @export
#'
funBeale <- function (x) {
  beale <- function(par) {
    x <- par[1]
    y <- par[2]
    yy <- y * y
    yyy <- yy * y
    
    f1 <- 1.5 - x * (1 - y)
    f2 <- 2.25 - x * (1 - yy)
    f3 <- 2.625 - x * (1 - yyy)
    
    return(f1 * f1 + f2 * f2 + f3 * f3)
  }
  matrix(apply(x, # matrix
               1, # margin (apply over rows)
               beale),
         , 1) # number of columns
}

#' @title funJennSamp (No. 12, More No 6)
#'
#' @description 2-dim Jennrich and Sampson Function Test Function
#'
#' @param x matrix of points to evaluate with the function.
#' Rows for points and columns for dimension.
#'
#' @return 1-column matrix with resulting function values
#'
#' @references
#' More, J. J., Garbow, B. S., & Hillstrom, K. E. (1981).
#' Testing unconstrained optimization software.
#' \emph{ACM Transactions on Mathematical Software (TOMS)}, \emph{7}(1), 17-41.
#' \doi{10.1145/355934.355936}
#'
#' Jennrich, R.I., and Sampson (1968).
#' Application of stepwise regression to nonlinear estimation.
#' \emph{Technometrics}, \emph{3}(3), 63-72.
#' \url{https://www.tandfonline.com/doi/abs/10.1080/00401706.1968.10490535}
#'
#' @examples
#' x1 <- matrix(c(1,1),1,)
#' funJennSamp(x1)
#'
#' res <- spot(,funJennSamp,c(0,0),c(0.3,0.3))
#' plotModel(res$model)
#'
#' @export
#'
funJennSamp <- function (x) {
  jennSamp <- function(par, ...) {
    if (!exists("m")) {
      m = 10
    }
    if (m < 2) {
      stop("Jennrich-Sampson: m must be >= 2")
    }
    x <- par[1]
    y <- par[2]
    
    fsum <- 0
    for (i in 1:m) {
      fi <- 2 + 2 * i - (exp(i * x) + exp(i * y))
      fsum <- fsum + fi * fi
    }
    fsum
  }
  matrix(apply(x, # matrix
               1, # margin (apply over rows)
               jennSamp),
         , 1) # number of columns
}


#' @title funHelical (No. 13, More No. 7)
#'
#' @description  3-dim Helical Test Function
#'
#' @param x matrix (n x 3)-dim of points to evaluate with the function.
#' Rows for points and columns for dimension.
#'
#' @return 1-column matrix with resulting function values
#'
#' @references
#' More', J. J., Garbow, B. S., and  Hillstrom, K. E. (1981).
#' Testing unconstrained optimization software.
#' \emph{ACM Transactions on Mathematical Software (TOMS)}, \emph{7}(1), 17-41.
#' \doi{10.1145/355934.355936}
#'
#' Fletcher, R., and  Powell, M. J. (1963).
#' A rapidly convergent descent method for minimization.
#' \emph{The Computer Journal}, \emph{6}(2), 163-168.
#' \doi{10.1093/comjnl/6.2.163}
#'
#' @examples
#' x1 <- matrix(c(1,1,1),1,)
#' funHelical(x1)
#' res <- spot(,funHelical,c(-40,-40,-40),c(40,40,40),control=list(funEvals=20))
#' plotModel(res$model,which=c(1,2),type="persp",border="NA")
#' plotModel(res$model,which=c(2,3),type="persp",border="NA")
#' plotModel(res$model,which=c(1,3),type="persp",border="NA")
#' plotModel(res$model, which=c(1,2))
#' plotModel(res$model, which=c(1,3))
#' plotModel(res$model, which=c(2,3))
#'
#' @export
#'

funHelical <- function(x) {
  helical <- function(par) {
    one_div_2pi <- 0.5 / pi
    
    theta <- function(x1, x2) {
      res <- one_div_2pi * atan(x2 / x1)
      if (x1 < 0) {
        res <- res + 0.5
      }
      res
    }
    x <- par[1]
    y <- par[2]
    z <- par[3]
    f1 <- 10 * (z - 10 * theta(x, y))
    f2 <- 10 * (sqrt(x * x + y * y) - 1)
    f3 <- z
    f1 * f1 + f2 * f2 + f3 * f3
  }
  matrix(apply(x, # matrix
               1, # margin (apply over rows)
               helical),
         , 1) # number of columns
}

#' @title funBard (No. 14, More No. 8)
#'
#' @description  3-dim Bard Test Function
#'
#' x0 = (1,1,1)
#' f = 8.21487...1e-3
#' f = 17.4286... at (0.8406..., -infty, -infty)
#'
#' @param x matrix of points to evaluate with the function.
#' Rows for points and columns for dimension.
#'
#' @return 1-column matrix with resulting function values
#'
#' @references
#' More, J. J., Garbow, B. S., and  Hillstrom, K. E. (1981).
#' Testing unconstrained optimization software.
#' \emph{ACM Transactions on Mathematical Software (TOMS)}, \emph{7}(1), 17-41.
#' \doi{10.1145/355934.355936}
#'
#' BARD, Y. Comparison of gradient methods for the solution of nonlinear parameter estimation
#' problems SIAM J. Numer. Anal. 7 (1970), 157-186.
#'
#'
#' @examples
#' x1 <- matrix(c(1,1),1,)
#' funBard(x1)
#'
#' @export
#'
funBard <- function (x) {
  bard <- function(par) {
    y <- c(0.14,
           0.18,
           0.22,
           0.25,
           0.29,
           0.32,
           0.35,
           0.39,
           0.37,
           0.58,
           0.73,
           0.96,
           1.34,
           2.10,
           4.39)
    m <- 15
    x1 <- par[1]
    x2 <- par[2]
    x3 <- par[3]
    
    fsum <- 0
    for (u in 1:m) {
      v <- 16 - u
      w <- min(u, v)
      f <- y[u] - (x1 + u / (v * x2 + w * x3))
      fsum <- fsum + f * f
    }
    return(fsum)
  }
  matrix(apply(x, # matrix
               1, # margin (apply over rows)
               bard),
         , 1) # number of columns
}

#' @title funGauss (No. 15, More No. 9)
#'
#' @description  3-dim Gaussian Test Function
#'
#' @param x matrix of points to evaluate with the function.
#' Rows for points and columns for dimension.
#'
#' @return 1-column matrix with resulting function values
#'
#' @references
#' Unpublished
#'
#' @examples
#' x1 <- matrix(c(1,1,1),1,)
#' funGauss(x1)
#'
#' res1 <- spot(,funGauss,
#'   c(-0.001,-0.007,-0.003),
#'   c(0.5,1.0,1.1),
#'   control=list(funEvals=15))
#'   plotModel(res1$model, which = 1:2)
#'
#' @export
#'
funGauss <- function (x) {
  gauss <- function(par) {
    y <-
      c(
        0.0009,
        0.0044,
        0.0175,
        0.0540,
        0.1295,
        0.2420,
        0.3521,
        0.3989,
        0.3521,
        0.2420,
        0.1295,
        0.0540,
        0.0175,
        0.0044,
        0.0009
      )
    m <- 15
    x1 <- par[1]
    x2 <- par[2]
    x3 <- par[3]
    
    fsum <- 0
    for (i in 1:m) {
      ti <- (8 - i) * 0.5
      f <- x1 * exp(-0.5 * x2 * (ti - x3) ^ 2) - y[i]
      fsum <- fsum + f * f
    }
    return(fsum)
  }
  matrix(apply(x, # matrix
               1, # margin (apply over rows)
               gauss),
         , 1) # number of columns
}


#' @title funMeyer (No. 16, More No. 10)
#'
#' @description  Meyer 3-dim Test Function
#'
#' @param x matrix (dim 1x3) of points to evaluate with the function.
#' Rows for points and columns for dimension.
#'
#' @return 1-column matrix with resulting function values
#'
#' @references
#' More, J. J., Garbow, B. S., and Hillstrom, K. E. (1981).
#' Testing unconstrained optimization software.
#' \emph{ACM Transactions on Mathematical Software (TOMS)}, \emph{7}(1), 17-41.
#' \doi{10.1145/355934.355936}
#'
#'
#' @examples
#' x1 <- matrix(c(1,1,1),1,)
#' funMeyer(x1)
#'
#' set.seed(13)
#' resMeyer <- spot(matrix(c(0.02,4000,250),1,3),
#'   funMeyer,c(0,1000,200),c(3,8000,500),
#'   control= list(funEvals=15))
#' resMeyer$xbest
#' resMeyer$ybest
#' print("Model with parameters")
#' plotModel(resMeyer$model)
#' plotModel(resMeyer$model,which=2:3)
#'
#' @export
#'
funMeyer <- function (x) {
  meyer <- function(par) {
    y <-
      c(
        34780,
        28610,
        23650,
        19630,
        16370,
        13720,
        11540,
        9744,
        8261,
        7030,
        6005,
        5147,
        4427,
        3820,
        3307,
        2872
      )
    m <- 16
    x1 <- par[1]
    x2 <- par[2]
    x3 <- par[3]
    ti <- 45 + 5 * (1:m)
    fi <- x1 * exp(x2 / (ti + x3)) - y
    
    return(sum(fi * fi))
    
  }
  matrix(apply(x, # matrix
               1, # margin (apply over rows)
               meyer),
         , 1) # number of columns
}

#' @title funGulf (No.17, More No. 11)
#'
#' @description 3-dim Gulf research and development Test Function
#'
#' @param x matrix (n x 3) of points to evaluate with the function. 
#' Rows for points and columns for dimension.
#' Values should be larger than 0.
#'
#' @param m  additional parameter: .
#' The Gulf function supports an additional parameter m in the range from 3 to 100
#'
#' @return 1-column matrix with resulting function values
#'
#' @references
#' More, J. J., Garbow, B. S., and  Hillstrom, K. E. (1981).
#' Testing unconstrained optimization software.
#' \emph{ACM Transactions on Mathematical Software (TOMS)}, \emph{7}(1), 17-41.
#' \doi{10.1145/355934.355936}
#'
#' @examples
#' x1 <- matrix(c(50,25,1.5),1,)
#' funGulf(x1)
#'
#' funGulf(x1,m=50)
#'
#' resGulf <- spot(,funGulf,c(0,0,0),c(100,50,5))
#' resGulf$xbest
#' resGulf$ybest
#' plotModel(resGulf$model, which=1:2)
#' plotModel(resGulf$model, which=2:3)
#'
#' # x0 is an optional start point (or set of start points), specified as a matrix.
#' # One row for each point, and one column for each optimized parameter.
#' x0 = matrix(c(5,2.5,0.15),1,3)
#' resGulf <- spot(x0,funGulf,c(0,0,0),c(100,50,5))
#' resGulf$xbest
#' resGulf$ybest
#'
#' @export
#'
funGulf <- function (x, m = 99) {
  gulf <- function(par, m) {
    ## m can be between 3 and 100
    if (m < 3 || m > 100) {
      stop("Gulf research and development function: m must be between 3 and 100")
    }
    #It looks like a copy paste from the previous function.
    #       y <- c(34780, 28610, 23650, 19630, 16370, 13720, 11540, 9744, 8261, 7030,
    #              6005, 5147, 4427, 3820, 3307, 2872)
    p66 <- 2 / 3
    
    x1 <- par[1]
    x2 <- par[2]
    x3 <- par[3]
    
    ti <- 1:m * 0.01
    y <- 25 + (-50 * log(ti)) ^ p66
    fi <- exp(-(abs(x2 - y) ^ x3) / x1) - ti
    return(sum(fi * fi))
  }
  
  
  matrix(apply(x, # matrix
               1, # margin (apply over rows)
               gulf, m),
         , 1) # number of columns
}

#' @title funbox3D (No. 18, More No. 12)
#'
#' @description Box three-dimensional Test Function
#'
#' @param x matrix of points to evaluate with the function.
#' Rows for points and columns for dimension.
#'
#' @return 1-column matrix with resulting function values
#'
#' @references
#' More, J. J., Garbow, B. S., and  Hillstrom, K. E. (1981).
#' Testing unconstrained optimization software.
#' \emph{ACM Transactions on Mathematical Software (TOMS)}, \emph{7}(1), 17-41.
#' \doi{10.1145/355934.355936}
#'
#' Box three - dimensional, (1966).
#' A comparison of several current optimization methods, and the use of transformations
#'  in constrained problems.
#' \emph{The Computer Journal}, \emph{3}(3), 66-77.
#' \url{https://academic.oup.com/comjnl/article/9/1/67/348150}
#'
#'  @examples
#' x <- matrix(c(1,10,1),1,)
#' funBox3d(x)
#'
#' res <- spot(,funBox3d,c(5,15,-5),c(15,5,5),control=list(funEvals=20))
#' # plotting the graphs
#' plotModel(res$model,which=1:2)
#' plotModel(res$model,which=2:3)
#' plotModel(res$model,which=c(1,3))
#'
#' @export
#'
funBox3d <- function (x) {
  box3d <- function(par, ...) {
    if (!exists("m")) {
      m = 20
    }
    if (m < 3) {
      stop("box3d: m must be >= 3")
    }
    
    x1 <- par[1]
    x2 <- par[2]
    x3 <- par[3]
    
    fsum <- 0
    for (i in 1:m) {
      ti <- 0.1 * i
      fi <-
        exp(-ti * x1) - exp(-ti * x2) - x3 * (exp(-ti) - exp(-i))
      fsum <- fsum + fi * fi
    }
    return(fsum)
  }
  
  matrix(apply(x, # matrix
               1, # margin (apply over rows)
               box3d),
         , 1) # number of columns
}


#' @title funPowellS (No. 19, More No. 13)
#'
#' @description  Powells 4-dim Test Function
#'
#' @param x matrix (dim 1x4) of points to evaluate with the function.
#' Rows for points and columns for dimension.
#'
#' @return 1-column matrix with resulting function values
#'
#' @references
#' More, J. J., Garbow, B. S., and  Hillstrom, K. E. (1981).
#' Testing unconstrained optimization software.
#' Trond Steihaug and Sara Suleiman
#' Global convergence and the Powell singular function
#' \emph{ACM Transactions on Mathematical Software (TOMS)}, \emph{7}(1), 17-41.
#' \doi{10.1145/355934.355936}
#' \url{http://owos.gm.fh-koeln.de:8055/bartz/optimization-ait-master-2020/blob/master/Jupyter.d/Exercise-VIIa.ipynb}
#' \url{http://bab10.bartzandbartz.de:8033/bartzbeielstein/bab-optimization-ait-master-2020/-/blob/master/Jupyter.d/01spotNutshell.ipynb}
#' \url{https://www.mat.univie.ac.at/~neum/glopt/bounds.html}
#'
#'
#' Powells Test function, M. J. D. Powell, 1962
#' An automatic method for finding the local minimum of a function.
#' \emph{The Computer Journal}, \emph{3}(3), 175-184.
#' \url{https://www.sfu.ca/~ssurjano/powell.html}
#'
#' @examples
#' x1 <- matrix(c(0,0,0,0),1,)
#' funPowellS(x1)
#' x2 <- matrix(c(3,-1,0,1),1,)
#' funPowellS(x2)
#' x3 <- matrix(c(0,0,0,-2),1,)
#' funPowellS(x3)
#' # optimization run with SPOT and 15 evaluations
#' res_fun <- spot(,funPowellS,c(-4,-4,-4,-4 ),c(5,5,5,5),control=list(funEvals=15))
#' res_fun
#'
#' @export
#'
funPowellS <- function(x) {
  powellS <- function(par) {
    x1 <- par[1]
    x2 <- par[2]
    x3 <- par[3]
    x4 <- par[4]
    
    x14 <- x1 - x4
    x14s <- x14 * x14
    f1 <- x1 + 10 * x2
    f2s <- 5 * (x3 - x4) ^ 2
    f3 <- (x2 - 2 * x3) ^ 2
    f4s <- 10 * x14s * x14s
    return(f1 * f1 + f2s + f3 * f3 + f4s)
  }
  matrix(apply(x, # matrix
               1, # margin apply over rows
               powellS),
         , 1) # number of columns
}
