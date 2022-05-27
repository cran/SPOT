#' @title runOptim
#'
#' @description Run \code{\link[stats]{optim}} on a list of spot benchmark functions
#'
#' @param fl function list. Generated with one of the function list 
#' generators in \code{spot}, e.g., \code{\link{makeSpotFunList}} or 
#' \code{\link{makeMoreFunList}}. Default: \code{\link{makeMoreFunList}}.
#' 
#' @param method The method used by \code{\link[stats]{optim}}: 
#' \code{"Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", or "Brent"}.
#' Default: \code{"Nelder-Mead"}.
#' 
#' @param n repeats. If \code{n >1 }, different start points (randomized) 
#' will be used. Default: \code{n=2}.
#' 
#' @param k subset of benchmark functions.
#' Default: \code{1:length(makeMoreFunList()$funList)},
#' i.e., all implemented functions.
#' 
#' @param verbosity Level 0 shows no output (default). 
#'
#' @return res. data.frame with results: \code{c("f", "r", "y")}
#'
#' @importFrom stats optim
#'
#' @examples
#' summary(runOptim(k=1)$y)
#' summary(runOptim(k=1, method="CG")$y)
#'
#' @export
#'
runOptim <- function(fl = makeMoreFunList(),
                     method="Nelder-Mead",
                     n=2,
                     k = 1:length(makeMoreFunList()$funList),
                     verbosity = 0){
  ## k number of functions
  res <- data.frame()
  for (j in k){
    dim <- fl$dimVec[j]
    for (i in 1:n) {
      set.seed(i)
      if(n>1){
      startPoint <-  matrix(fl$startPointList[[j]] +  2 * runif(dim) - 1,1,)
      } else { startPoint <-  matrix(fl$startPointList[[j]],1,)
      }
      if(verbosity>0) print(c(j,i))
      res <- rbind(res,
                   c(j,i, optim(par=startPoint, fn=fl$funList[[j]], NULL, method = method, hessian = FALSE)$value))
    }
  }
  colnames(res)<-c("f", "r", "y")
  return(res)
}

#' @title runSpotBench
#'
#' @description Run \code{\link{spot}} on a list of spot benchmark functions
#'
#' @param fl function list. Generated with one of the function list 
#' generators in \code{spot}, e.g., \code{\link{makeSpotFunList}} or 
#' \code{\link{makeMoreFunList}}. Default: \code{\link{makeMoreFunList}}.
#' 
#' @param control The control list used by \code{spot}.
#' 
#' @param n repeats. If \code{n >1 }, different start points (randomized) 
#' will be used. Default: \code{n=2}.
#' 
#' @param k subset of benchmark functions.
#' Default: \code{1:length(makeMoreFunList()$funList)},
#' i.e., all implemented functions.
#' 
#' @param verbosity Level 0 shows no output (default). 
#'
#' @return res. data.frame with results: \code{c("f", "r", "y")}
#'
#' @importFrom stats optim
#'
#' @examples
#' summary(runSpotBench(k=1)$y)
#'
#' @export
#'
runSpotBench <- function(fl = makeMoreFunList(),
                         control = list(),
                    n=2,
                    k = 1:length(makeMoreFunList()$funList),
                    verbosity = 0) {
  res <- data.frame()
  for (j in k) {
    dim <- fl$dimVec[j]
    for (i in 1:n) {
      set.seed(i)
      startPoint <-  matrix(fl$startPointList[[j]] +  2 * runif(dim) - 1,1,)
      if(j == 17){
        startPoint <-  matrix(runif(dim),1,)
      }
      if (verbosity >0)  print(startPoint)
      lower <- rep(-10, dim)
      if (verbosity >0)  print(lower)
      upper <- rep(10, dim)
      if (verbosity >0)  print(upper)
      if(j == 10){
        upper <- rep(1e7, dim)
      }
      if(j == 16){
        lower <- c(0,1000,200)
        upper <- c(3,8000,500)
        startPoint <-  matrix(lower + (upper-lower)* runif(3),1,)
      }
      if (verbosity >0)  print(j)
      fun <- function(x){matrix(apply(x, 1, fl$funList[[j]]), , 1)}
      res <- rbind(res,
                   c(
                     j,
                     i,
                     spot(
                       x = startPoint,
                       fun = fun, 
                       lower = lower,
                       upper = upper,
                       control = list(
                         model = buildKriging,
                         verbosity = 0,
                         modelControl = list(target ="ei", 
                                             useLambda=TRUE,
                                             optimizer = optimDE),
                         yImputation = list(handleNAsMethod = handleNAsMean)
                       ))$ybest
                   ))
    }
  }
  colnames(res) <- c("f", "r", "y")
  return(res)
}





