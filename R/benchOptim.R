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

# runSpot <- function() {
#   
#   res <- data.frame()
#   for (j in 1:k) {
#     dim <- fl$moreDimVec[j]
#     for (i in 1:n) {
#       set.seed(i)
#       startPoint <-  matrix(fl$moreStartPointList +  2 * runif(dim) - 1,1,)
#       if(j == 17){
#         startPoint <-  matrix(runif(dim),1,)
#       }
#       #print(startPoint)
#       lower <- rep(-1, dim)
#       #print(lower)
#       upper <- rep(1, dim)
#       if(j == 10){
#         upper <- rep(1e7, dim)
#       }
#       if(j == 16){
#         lower <- c(0,1000,200)
#         upper <- c(3,8000,500)
#         startPoint <-  matrix(lower + (upper-lower)* runif(3),1,)
#       }
#       #print(upper)
#       print(j)
#       res <- rbind(res,
#                    c(
#                      j,
#                      i,
#                      spot(
#                        x = startPoint,
#                        fun = fl$moreFunList[[j]],
#                        lower = lower,
#                        upper = upper,
#                        control = list(
#                          model = buildBO,
#                          modelControl = list(target ="ei", optimizer = optimLBFGSB)))$ybest
#                    ))
#     }
#   }
#   colnames(res) <- c("f", "r", "y")
#   return(res)
# }





