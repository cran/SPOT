#' @title plotPCA
#' 
#' @description 
#' plotPCA returns a 2D plot of optimization data in it's own space using buildPCA. 
#' It plots first two PCAs by default.
#'
#' @param x dataset of parameters to be transformed & plotted
#' @param control control list
#'
#' @importFrom stats biplot
#'
#' @examples
#' # define objective function
#' funGauss <- function (x) {
#'   gauss <- function(par) {
#'     y <- c(0.0009, 0.0044, 0.0175, 0.0540, 0.1295, 0.2420, 0.3521, 0.3989,
#'            0.3521, 0.2420, 0.1295, 0.0540, 0.0175, 0.0044, 0.0009)
#'     m <- 15
#'     x1 <- par[1]
#'     x2 <- par[2]
#'     x3 <- par[3]
#'     
#'     fsum <- 0
#'     for (i in 1:m) {
#'       ti <- (8 - i) * 0.5
#'       f <- x1 * exp(-0.5 * x2 * (ti - x3) ^ 2) - y[i]
#'       fsum <- fsum + f * f
#'     }
#'     return(fsum)
#'   }
#'   matrix(apply(x, # matrix
#'                1, # margin (apply over rows)
#'                gauss),
#'          , 1) # number of columns
#' }
#' 
#' # define starting point
#' x1 <- matrix(c(1,1,1),1,)
#' funGauss(x1)
#' 
#' # define boundaries
#' lower = c(-0.001,-0.007,-0.003)
#' upper = c(0.5,1.0,1.1)
#' 
#' res <- spot(,funGauss, lower=lower, upper=upper, control=list(funEvals=15))
#' 
#' control = list(scale=TRUE) #pca control list, # scale the variables
#' 
#' plotPCA(res$x, control=control) # plot first two PCAs
#' 
#' @return It returns a plot image.
#' 
#' @seealso \code{\link{buildPCA}}, \code{\link{biplot}}
#' 
#' @author Alpar Gür \email{alpar.guer@@smail.th-koeln.de}
#' 
#' @export
plotPCA <- function(x, control = list()){
  # default control list
  con <- list(retx = TRUE,
              center = TRUE,
              scale = FALSE,
              tol = NULL)
  
  con[names(control)] <- control # update default control list with user defined controls
  control <- con # use control as control list
  
  results <- buildPCA(x, control)

  pcaPlot <- biplot(results, scale = 0) #biplot plots first two components of PCs by default
  return(pcaPlot)
}
