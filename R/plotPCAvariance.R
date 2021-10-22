#' @title plotPCAvariance
#' 
#' @description 
#' plotPCAvariance illustrates the total variance within the dataset.
#' It plots the effectiveness of each principal component and can be used to decide how many and which prinicpal components to plot.
#' In order to create this plot, users don't need to build PCA beforehand since it handles this process automatically.
#'
#'
#' @param x dataset of parameters to be transformed & plotted
#'
#' @importFrom ggplot2 qplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 ylim
#'
#'
#' @examples
#' # objective function
#' funBard <- function (x) {
#'   bard <- function(par) {
#'     y <- c(0.14, 0.18, 0.22, 0.25, 0.29, 0.32, 0.35, 0.39, 0.37, 0.58,
#'            0.73, 0.96, 1.34, 2.10, 4.39)
#'     m <- 15
#'     x1 <- par[1]
#'     x2 <- par[2]
#'     x3 <- par[3]
#'     
#'     fsum <- 0
#'     for (u in 1:m) {
#'       v <- 16 - u
#'       w <- min(u, v)
#'       f <- y[u] - (x1 + u / (v * x2 + w * x3))
#'       fsum <- fsum + f * f
#'     }
#'     return(fsum)
#'   }
#'   matrix(apply(x, # matrix
#'                1, # margin (apply over rows)
#'                bard),
#'          , 1) # number of columns
#' }
#' 
#' # starting point
#' x1 <- matrix(c(1,1),1,)
#' funBard(x1)
#' 
#' #boundaries
#' lower = c(-0.001,-0.007,-0.003)
#' upper = c(0.5,1.0,1.1)
#' 
#' res <- spot(,funBard, lower=lower, upper=upper, control=list(funEvals=15))
#' 
#' plotPCAvariance(res$x) # plot variance within the dataset
#' 
#' @return It returns a plot image.
#' 
#' @seealso \code{\link{buildPCA}}
#' 
#' @author Alpar Gür \email{alpar.guer@@smail.th-koeln.de}
#' 
#' @export
plotPCAvariance <- function(x){
  pca <- buildPCA(x)
  # calculate total variance explained by each principal component
  var_explained <- pca$sdev^2 / sum(pca$sdev^2)
  var_explained
  # create scree plot
  screePlot <- qplot(c(1:ncol(x)), var_explained) +
    geom_line() +
    xlab("Principal Component") +
    ylab("Variance") +
    ggtitle("Scree Plot - Total Variance") +
    ylim(0, 1)
  
  return(screePlot)
}