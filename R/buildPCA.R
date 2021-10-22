#' @title buildPCA
#' 
#' @description 
#' buildPCA builds principal components of given dataset.
#' It is used inside \code{\link{plotPCA}} function to build necessary 
#' object to perform principal components analysis.
#' 
#' 
#' @param x dataset of parameters to be transformed
#' @param control control list
#'
#' @importFrom stats prcomp
#'
#'
#' @examples
#' #define objective function
#' 
#' objFun <- function(x) 2*(x[1] - 1)^2 + 5*(x[2] - 3)^2 + (10*x[3] - x[4]/3)   
#' 
#' spotConfig <- 
#' list(types = c('numeric', 'numeric', 'numeric', 'numeric'), 
#' funEvals = 15, #budget
#' noise = TRUE,
#' seedFun = 1,
#' replicated = 2,
#' seedSPOT = 1,
#' design = designLHD,
#' model = buildRandomForest, #surrogate model
#' optimizer = optimLHD, #LHD to optimize model
#' optimizerControl = list(funEvals=100)) #100 model evals in each step
#' 
#' lower <- c(-20, -20, -20, -20) 
#' upper <- c(20, 20, 20, 20)
#' 
#' res <- spot(x=NULL, 
#'    fun=objFun, 
#'    lower=lower, 
#'    upper=upper, 
#'    control=spotConfig) 
#' 
#' resPCA <- buildPCA(res$x) 
#' 
#' @return returns a list with the following elements: \cr
#' 			\code{sdev} the standard deviations of the principal components (i.e., the square roots of the eigenvalues of the covariance/correlation matrix, though the calculation is actually done with the singular values of the data matrix).\cr
#' 			\code{rotation} the matrix of variable loadings (i.e., a matrix whose columns contain the eigenvectors).\cr
#' 			\code{x} transformed matrix.\cr
#' 			\code{center,scale} the centering and scaling used, or FALSE.
#' 
#' 
#' @author Alpar Gür \email{alpar.guer@@smail.th-koeln.de}
#' 
#' @export
buildPCA <- function(x, control = list()) {
  # default control list
  con <- list(retx = TRUE,
              center = TRUE,
              scale = FALSE,
              tol = NULL)
  
  con[names(control)] <- control # update default control list with user defined controls
  control <- con # use control as control list
  
  # calculate principal components
  pc <- prcomp(x,
                retx = control$retx,
                center = control$center,
                scale = control$scale,
                tol = control$tol)
  # multiply by -1 and flip the matrix, because eigen vectors pointing negative by default
  pc$rotation <- -1*pc$rotation
  # reverse the signs of the scores
  pc$x <- -1*pc$x
  
  return(pc)
}




