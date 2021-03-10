#' @title Expected Improvement Infill Criterion 
#' 
#' @description
#' Compute the negative of the Expected Improvement of a set of candidate solutions.
#' Based on mean and standard deviation of a candidate solution,
#' this estimates the expectation of improvement. Improvement
#' considers the amount by which the best known value (best observed value)
#' is exceeded by the candidates.
#' Expected Improvement infill criterion that can be passed to control$modelControl$infillCriterion 
#' in order to be used during the optimization in SPOT.
#' Parameters dont have to be specified as this function is ment to be internally by SPOT.
#' 
#' @param predictionList The results of a predict.model call
#' @param model The surrogate model which was used for the prediction
#'
#' @importFrom stats pnorm
#' @importFrom stats dnorm
#' 
#' @return numeric vector, expected improvement results
#' @export
#'
#' @examples
#' \donttest{
#' spot(,funSphere,c(-2,-3),c(1,2), control = 
#'     list(infillCriterion = infillEI, modelControl = list(target = c("y","s"))))
#'     }
infillEI <- function(predictionList, model){
    pMean <- predictionList$y
    sigma <- predictionList$s
    fMin <- min(model$y)
    d <- fMin - pMean
    dn <- d/sigma
    ei <- d*pnorm(dn) + sigma*dnorm(dn)
    return(- ei)
}