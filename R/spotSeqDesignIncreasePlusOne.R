###################################################################################
#' Increase Sequential Design Plus One
#' 
#' This function increases the repeats done in each sequential design step of SPOT by adding 1.
#' Increasing repeats ensures that SPOT prediction is getting more precise with growing step size.
#' 
#' @param actRepeats must hold the number of repeats recommended last time. 
#'
#' @return number \code{repeats} \cr
#' - \code{repeats} is the new recommended number of repeats
#'
#' @references  \code{\link{SPOT}} \code{\link{spot}} \code{\link{spotSeqDesignIncreaseMultTwo}} 
####################################################################################

spotSeqDesignIncreasePlusOne <- function (actRepeats){
	return(actRepeats +1)
}