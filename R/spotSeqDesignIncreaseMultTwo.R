
###################################################################################
#' Increase Sequential Design Mult Two
#' 
#' This function increases the repeats done in each sequential design step of SPOT by multiplying by 2.
#' Increasing repeats ensures that SPOT prediction is getting more precise with growing step size.
#' 
#' @param actRepeats must hold the number of repeats recommended last time. 
#'
#' @return number \code{repeats} \cr
#' - \code{repeats} is the new recommended number of repeats
#'
#' @references  \code{\link{SPOT}} \code{\link{spot}} \code{\link{spotSeqDesignIncreasePlusOne}} 
####################################################################################
spotSeqDesignIncreaseMultTwo <- function (actRepeats){
	return(actRepeats *2)
}