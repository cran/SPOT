
###################################################################################
#' Increase Sequential Design Mult Two
#' 
#' This function increases the repeats done in each sequential designby multiplying by 2
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