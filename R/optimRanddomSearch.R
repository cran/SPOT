#' @title Random search surrogate-optimizer
#'
#' @description 
#' This function is used to emulate uniform random search with SPOT.
#' It is used as the optimizer that searches for new candidates.
#' It returns a single uniform random sample within
#' the given lower and upper bounds of the search space.
#' 
#' @param x start guess, not used.
#' @param fun objective function to be evaluated via random search.
#' @param lower bound on the independent variables (search space).
#' @param upper bound on the independent variables (search space).
#' @param control not used.
#' @param ... additional arguments, not used.
#' 
#' @importFrom stats runif
#' 
#' @return list
#' @export
optimRSfun <- function(x, fun, lower, upper, control, ...) {
	xbest <- matrix(runif(length(lower), lower, upper), 1)
	ybest <- matrix(fun(xbest), 1)
	list(
		xbest = xbest,
		ybest = ybest,
		x = xbest,
		y = ybest,
		count = 1
	)
}