#' @title  Build random search dummy model
#'
#' @description 
#' This function is used to emulate uniform random search with SPOT.
#' It is a placeholder for the surrogate model and
#' simply returns an empty list, with class "rsdummy".
#' 
#' @param x x (independent variables), not used.
#' @param y y (dependent variable), not used.
#' @param control control, not used.
#' 
#' @export
buildrsdummy <- function(x, y, control) {
	res <- list()
	class(res) <- "rsdummy"
	res
}

#' @title Predict random search dummy
#'
#' @description 
#' This function is used to emulate uniform random search with SPOT.
#' It is a placeholder for the surrogate model and
#' simply returns \code{NA} when called.
#' 
#' @param object dummy object, not used.
#' @param newdata dummy data, not used.
#' @param ... additional arguments, not used.
#' 
#' @export
#' @keywords internal
predict.rsdummy <- function(object, newdata, ...) {
	return(NA)
}
