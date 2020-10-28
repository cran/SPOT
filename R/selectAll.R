
###################################################################################
#' Select all Design Points
#' 
#' This function returns the whole set of proposed design points.
#' It is a dummy function, used as the default for more sophisticated selection procedures.
#'
#' @param x matrix of design points
#' @param y matrix of function values 
#' @param control list of controls
#' 
#' @return This function returns a list with:
  #' \describe{
  #'		\item{\code{x}}{matrix of selected design points.}
  #'		\item{\code{y}}{ matrix of selected function values.}
  #' }
#'
#' @export
#' @keywords internal
###################################################################################
selectAll <- function(x, y, control){
 return( list(x=x, y=y))
}
