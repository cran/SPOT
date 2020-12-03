#' @title selectN. Select n Design Points
#'
#' @description  This function returns a subset of n design points.
#'
#' @param x matrix of design points
#' @param y matrix of function values
#' @param control list of controls
#' \describe{
#'		\item{\code{N}}{number of points to be returned.}
#' }
#'
#'
#' @return This function returns a list with n design points:
#' \describe{
#'		\item{\code{x}}{matrix of selected design points.}
#'		\item{\code{y}}{ matrix of selected function values.}
#' }
#'
#' @export
#' @keywords internal
###################################################################################
selectN <- function(x, y, control) {
  nx <- dim(x)[1]
  ny <- dim(y)[1]
  try(if (nx != ny)
    stop("x and y matrices: different length"))
  N <- ifelse(exists("N", where = control), control$N, nx)
  a <- nx - N + 1
  b <- nx
  if (N > min(nx, ny)) {
    return(list(x = x, y = y))
  }
  else{
    return(list(x = matrix(x[a:b, , drop = FALSE], N,),
                y = matrix(y[a:b, , drop = FALSE], N,)))
  }
}
