#' @title duplicateAndReplicateHandling
#'
#' @description Handle Duplicates and Replicates
#'
#' @details
#' This function deals with duplicates, that is, when a candidate solution (\code{xnew})
#' has already been evaluated (that is, \code{xnew} is element of \code{x}).
#' If \code{control$noise} is \code{TRUE} (objective is noisy), duplicates are allowed.
#' In that case, this function only makes sure that such duplicates
#' do not receive additional replications (i.e., the duplicate is evaluated only once).
#' If the objective is not noisy, duplicates are replaced by randomly created solution
#' and a warning is given.
#'
#' @param xnew matrix of new candidate solution(s), one row for each solution.
#' @param x matrix of evaluated solutions.
#' @param lower vector for lower boundary of decision space
#' @param upper vector for upper boundary of decision space
#' @param control list of controls
#'
#' @return matrix \code{xnew}, with additional replicates for non-duplicates,
#' or duplicates replaced by random solutions.
#'
#' @export
#' @keywords internal
duplicateAndReplicateHandling <-
  function(xnew, x, lower, upper, control) {
    ## FIXME: helper function, remove after testing
    ## replaces "if (any(apply(x, 1, identical, xnew[i, ])))"
    rowsInMatrix <- function(x, X) {
      n <- dim(X)[1]
      r <- rep(0, n)
      for (i in (1:n)) {
        if (identical(x,  X[i,]))
          r[i] <- 1
      }
      return(r)
    }
    if (control$noise) {
      ## determine on the fly whether xnew is in x -> no "initialReplications" needed.
      ## else, assign initial replications.
      xtmp <- NULL
      for (i in 1:nrow(xnew)) {
        #if solution has NOT been evaluated previously
        if (!any(apply(x, 1, identical, xnew[i, ]))) {
          #TODO this may conflict with parameter names... does x, xnew, xtmp  etc have parameter names?
          xtmp <-
            rbind(xtmp, xnew[rep(i, control$replicates - 1), ])  #TODO this does
          ## not deal with duplicates in xnew itself.
        }
      }
      if (!is.null(xtmp)) {
        xtmp <- matrix(xtmp, ncol = ncol(x))
      }
      xnew <- rbind(xnew, xtmp)
    } else{
      ## check for duplicates in case of noise==FALSE -->
      ## warning + return results or warning + replace with random solution
      for (i in 1:nrow(xnew)) {
        #if solution has NOT been evaluated previously:
        ## FIXME: test, does the following work?: if (any(apply(x, 1, identical, xnew[i, ]))) {
        ## It was replaced with rowsInMatrix(), see FIXME below:
        ## if(sum(rowsInMatrix(x= xnew[i,], X=x)) >0){
        if (any(apply(x, 1, identical, xnew[i, ]))) {
          ## TODO this may conflict with parameter names... does x, xnew  etc have parameter names?
          vmessage(
            control$verbosity,
            "SPOT created a duplicated solution. This can be due to early convergence or bad configuration. Duplicate is replaced by random solution.",
            rbind(x, xnew[i, , drop = FALSE])
          )
          if (control$verbosity > 0) {
            warning(
              "SPOT created a duplicated solution. This can be due to early
              convergence or bad configuration. Duplicate is replaced by random solution."
            )
          }
          control$designControl$replicates <- 1
          control$designControl$size <- 1
          ## BEGIN new May 11 2022:
          ## xnew[i,] <- designUniformRandom(, lower, upper, control$designControl)
          xn <- designUniformRandom(, lower, upper, control$designControl)
          if (!is.null(control$designControl$inequalityConstraint)) {
            while (apply(xn, 1, control$designControl$inequalityConstraint) > 0) {
              xn <-  designUniformRandom(, lower, upper, control$designControl)
            }
          }
          xnew[i, ] <- xn
          ## END new May 11 2022
          ## TODO what if result is also duplicate
        }
      }
    }
    xnew
  }