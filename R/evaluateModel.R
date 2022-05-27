#' @title Evaluate Model
#' @description  This function produces an objective function with y=f(x) from a provided model fit.
#' Important note: this function expects \code{predict(object,newdata)} to return
#' a list. The \code{object$target} parameter is a string that determines which list item
#' is returned by the created function. If not set (NULL), \code{object$target} is set to \code{"y"}.
#' @param object fit created by a modeling function, e.g., \code{\link{buildRandomForest}}
#' @param infillCriterion optional parameter, a function that accepts prediction results and a model object.
#' The function should use these
#' to alter the prediction result in a user desired way.
#' For example turning the prediction results of a kriging model (mean and sd) into the expected
#' improvement criterion
#' @param verbosity verbosity (can be taken from control$verbosity). Default: 0
#' @importFrom stats predict
#' @return a function in the style of \code{y=f(x)}, which uses the fitted object
#' to predict \code{y} for sample \code{x}.
#' @export
#' @keywords internal
evaluateModel <-
  function(object,
           infillCriterion = NULL,
           verbosity = 0) {
    if (is.null(object$target)) {
      object$target <- "y"
    }
    
    evalModelFun <- function(x) {
      vmessage(verbosity,
               "evalModelFun: calling predict() with x as newdata:",
               x)
      vmessage(verbosity,
               "evalModelFun: using object$target:",
               object$target)
      res <-
        try(predict(object = object, newdata = x)[object$target])
      if (inherits(res, "try-error")) {
        trialCounter <- 1
        while (inherits(res, "try-error")) {
          trialCounter <- trialCounter + 1
          message("evalModelFun() in evaluateModel.R: Caught an error!")
          message(paste0("Attempt nr. ", trialCounter, " calling predict again."))
          #save(list(object=object, x=x), file=paste0("evalModelFun", trialCounter, ".RData"))
          res <-
            try(predict(object = object, newdata = x)[object$target])
          if (trialCounter >= 10) {
            break
          }
        }
      }
      if (is.list(res) & (length(res) == 1)) {
        res <- res[[1]]
      }
      ## here only the predicted target, e.g., "ei", is returned:
      return(res)
    }
    
    if (is.null(infillCriterion)) {
      return(function(x) {
        res <- evalModelFun(x)
        if (is.list(res)) {
          return(res[[1]])
        }
        return(res)
      })
    } else{
      return(function(x) {
        return(infillCriterion(evalModelFun(x), object))
      })
    }
  }