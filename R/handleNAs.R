#' @title handleNAsMean
#' @description Remove NAs from a vector by replacing them by the sample mean.
#'
#' @param x The x values from which y was calculated, not used here
#' @param y The vector of numerics from which the NAs should be removed
#' @param imputeCriteriaFuns \code{list} criteria functions specified via
#' \code{imputeCriteriaFuns} in \code{\link{spotControl}}. Default:
#' \code{list(is.na, is.infinite, is.nan)}.
#' @param penaltyImputation penalty used for imputed values
#'
#' @return y The cleaned vector
#'
#' @examples
#' vecWithNAs <- c(-1, 0,1,NA,3,Inf,5,NA)
#' control <- spotControl(dim=length(vecWithNAs))
#' print(vecWithNAs)
#' print(handleNAsMean(y=vecWithNAs,
#'                      imputeCriteriaFuns= control$yImputation$imputeCriteriaFuns))
#' @export
handleNAsMean <- function(x,
                          y = NULL,
                          imputeCriteriaFuns = list(is.na, is.infinite, is.nan),
                          penaltyImputation = 3) {
  if (is.null(imputeCriteriaFuns)) {
    imputeCriteriaFuns <- list(is.na, is.infinite, is.nan)
  }
  p <- getPositions(y,
                    imputeCriteriaFuns)
  if (is.null(penaltyImputation)) {
    penaltyImputation <- 3
  }
  if (sum(p) > 0) {
    ## Handling of missing values
    y[p] <- tryCatch(
      expr = {
        mean(y[-p]) + penaltyImputation *  sd(y[-p])
      },
      error = function(e) {
        message("Calling handleNAsMean() with only one value: Imputation requires at least 2 values to determine sd()")
        print(e)
      }
    )
  }
  return(y)
}



#' handleNAsMax
#'
#' Remove NAs from a vector by replacing them by the currant max + p*s.d., where p
#' denotes a penalty term.
#'
#' @param x The x values from which y was calculated, not used here
#' @param y The vector of numerics from which the NAs should be removed
#' @param imputeCriteriaFuns \code{list} criteria functions specified via
#' \code{imputeCriteriaFuns} in \code{\link{spotControl}}. Default:
#' \code{list(is.na, is.infinite, is.nan)}.
#' @param penaltyImputation penalty used for imputed values
#'
#' @return y The cleaned vector
#'
#' @export
#'
#' @examples
#' vecWithNAs <- c(-1, 0,1,NA,3,Inf,5,NA)
#' control <- spotControl(dim=length(vecWithNAs))
#' print(vecWithNAs)
#' print(handleNAsMax(y=vecWithNAs,
#'                      imputeCriteriaFuns= control$yImputation$imputeCriteriaFuns))
handleNAsMax <- function(x,
                         y = NULL,
                         imputeCriteriaFuns = list(is.na, is.infinite, is.nan),
                         penaltyImputation = 3) {
  if (is.null(imputeCriteriaFuns)) {
    imputeCriteriaFuns <- list(is.na, is.infinite, is.nan)
  }
  p <- getPositions(y,
                    imputeCriteriaFuns)
  if (is.null(penaltyImputation)) {
    penaltyImputation <- 3
  }
  if (sum(p) > 0) {
    ## Handling of missing values
    y[p] <- tryCatch(
      expr = {
       max(y[-p]) + penaltyImputation *  sd(y[-p])
      },
      error = function(e) {
        message("Calling handleNAsMax() with only one value: Imputation requires at least 2 values to determine sd()")
        print(e)
      }
    )
  }
  return(y)
}


#' handleNAsKrigingWorst
#'
#' Remove NAs from a vector by replacing them with a penalized
#' Kriging-based expectation
#'
#' @param x The x values from which y was calculated
#' @param y The vector of numerics from which the NAs should be removed
#' @param penaltyImputation multiplier for sPredicted (penalty term). Default: \code{3}.
#' @param imputeCriteriaFuns \code{list} criteria functions specified via
#' \code{imputeCriteriaFuns} in \code{\link{spotControl}}. Default:
#' \code{list(is.na, is.infinite, is.nan)}.
#'
#' @return y The imputed vector w/o \code{NA} and w/o \code{Inf} values.
#'
#' @examples
#' imputeCriteriaFuns <- list(is.na, is.infinite, is.nan)
#' x <- matrix(runif(20), ncol = 2)
#' y <- funSphere(x)
#' y[3] <- NA
#' y[5] <- Inf
#' plot(y, type="b")
#' print(y)
#' y1 <- handleNAsKrigingWorst(x=x, y=y, imputeCriteriaFuns=imputeCriteriaFuns)
#' print(y1)
#' points(3, y1[3], type="b", col="red")
#' points(5, y1[5], type="b", col="red")
#' @export
#'
handleNAsKrigingWorst <- function(x,
                                  y,
                                  penaltyImputation = 3,
                                  imputeCriteriaFuns = list(is.na, is.infinite, is.nan))
{
  p <- getPositions(y,
                    imputeCriteriaFuns)
  if (sum(p) > 0) {
    if (is.null(penaltyImputation))
      penaltyImputation <- 3
    if (is.null(imputeCriteriaFuns))
      imputeCriteriaFuns <- list(is.na, is.infinite, is.nan)
    yWithout <- y[-c(p), , drop = FALSE]
    xWithout <- x[-c(p), , drop = FALSE]
    model <-
      buildKriging(xWithout, yWithout, control = list(target = c("y", "s")))
    yPredicted <- predict(model, newdata = x[p, , drop = FALSE])$y
    sPredicted <- predict(model, newdata = x[p, , drop = FALSE])$s
    yNew <- yPredicted + penaltyImputation * sPredicted
    y[p] <- yNew
  }
  return(y)
}


#' @title get impute positions
#'
#' @description Determines positions in a vectors
#' that fulfill criteria defined by a list
#' of criteria, e.g., \code{is.na}.
#'
#' @param y The vector of numerics from which NA/Inf values should be removed
#' @param imputeCriteriaFuns \code{list} criteria functions specified via
#' \code{imputeCriteriaFuns} in \code{\link{spotControl}}.
#' Default: \code{list(is.na, is.infinite, is.nan)}.
#'
#' @return p vector of positions that fulfill one of the criteria
#'
#' @examples
#' imputeCriteriaFuns <- list(is.na, is.infinite, is.nan)
#' y <- c(1,2,Inf,4,NA,6)
#' p <- getPositions(y, imputeCriteriaFuns)
#' @export
getPositions <- function(y,
                         imputeCriteriaFuns = list(is.na, is.infinite, is.nan)) {
  if (is.null(imputeCriteriaFuns)) {
    imputeCriteriaFuns <- list(is.na, is.infinite, is.nan)
  }
  y <- matrix(y, ncol = 1)
  v <- sapply(imputeCriteriaFuns, mapply, y)
  v <- matrix(v, ncol = length(imputeCriteriaFuns))
  i <- apply(v, 1, any)
  p <- which(i)
  return(p)
}

#' @title Impute NAs and Inf in y
#'
#' @param x The x values from which y was calculated
#' @param y The vector of numerics from which NA/Inf values should be removed
#' @param control \code{\link{spot}} control list. See also \code{\link{spotControl}}.
#'
#' @return y The imputed vector w/o \code{NA} and w/o \code{Inf} values.
#'
#' @examples
#'
#' x <- matrix(runif(10), ncol=2, nrow=5)
#' y <- funSphere(x)
#' y[1] <- NA
#' control <- spotControl(dimension = 2)
#' # no imputation function, i.e, w/o imputation
#' imputeY(x=x, y=y, control=control)
#' # with imputation
#' control$yImputation$handleNAsMethod <- handleNAsKrigingWorst
#' y <- imputeY(x=x, y=y, control=control)
#' # no imputation required:
#' imputeY(x=x, y=y, control=control)
#'
#' @export
imputeY <- function(x,
                    y,
                    control) {
  if (is.null(control$yImputation$handleNAsMethod)) {
    return(y)
  }
  y1 <- y
  for(i in 1:ncol(y)){
    # print(paste0("Treating",i))
  y1[,i] <- control$yImputation$handleNAsMethod(
    x = x,
    y = y[,i, drop=FALSE],
    imputeCriteriaFuns = control$yImputation$imputeCriteriaFuns,
    penaltyImputation = control$yImputation$penaltyImputation
  )}
  if (!identical(y,y1)) {
    message("NAs were found and treated!")
    ## if (control$verbosity > 0) {
    message("y before treatment:")
    print(y)
    message("y after treatment:")
    print(y1)
    ## }
  }
  return(y1)
}
