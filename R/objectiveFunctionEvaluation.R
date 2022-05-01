#' @title objectiveFunctionEvaluation  Objective Function Evaluation
#'
#' @description  This function handles the evaluation of the objective function
#' in \code{\link{spot}}.
#' This includes handling of the random number generator stream, variable transformations
#' (\code{\link{transformX}}) as well as the actual evaluation.
#' @param x matrix of already known solutions,
#' to determine whether RNG seeds for new solutions need to be incremented.
#' @param xnew matrix of new solutions.
#' @param fun objective function to evaluate the solutions in \code{xnew}.
#' @param control control list with the following entries:
#' \describe{
#'  \item{\code{seedFun}}{initial seed to be used for the random number generator
#'   seed. Set to \code{NA} to avoid using a fixed seed.}
#' \item{\code{noise:}}{\code{logical} parameter specifying
#'       whether the target function is noisy. }
#' \item{\code{verbosity:}}{verbosity. Default: 0.}
#' \item{\code{transformFun:}}{transformation functions applied to \code{xnew}. See
#' \code{\link{transformX}}}
#' }
#' @param ... parameters passed to \code{fun}.
#'
#' @examples
#' \donttest{
#' ## 1) without noise
#' x <- NULL
#' xnew <- matrix(1:10, ncol=2)
#' fun <- funSphere
#' control <- spotControl(dim(xnew)[2])
#' control$verbosity <- 0
#' objectiveFunctionEvaluation(x=x, xnew=xnew, fun=fun, control=control)
#' ##
#' fun <- funMoo
#' objectiveFunctionEvaluation(x=x, xnew=xnew, fun=fun, control=control)
#' ## 2) with noise
#' fun = function(x){funSphere(x) + rnorm(nrow(x))}
#' control$noise <- TRUE
#' objectiveFunctionEvaluation(x=x, xnew=xnew, fun=fun, control=control)
#' ## 3) known solutions
#' x <- matrix(11:20, ncol=2)
#' xnew <- matrix(1:10, ncol=2)
#' fun <- funSphere
#' objectiveFunctionEvaluation(x=x, xnew=xnew, fun=fun, control=control)
#' ## 4) known solutions with noise and repeats
#' x <- matrix(1:20, ncol=2, byrow=TRUE)
#' xnew <- matrix(1:10, ncol=2, byrow=TRUE)
#' fun = function(x){funSphere(x) + rnorm(nrow(x))}
#' objectiveFunctionEvaluation(x=x, xnew=xnew, fun=fun, control=control)
#' ## 5) identical solutions with noise and repeats
#' x <- matrix(1:10, ncol=2, byrow=TRUE)
#' xnew <- x
#' fun = function(x){funSphere(x) + rnorm(nrow(x))}
#' y <- objectiveFunctionEvaluation(x=NULL, xnew=x, fun=fun, control=control)
#' y1 <- objectiveFunctionEvaluation(x=x, xnew=xnew, fun=fun, control=control)
#' y2 <- objectiveFunctionEvaluation(x=NULL, xnew=xnew, fun=fun, control=control)
#' print(cbind(x, y))
#' print(cbind(xnew, y1))
#' print(cbind(xnew, y2))
#' identical(y, y1) # FALSE
#' identical(y, y2) # TRUE
#' ## 6) known solutions with noise and repeats. function sets seed
#' x <- matrix(1:20, ncol=2, byrow=TRUE)
#' xnew <- matrix(1:10, ncol=2, byrow=TRUE)
#' fun <- function(x,seed){
#'   set.seed(seed)
#'   funSphere(x)+rnorm(nrow(x))}
#' control$seedFun <- 1
#' y1 <- objectiveFunctionEvaluation(x=x, xnew=xnew, fun=fun, control=control)
#' y2 <- objectiveFunctionEvaluation(x=x, xnew=xnew, fun=fun, control=control)
#' identical(y1, y2) # TRUE
#' control$seedFun <- 2
#' y3 <- objectiveFunctionEvaluation(x=x, xnew=xnew, fun=fun, control=control)
#' identical(y1,y3) # FALSE
#' ## 7) spot examples:
#' res1a <- spot(,function(x,seed){set.seed(seed);funSphere(x)+rnorm(nrow(x))},
#' c(-2,-3),c(1,2),control=list(funEvals=25,noise=TRUE,seedFun=1))
#' res1b <- spot(,function(x,seed){set.seed(seed);funSphere(x)+rnorm(nrow(x))},
#' c(-2,-3),c(1,2),control=list(funEvals=25,noise=TRUE,seedFun=1))
#' res2 <- spot(,function(x,seed){set.seed(seed);funSphere(x)+rnorm(nrow(x))},
#' c(-2,-3),c(1,2),control=list(funEvals=25,noise=TRUE,seedFun=2))
#' sprintf("Should be equal: %f = %f. Should be different:  %f", res1a$ybest,
#' res1b$ybest, res2$ybest)
#' }
#' @return the matrix ynew, which are the observations for fun(xnew)
#'
#' @seealso \code{\link{spot}} for more details on the parameters, e.g., \code{fun}
#' @seealso \code{\link{transformX}}
#' @seealso \code{\link{spotControl}}
#'
#' @export
objectiveFunctionEvaluation <- function(x = NULL,
                                        xnew,
                                        fun,
                                        control = list(),
                                        ...) {
  seedFun <- control$seedFun
  noise <- control$noise
  verbosity <- control$verbosity
  transformFun <- control$transformFun
  if (verbosity > 0) {
    message("Entering objectiveFunctionEvaluation()")
  }
  if (!is.null(x)) {
    x <- data.matrix(x)
  }
  xnew <-
    data.matrix(xnew) #TODO: same as in ocba. else, problems with identical() due to names
  ## if xnew is empty, return NULL
  if (nrow(xnew) == 0) {
    ## return(numeric(0))
    ## Changed in 2.2.0 to:
    message("Warning: return(NULL) from objectiveFunctionEvaluation() because xnew is empty!")
    return(NULL)
  }
  
  ## save seed status (to avoid disturbing the main loops RNG stream)
  ## note: theoretically only needed in case of noise==TRUE, but always done to avoid mistakes.
  if (exists(as.character(substitute(.Random.seed))))
    SAVESEED <- .Random.seed
  else
    SAVESEED = NULL
  
  if (verbosity > 0) {
    message("objectiveFunctionEvaluation(): xnew before transformX().")
    print(xnew)
  }
  
  if (length(transformFun) > 0) {
    xnew <- transformX(xNat = xnew, fn = transformFun)
  }
  
  if (verbosity > 0) {
    message("objectiveFunctionEvaluation(): xnew after transformX().")
    print(xnew)
  }
  
  if (noise & !is.na(seedFun)) {
    ## calculate seeds for each evaluation
    seed <- numeric(nrow(xnew))
    for (i in 1:nrow(xnew)) {
      xnewi <- xnew[i,]
      x <- rbind(x, xnewi)
      repetitions <- sum(apply(x, 1, identical, xnewi)) - 1
      seed[i] <- seedFun + repetitions
    }
    
    ## either pass seed to objective function fun (if there is a parameter to receive it)
    nms <- names(formals(fun))
    passSeed <- FALSE
    if (length(nms) > 1) {
      passSeed <- names(formals(fun)[2]) == "seed"
    }
    if (passSeed) {
      ynew <- fun(xnew, seed, ...)
    } else{
      ## or set seed directly here
      ynew <- NULL
      for (i in 1:nrow(xnew)) {
        set.seed(seed[i])
        ynew <- rbind(ynew, fun(xnew[i, , drop = FALSE],...))
      }
    }
  } else{
    if (is.null(control$yImputation$handleNAsMethod)) {
      # No NA handling:
      ynew <- fun(xnew, ...)
    } else{
      # If obj fun errors occur:
      # return NULL and continue
      # instead of stopping the evaluations.
      # This is only done if an NA handling method is defined.
      ynew <- NULL
      for (i in 1:nrow(xnew)) {
        yVal <-  tryCatch(
          expr = {
            fun(xnew[i, , drop = FALSE], ...)
          },
          error = function(e) {
            return(NULL)
          }
        )
        ynew <- rbind(ynew, yVal)
      } # end for
    } # end else
  }
  
  ## load seed status (to avoid disturbing the main loops RNG stream), see save at start of function.
  if (!is.null(SAVESEED))
    assign(".Random.seed", SAVESEED, envir = globalenv())
  
  ## load seed status (to avoid disturbing the main loops RNG stream), see save at start of function.
  if (!is.null(SAVESEED))
    assign(".Random.seed", SAVESEED, envir = globalenv())
  
  ## Changed: 2022-03-12:
  ## ynew not a matrix and xnew>1 (several values are evaluated)
  ## => return a  matrix with xnew rows
  ## The following code is replaced by the code directly below:
  # if(!is.matrix(ynew) & nrow(xnew)>1)
  # 	ynew <- matrix(data = ynew,
  # 	               ncol = 1) #convert to column matrix
  # if(!is.matrix(ynew) & nrow(xnew)==1)
  # 	ynew <- matrix(data = ynew,
  # 	               nrow = 1) #convert to row matrix
  if (verbosity > 0) {
    message("ynew before converting to matrix:")
    print(ynew)
  }
  if (!is.matrix(ynew)) {
    ynew <- matrix(data = ynew,
                   nrow = nrow(xnew),
                   byrow = TRUE)
    if (verbosity > 0) {
      message("ynew after converting to matrix:")
      print(ynew)
    }
  }
  return(ynew)
}

#' @title update seed
#' @param x matrix of already known solutions,
#' to determine whether RNG seeds for new solutions need to be incremented.
#' @param xnew matrix of new solutions.
#' @param seed seed
#' @param seedFun initial seed to be used for the random number generator seed.
#' Set to NA to avoid using a fixed seed.
#' @param verbosity verbosity. Default: 0
#' @return the updated seed
#' @export
#' @keywords internal
updateSeedFromXnew <-
  function(x, xnew, seed, seedFun, verbosity = 0) {
    tryCatch(
      expr = {
        for (i in 1:nrow(xnew)) {
          xnewi <- xnew[i,]
          x <- rbind(x, xnewi)
          repetitions <- sum(apply(x, 1, identical, xnewi)) - 1
          seed[i] <- seedFun + repetitions
        }
        return(seed)
      },
      error = function(e) {
        message('updateSeedFromXnew: Caught an error!')
        print(e)
      },
      warning = function(w) {
        message('updateSeedFromXnew: Caught an warning!')
        print(w)
      },
      finally = {
        if (verbosity > 0) {
          message('updateSeedFromXnew: All done, quitting.')
        }
      }
    )
  }
