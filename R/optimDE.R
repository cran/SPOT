###################################################################################################
#' Minimization by Differential Evolution
#'
#' For minimization, this function uses the \code{"DEoptim"} method from the
#' code{DEoptim} package. It is basically a wrapper, to enable DEoptim for usage
#' in SPOT.
#'
#' @param x optional start point
#' @param fun objective function, which receives a matrix x and returns observations y
#' @param lower boundary of the search space
#' @param upper boundary of the search space
#' @param control list of control parameters
#' \describe{
#'   \item{\code{funEvals}}{Budget, number of function evaluations allowed. Default is 200.}
#'   \item{\code{populationSize}}{Population size or number of particles in the population. Default is 10*dimension.}
#' }
#' @param ... passed to \code{fun}
#'
#' @return list, with elements
#' \describe{
#'   \item{\code{x}}{archive of the best member at each iteration}
#'   \item{\code{y}}{archive of the best value of fn at each iteration}
#'   \item{\code{xbest}}{best solution}
#'   \item{\code{ybest}}{best observation}
#'   \item{\code{count}}{number of evaluations of \code{fun}}
#' }
#'
#' @examples
#' res <- optimDE(,lower = c(-10,-20),upper=c(20,8),fun = funSphere)
#' res$ybest
#' optimDE(x = matrix(rep(1,6), 3, 2),lower = c(-10,-20),upper=c(20,8),fun = funSphere, 
#'    control = list(funEvals=100, populationSize=20))
#' #Compare to DEoptim:
#' require(DEoptim)
#' set.seed(1234)
#' DEoptim(function(x){funRosen(matrix(x,1))}, lower=c(-10,-10), upper=c(10,10), 
#'   DEoptim.control(strategy = 2,bs = FALSE, N = 20, itermax = 28, CR = 0.7, F = 1.2,
#'   trace = FALSE, p = 0.2, c = 0, reltol = sqrt(.Machine$double.eps), steptol = 200 ))
#' set.seed(1234)
#' optimDE(, fun=funRosen, lower=c(-10,-10), upper= c(10,10), 
#'    control = list( populationSize = 20, funEvals = 580, F = 1.2, CR = 0.7))
#' @export
###################################################################################################
optimDE <- function(x = NULL,
                    fun,
                    lower,
                    upper,
                    control = list(),
                    ...) {
  con <-
    list(
      funEvals = 200,
      populationSize = (10 * length(lower)),
      strategy = 2,
      bs = FALSE,
      NP = 10,
      itermax = 100,
      CR = 0.5,
      F = 0.8,
      trace = FALSE,
      initialpop = NULL,
      ## storepopfrom
      #storepopfreq = 1,
      p = 0.2,
      c = 0
    )
  con[names(control)] <- control
  control <- con
  funEvals <- control$funEvals
  NP <- control$populationSize
  
  ## recalculate funEvals to DE iteration on basis of population size
  itermax <- floor((funEvals - NP) / NP)
  if (itermax < 1)
    itermax = 1
  
  control$NP <- control$populationSize
  control$itermax <- itermax
  
  ## Delete unused settings
  control$populationSize <- NULL
  control$funEvals <- NULL
  control$types <- NULL
  
  ## wrapper for matrix inputs to fun
  fn <- function(x, ...)
    fun(matrix(x, 1), ...)
  
  ## start optim
  res <- DEoptim(
    fn = fn,
    lower = lower,
    upper = upper,
    control = control,
    ...
  )
  list(
    x = res$member$bestmemit,
    y = res$member$bestvalit,
    xbest = matrix(res$optim$bestmem, 1),
    ybest = matrix(res$optim$bestval, 1),
    count = res$optim$nfeval * nrow(res$member$pop)
  )
}