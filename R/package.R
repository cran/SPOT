# Package Description for Roxygen:
#' Sequential Parameter Optimization Toolbox
#'
#' SPOT uses a combination statistic models and optimization algorithms 
#' for the purpose of parameter optimization. Design of Experiment methods
#' are employed to generate an initial set of candidate solutions, which
#' are evaluated with a user-provided objective function.
#' The resulting data is used to fit a model, which in turn is subject
#' to an optimization algorithm, to find the most promising candidate solution(s).
#' These are again evaluated, after which the model is updated with the new results.
#' This sequential procedure of modeling, optimization, and evaluation is iterated
#' until the evaluation budget is exhausted. 
#'
#'
#' @name SPOT-package
#' @aliases SPOT
#' @docType package
#' @title Sequential Parameter Optimization Toolbox
#' @author Thomas Bartz-Beielstein \email{tbb@@bartzundbartz.de}, Martin Zaefferer, and F. Rehbach
#' with contributions from: C. Lasarczyk, M. Rebolledo, Joerg Stork.
#' @keywords package
#' @seealso Main interface function is \code{\link{spot}}.
#' 
#' @section Maintainer:
#' Thomas Bartz-Beielstein \email{tbb@@bartzundbartz.de} 
#End of Package Description
NA