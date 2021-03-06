
###################################################################################
#' @title Gas Sensor Data
#'
#' @description 
#' A data set of a Gas Sensor, similar to the one used by Rebolledo et al. 2016.
#' It also contains information of 10 different test/training splits, to enable comparable evaluation procedures.
#'
#' @format A data frame with 280 rows and 20 columns (1 output, 7 input, 2 disturbance, 10 training/test split) :
#' \describe{
#'   \item{Y}{Measured Sensor Output}
#'   \item{X1}{Sensor Input 1}
#'   \item{X2}{Sensor Input 2}
#'   \item{X3}{Sensor Input 3}
#'   \item{X4}{Sensor Input 4}
#'   \item{X5}{Sensor Input 5}
#'   \item{X6}{Sensor Input 6}
#'   \item{X7}{Sensor Input 7}
#'   \item{Batch}{Disturbance variable, measurement batch}
#'   \item{Sensor}{Disturbance variable, sensor ID}
#'   \item{Set1}{test/training split, \code{1} is training data, \code{2} is test data}
#'   \item{Set2}{test/training split}
#'   \item{Set3}{test/training split}
#'   \item{Set4}{test/training split}
#'   \item{Set5}{test/training split}
#'   \item{Set6}{test/training split}
#'   \item{Set7}{test/training split}
#'   \item{Set8}{test/training split}
#'   \item{Set9}{test/training split}
#'   \item{Set10}{test/training split}
#' }
#'
#' @details 
#' Two different modeling tasks are of interest for this data set:
#' \code{Y~X1+X2+X3+X4+X5+X6+X7+Batch+Sensor} and
#' \code{X1~Y+X7+Batch+Sensor}.
#' 
#' @references
#' Margarita A. Rebolledo C., Sebastian Krey, Thomas Bartz-Beielstein, Oliver Flasch, Andreas Fischbach and Joerg Stork. \cr
#' 2016.\cr
#' Modeling and Optimization of a Robust Gas Sensor.\cr
#' 7th International Conference on Bioinspired Optimization Methods and their Applications (BIOMA 2016).\cr
###################################################################################
"dataGasSensor"
