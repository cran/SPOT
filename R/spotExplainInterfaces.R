#' General Help on SPOT...
#'
#' SPOT is a system open and designed to plug in \cr 
#' - your own algorithm see  \code{\link{spotAlgStartAlg...}}\cr
#' - your own predictors see  \code{\link{spotPredict...}}\cr
#' - your own design creating functions  \code{\link{spotCreate...}}\cr
#' - your own increase functions to increase the repeats for each sequential step
#'
####################################################################################
spotInterface... <- function(){
	
}

#' General Help on Prediction models in SPOT...
#'
#' SPOT provides some predictors to create a meta-model of the function or algorithm to be analyzed. 
#' Nevertheless the user may provide and include his own predictors. To solve this task the user must follow 
#' these instructions carefully:\cr
#' 1) The function the user wants to include must be an R-function provided in a separated file where the 
#' function name MUST be the same as the filename (just without the file extension .R)\cr
#' 2) use the function \code{\link{spotInstAndLoadPackages}} to add the packages that are required for your
#' function, just make it the first line in your function.\cr 
#' 3) adapt the configuration file, two parameters are to be included/changed:\cr
#' \code{seq.predictionModel.func="myPredictLm"} \cr
#' this example assumes the existence of a file "myPredictLm.R" in the directory "/home/theUser/mySpotExtensions/"
#' holding a function "myPredictLm" 
#' 
#' @seealso Predictors that are shipped with SPOT are: \code{\link{spotPredictLm}}, \code{\link{spotPredictMlegp}} 
#' \code{\link{spotPredictRandomForest}}, \code{\link{spotPredictTree}}, \code{\link{spotPredictTgp}}, please check these 
#' examples for the correct input parameters and the structure of the return value before you include your own predictors\cr
#' The Options of the configuration file (.conf) are described in \code{\link{spotGetOptions}} 
#'##################################################################################
spotPredict... <- function(){

}

#' General Help on Design Creation in SPOT on Algorithm Parameter Tuning...
#'
#' Interfacing the user specific optimization task is the main workload that is to be done by all
#' users that are willing to use SPOT beyond looking into the demos. Although the optimization 
#' task may be of various kind, a frequent use case is that SPOT should optimize the parameters of an algorithm 
#' that optimizes a function. So we need\cr
#' 1) an algorithm - provided as R-script: a file holding the algorithm. \cr
#' 2) the algorithm problem definition (some parameters for to define the algorithm): the (.apd)-file. \cr 
#' 3) the parameters to be optimized, the region of interest - that is: some algorithm parameters with their upper and lower bounds
#' given by the (.roi) file.\cr  
#' 
#' 
#'
#' @seealso  \code{\link{SPOT}} \code{\link{spot}} \code{\link{spotStepInitial}}
#' \code{\link{spotStepSequential}} \code{\link{spotStepRunAlg}} \code{\link{spotStepReport}} 
#' \code{\link{spotGetOptions}} 
#' @keywords internal
###################################################################################
spotAlgStartAlg... <- function(){

}

#' General Help on Design Creation in SPOT...
#'
#' SPOT provides some functions for the creation of a set of design points. Design of Experiment (DoE)
#' and latin hypercube designs (Lhd) are the best known and may be used even to create designs. Two different 
#' steps in SPOT, the initial step and all the sequential steps may use different design creating functions. 
#' To provide and include the users own design creating function, the user must follow these instructions carefully:\cr
#' 1) The function the user wants to include must be an R-function provided in a separated file where the 
#' function name MUST be the same as the filename (just without the file extension .R)\cr
#' 2) use the function \code{\link{spotInstAndLoadPackages}} to add the packages that are required for your
#' function, just make it the first line in your function.\cr 
#' 3) adapt the configuration file(.conf), up to four parameters are to be included/changed:\cr
#' \code{init.design.func="myCreateDesign1"} \cr
#' \code{seq.design.func="myCreateDesign2"} \cr
#' this example assumes the existence of the files "myCreateDesign1.R" and "myCreateDesign2.R" in the 
#' directory "/home/theUser/mySpotExtensions/" holding a function "myCreateDesign1" or "myCreateDesign2"
#' respectively
#' 
#' @seealso Design creating functions that are shipped with SPOT are: \code{\link{spotCreateDesignDoeR3}},
#' \code{\link{spotCreateDesignLhs}},\code{\link{spotCreateDesignFrF2}},\code{\link{spotCreateDesignBasicDoe}}, 
#' please check these  examples for the correct input parameters and the structure of the return value 
#' before you include your own design creating function.\cr
#' The Options of the configuration file (.conf) are described in \code{\link{spotGetOptions}} 
spotCreate... <- function(){

}