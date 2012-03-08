\name{spotPredict...}
\alias{spotPredict...}
\title{General Help on Prediction models in SPOT...}
\usage{
  spotPredict...()
}
\description{
  SPOT provides some predictors to create a meta-model of
  the function or algorithm to be analyzed. Nevertheless
  the user may provide and include his own predictors. To
  solve this task the user must follow these instructions
  carefully:\cr 1) The function the user wants to include
  must be an R-function provided in a separated file where
  the function name MUST be the same as the filename (just
  without the file extension .R)\cr 2) use the function
  \code{\link{spotInstAndLoadPackages}} to add the packages
  that are required for your function, just make it the
  first line in your function.\cr 3) adapt the
  configuration file, two parameters are to be
  included/changed:\cr
  \code{seq.predictionModel.func="myPredictLm"} \cr this
  example assumes the existence of a file "myPredictLm.R"
  in the directory "/home/theUser/mySpotExtensions/"
  holding a function "myPredictLm"
}
\seealso{
  Predictors that are shipped with SPOT are:
  \code{\link{spotPredictLm}},
  \code{\link{spotPredictMlegp}}
  \code{\link{spotPredictRandomForest}},
  \code{\link{spotPredictTree}},
  \code{\link{spotPredictTgp}}, please check these examples
  for the correct input parameters and the structure of the
  return value before you include your own predictors\cr
  The Options of the configuration file (.conf) are
  described in \code{\link{spotGetOptions}}
  ##################################################################################
}

