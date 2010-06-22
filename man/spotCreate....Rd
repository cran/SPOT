\name{spotCreate...}
\alias{spotCreate...}
\title{SPOT Create.}
\description{SPOT Create...}
\details{SPOT provides some functions for the creation of a set of design points. Design of Experiment (DoE)
and latin hypercube designs (Lhd) are the best known and may be used even to create designs. Two different 
steps in SPOT, the initial step and all the sequential steps may use different design creating functions. 
To provide and include the users own design creating function, the user must follow these instructions carefully:\cr
1) The function the user wants to include must be an R-function provided in a separated file where the 
function name MUST be the same as the filename (just without the file extension .R)\cr
2) use the function \code{\link{spotInstAndLoadPackages}} to add the packages that are required for your
function, just make it the first line in your function.\cr 
3) adapt the configuration file(.conf), up to four parameters are to be included/changed:\cr
\code{init.design.func="myCreateDesign1"} \cr
\code{init.design.path="/home/theUser/mySpotExtensions/"} \cr
\code{seq.design.func="myCreateDesign2"} \cr
\code{seq.design.path="/home/theUser/mySpotExtensions/"} \cr
this example assumes the existence of the files "myCreateDesign1.R" and "myCreateDesign2.R" in the 
directory "/home/theUser/mySpotExtensions/" holding a function "myCreateDesign1" or "myCreateDesign2"
respectively}
\references{Design creating functions that are shipped with SPOT are: \code{\link{spotCreateDesignDoeR3}},
\code{\link{spotCreateDesignLhs}},\code{\link{spotCreateDesignFrF2}},\code{\link{spotCreateDesignBasicDoe}}, 
please check these  examples for the correct input parameters and the structure of the return value 
before you include your own design creating function.\cr
The Options of the configuration file (.conf) are described in \code{\link{spotGetOptions}}}
