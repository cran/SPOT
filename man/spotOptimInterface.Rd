\name{spotOptimInterface}
\alias{spotOptimInterface}
\title{function call for spotOptim...}
\usage{spotOptimInterface(spotConfig)}
\description{function call for spotOptim}
\details{SPOT uses this function to call functions passed to \code{\link{spotOptim}} or \code{\link{spot}} like they would be passed to optim().
That means, it will be used whenever an actual function is passed instead of a string. When a string is passed
the string itself will contain the interface to use.
This function is needed as an interface, to ensure the right information
are passed from SPOT to the target function.}
\value{this function returns the \code{spotConfig} list with the results in spotConfig$alg.currentResult}
\references{\code{\link{SPOT}} \code{\link{spot}} \code{\link{demo}} \code{\link{optim}}
\code{\link{spotFuncStartBranin}}}
\arguments{\item{spotConfig}{Contains the list of spot configurations, results of the algorithm can be passed to this list instead of the .res file.
spotConfig defaults to "NA", and will only be passed to the Algorithm if spotConfig$spot.fileMode=FALSE. See also: \code{\link{spotGetOptions}}
Items used are: \cr \cr
alg.currentDesign: data frame holding the design points that will be evaluated \cr
io.apdFileName: name of the apd file \cr
io.desFileName: name of the des file \cr
io.resFileName: name of the res file, for logging results (if spotConfig$spot.fileMode==TRUE)\cr
spot.fileMode: boolean, if selected with true the results will also be written to the res file, otherwise it will only be saved in the spotConfig returned by this function\cr}}
