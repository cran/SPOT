\name{spotFuncStartSphere}
\alias{spotFuncStartSphere}
\title{Sphere function call for SPOT...}
\usage{spotFuncStartSphere(spotConfig)}
\description{Sphere function call for SPOT}
\details{SPOT uses this function for some demos to call the \code{\link{spotSphereFunction}} function}
\value{this function returns the \code{spotConfig} list with the results in spotConfig$alg.currentResult}
\references{\code{\link{SPOT}} \code{\link{spot}} \code{\link{demo}} \code{\link{optim}}
\code{\link{spotSphereFunction}}}
\arguments{\item{spotConfig}{Contains the list of spot configurations, results of the algorithm can be passed to this list instead of the .res file.
spotConfig defaults to "NA", and will only be passed to the Algorithm if spotConfig$spot.fileMode=FALSE. See also: \code{\link{spotGetOptions}}
Items used are: \cr \cr
alg.currentDesign: data frame holding the design points that will be evaluated \cr
io.apdFileName: name of the apd file \cr
io.desFileName: name of the des file \cr
io.resFileName: name of the res file, for logging results (if spotConfig$spot.fileMode==TRUE)\cr
spot.fileMode: boolean, if selected with true the results will also be written to the res file, otherwise it will only be saved in the spotConfig returned by this function\cr}}
