\name{spotAlgStartEsVar}
\alias{spotAlgStartEsVar}
\title{Interface for an Evolution Strategy to be robustly tuned by SPOT...}
\usage{
  spotAlgStartEsVar(spotConfig)
}
\arguments{
  \item{spotConfig}{Contains the list of spot
  configurations, results of the algorithm can be passed to
  this list instead of the .res file.  spotConfig defaults
  to "NA", and will only be passed to the Algorithm if
  spotConfig$spot.fileMode=FALSE. See also:
  \code{\link{spotGetOptions}} Items used are: \cr \cr
  alg.currentDesign: data frame holding the design points
  that will be evaluated \cr io.apdFileName: name of the
  apd file \cr io.desFileName: name of the des file \cr
  io.resFileName: name of the res file, for logging results
  (if spotConfig$spot.fileMode==TRUE)\cr spot.fileMode:
  boolean, if selected with true the results will also be
  written to the res file, otherwise it will only be saved
  in the spotConfig returned by this function\cr}
}
\value{
  this function returns the \code{spotConfig} list with the
  results in spotConfig$alg.currentResult
}
\description{
  This Evolution Strategy implementation can be tuned by
  SPOT. The ES can use different fitness functions. The
  results are written to the res file. This function is
  needed as an interface, to ensure the right information
  are passed from SPOT to the target algorithm (e.g. the
  ES) and vice versa. In contrast to
  \code{\link{spotAlgStartEs}} it is an interface for
  Pareto optimization, to optimize both the performance as
  well as the variance of the ES algorithm, to reach more
  robust results.
}
\seealso{
  \code{\link{SPOT}} \code{\link{spotAlgEs}}
  \code{\link{spotAlgStartEs}}
}

