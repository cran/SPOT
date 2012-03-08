\name{spotAlgStartAlg...}
\alias{spotAlgStartAlg...}
\title{General Help on Design Creation in SPOT on Algorithm Parameter Tuning...}
\usage{
  spotAlgStartAlg...()
}
\description{
  Interfacing the user specific optimization task is the
  main workload that is to be done by all users that are
  willing to use SPOT beyond looking into the demos.
  Although the optimization task may be of various kind, a
  frequent use case is that SPOT should optimize the
  parameters of an algorithm that optimizes a function. So
  we need\cr 1) an algorithm - provided as R-script: a file
  holding the algorithm. \cr 2) the algorithm problem
  definition (some parameters for to define the algorithm):
  the (.apd)-file. \cr 3) the parameters to be optimized,
  the region of interest - that is: some algorithm
  parameters with their upper and lower bounds given by the
  (.roi) file.\cr
}
\seealso{
  \code{\link{SPOT}} \code{\link{spot}}
  \code{\link{spotStepInitial}}
  \code{\link{spotStepSequential}}
  \code{\link{spotStepRunAlg}} \code{\link{spotStepReport}}
  \code{\link{spotGetOptions}}
}
\keyword{internal}

