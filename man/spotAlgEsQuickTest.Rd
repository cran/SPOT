\name{spotAlgEsQuickTest}
\alias{spotAlgEsQuickTest}
\title{ES Quick Test}
\usage{
  spotAlgEsQuickTest(dimension = 2, mutation = 2,
    sigmaInit = 1, nSigma = 1, tau0 = 1.5, tau = 1,
    rho = "bi", stratReco = 1, objReco = 1, kappa = -1,
    maxGen = Inf, maxIter = 200, seed = 123, noise = 0,
    fName = spotBraninFunction, lowerLimit = -1,
    upperLimit = 1, mue = 2, nu = 2, plotResult = TRUE,
    logPlotResult = FALSE, verbosity = 1,
    sigmaRestart = 0.2, prescanmult = 1)
}
\arguments{
  \item{mue}{value of mue (number of parents) to be used
  with the ES default is 2}

  \item{nu}{value of nu to be used with the ES default is
  2}

  \item{tau}{value of tau (learning parameter) to be used
  with the ES default is 1}
}
\description{
  This is a quick test function for the Evolution Strategy
  algorithm.
}
\seealso{
  \code{\link{spotAlgEs}} \code{\link{spotAlgStartEs}}
}
\keyword{internal}

