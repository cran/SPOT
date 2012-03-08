\name{spotDetermineTheta}
\alias{spotDetermineTheta}
\title{Determine Theta for Forresters Kriging}
\usage{
  spotDetermineTheta(X, y, fit = list())
}
\arguments{
  \item{X}{design matrix}

  \item{y}{vector of observations at X}

  \item{fit}{list of settings and parameters of a forrester
  model}
}
\value{
  a fit, with the found parameters for the model
}
\description{
  Determine Theta for Forresters Kriging
}
\seealso{
  \code{\link{spotPredictForrester}}
  \code{\link{spotNormalizeMatrix2}}
  \code{\link{spotNormalizeMatrix}}
  \code{\link{spotReverseNormalizeMatrix}}
  \code{\link{spotNormalizeMatrix}}
  \code{\link{spotRegPredictor}}
  \code{\link{spotRegLikelihood}}
}
\keyword{internal}

