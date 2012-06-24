\name{spotRegLikelihood}
\alias{spotRegLikelihood}
\title{Calculate negative log-likelihood}
\usage{
  spotRegLikelihood(x, AX, Ay)
}
\arguments{
  \item{x}{vector, containing log(theta) and lambda}

  \item{AX}{3 dimensional array, constructed by
  spotDetermineTheta from the sample locations}

  \item{Ay}{vector of observations at sample locations}
}
\value{
  list with elements\cr \code{NegLnLike} concentrated
  log-likelihood *-1 for minimising \cr \code{Psi}
  correlation matrix\cr \code{Psinv} inverse of correlation
  matrix (to save computation time in spotRegPredictor)\cr
  \code{mu} \cr \code{ssq}
}
\description{
  Used to determine theta/lambda values for the Kriging
  model in \code{\link{spotDetermineTheta}}.
}
\seealso{
  \code{\link{spotPredictForrester}}
  \code{\link{spotDetermineTheta}}
}
\keyword{internal}

