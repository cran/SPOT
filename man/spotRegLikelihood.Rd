\name{spotRegLikelihood}
\alias{spotRegLikelihood}
\title{Calculate negative log-likelihood}
\usage{
  spotRegLikelihood(x, AX, Ay)
}
\arguments{
  \item{x}{vector, containing log(theta) and lambda}

  \item{AX}{design matrix (sample locations)}

  \item{Ay}{vector of observations}
}
\value{
  list with elements\cr \code{NegLnLike} concentrated
  log-likelihood *-1 for minimising \cr \code{Psi}
  correlation matrix\cr \code{mu} \cr \code{ssq}
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

