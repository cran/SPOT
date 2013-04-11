\name{spotEnsembleSummedRankWeighted}
\alias{spotEnsembleSummedRankWeighted}
\title{spotEnsembleSummedRankWeighted}
\usage{
  spotEnsembleSummedRankWeighted(x, error, size)
}
\arguments{
  \item{x}{result structure: a list with element \code{Y}
  for predicted values and \code{fits} for model fit list}

  \item{error}{mean squared prediction errors}

  \item{size}{size of design to be modeled}
}
\value{
  the summed voted order, weighted by error
}
\description{
  Get the voting result for each point. Summed over all
  models.
}
\keyword{internal}

