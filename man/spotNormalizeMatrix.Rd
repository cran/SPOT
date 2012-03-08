\name{spotNormalizeMatrix}
\alias{spotNormalizeMatrix}
\title{Normalize design}
\usage{
  spotNormalizeMatrix(x, ymin, ymax)
}
\arguments{
  \item{x}{design matrix in input space}

  \item{ymin}{minimum vector of normalized space}

  \item{ymax}{maximum vector of normalized space}
}
\value{
  normalized design matrix
}
\description{
  Normalize design by using minimum and maximum of the
  design values for input space
}
\seealso{
  \code{\link{spotPredictForrester}}
  \code{\link{spotDetermineTheta}}
  \code{\link{spotNormalizeMatrix2}}
  \code{\link{spotReverseNormalizeMatrix}}
}
\keyword{internal}

