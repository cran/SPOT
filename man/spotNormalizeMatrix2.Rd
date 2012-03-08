\name{spotNormalizeMatrix2}
\alias{spotNormalizeMatrix2}
\title{Normalize design}
\usage{
  spotNormalizeMatrix2(x, ymin, ymax, xmin, xmax)
}
\arguments{
  \item{x}{design matrix in input space}

  \item{ymin}{minimum vector of normalized space}

  \item{ymax}{maximum vector of normalized space}

  \item{xmin}{minimum vector of input space}

  \item{xmax}{maximum vector of input space}
}
\value{
  normalized design matrix
}
\description{
  Normalize design with given maximum and minimum in input
  space
}
\seealso{
  \code{\link{spotPredictForrester}}
  \code{\link{spotDetermineTheta}}
  \code{\link{spotNormalizeMatrix}}
  \code{\link{spotReverseNormalizeMatrix}}
}
\keyword{internal}

