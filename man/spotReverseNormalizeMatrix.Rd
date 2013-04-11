\name{spotReverseNormalizeMatrix}
\alias{spotReverseNormalizeMatrix}
\title{Reverse Normalize}
\usage{
  spotReverseNormalizeMatrix(y, xmin, xmax, ymin, ymax)
}
\arguments{
  \item{y}{design matrix in norm space}

  \item{ymin}{minimum vector of normalized space}

  \item{ymax}{maximum vector of normalized space}

  \item{xmin}{minimum vector of output space}

  \item{xmax}{maximum vector of output space}
}
\value{
  denormalized design matrix
}
\description{
  Reverse normalization of a design matrix
}
\seealso{
  \code{\link{spotPredictForrester}}
  \code{\link{forrBuilder}}
  \code{\link{spotNormalizeMatrix2}}
  \code{\link{spotNormalizeMatrix}}
}
\keyword{internal}

