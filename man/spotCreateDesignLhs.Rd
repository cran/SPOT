\name{spotCreateDesignLhs}
\alias{spotCreateDesignLhs}
\title{spotCreateDesignLhs}
\usage{
  spotCreateDesignLhs(spotConfig, noDesPoints = NaN,
    repeats = NaN)
}
\arguments{
  \item{spotConfig}{list of spotConfiguration}

  \item{noDesPoints}{number of design points, default is
  NaN}

  \item{repeats}{number of repeats, default is NaN}
}
\value{
  matrix \code{M} \cr - \code{M} has \code{dimension}
  columns and \code{noDesPoints} rows with entries
  corresponding to the region of interest.
}
\description{
  Uses the improvedLHS function from the lhs package to
  create a Latin Hypercube Design. improvedLHS optimizes
  the euclidean distance between points in the sample.
}
\seealso{
  \code{\link{spotCreateDesignBasicDoe}},
  \code{\link{spotCreateDesignFrF2}},
  \code{\link{spotCreateDesignLhd}},
  \code{\link{spotCreateDesignLhsOpt}}
}

