\name{spotCreateDesignLhs}
\alias{spotCreateDesignLhs}
\title{Create a resolution III design with center point
based on the number of dimensions and the number of design points}
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
  Uses the function maxminLHS from the lhs to generate a
  Latin Hypercube Design. This function attempts to
  optimize the sample by maximizing the minium distance
  between design points (maximin criteria).
}
\details{
  The dimension is determined from the number of rows of
  the .roi - file (each row in the roi file defines a
  variable).
}

