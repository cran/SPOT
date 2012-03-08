\name{spotCreateDesignFrF2}
\alias{spotCreateDesignFrF2}
\title{Create a resolution III design with center point
based on the number of dimensions and the number of design points}
\usage{
  spotCreateDesignFrF2(spotConfig, noDesPoints = NaN,
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
  columns and \code{noDesPoints} rows
}
\description{
  Uses the function FrF2 from the FrF2-package to generate
  a (fractional) factorial design
}
\details{
  The dimension is determined from the number of rows of
  the .roi - file (each row in the roi file defines a
  variable).
}

