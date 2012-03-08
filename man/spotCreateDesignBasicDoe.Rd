\name{spotCreateDesignBasicDoe}
\alias{spotCreateDesignBasicDoe}
\title{Create a full factorial design based on the number of dimensions and the number of design points}
\usage{
  spotCreateDesignBasicDoe(spotConfig, noDesPoints = 100,
    repeats = 1)
}
\arguments{
  \item{spotConfig}{list of spotConfiguration, where only
  the table $alg.roi (.roi-file) is used,}

  \item{noDesPoints}{optional parameter gives the number of
  design points. If noDesPoints is larger than the size of
  full factorial design, an error is reported.}

  \item{repeats}{is obsolete for "doe" for dow gives a
  deterministic return value. The parameter is given to
  meet the standard interface to design creating functions
  (default is 1)}
}
\value{
  Matrix \code{M} \cr - \code{M} has \code{dimension}
  columns and \code{noDesPoints} rows
}
\description{
  Uses the function gen.factorial from the AlgDesign
  package.
}
\details{
  The dimension is driven from the number of rows of the
  .roi - file (each row in the roi file defines a variable,
  In case of a missing number of design points a value is
  calcuated from the dimension
}

