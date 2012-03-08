\name{spotCreateDesignLhd}
\alias{spotCreateDesignLhd}
\title{Create a design based on the number of dimensions and the number of design points}
\usage{
  spotCreateDesignLhd(spotConfig, noDesPoints = NaN,
    retries = NaN)
}
\arguments{
  \item{spotConfig}{list of spotConfiguration}

  \item{noDesPoints}{number of design points, default is
  NaN}

  \item{retries}{number of retries, which is the number of
  trials to find a design with the lowest distance, default
  is NaN}
}
\value{
  matrix \code{design} \cr - \code{design} has
  \code{dimension} columns and \code{noDesPoints} rows with
  entries corresponding to the region of interest.
}
\description{
  The dimension is driven from the number of rows of the
  .roi - file (each row in the roi file defines a variable,
  In case of a missing number of design points a value is
  calcuated from the dimension
}

