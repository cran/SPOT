\name{spotNormDesign}
\alias{spotNormDesign}
\title{spotNormDesign}
\usage{
  spotNormDesign(dim, size, calcMinDistance = FALSE)
}
\arguments{
  \item{dim}{number, dimension of the problem (will be no.
  of columns of the result matrix)}

  \item{size}{number of points with that dimension needed.
  (will be no. of rows of the result matrix)}

  \item{calcMinDistance}{Boolean to indicate whether a
  minimal distance should be calculated}
}
\value{
  list \code{L} \cr - \code{L} consists of a matrix and nd
  (if required) a minimal distance
}
\description{
  Produces a normalized design and calculates the minimal
  distance if required. A design is a matrix with
  \code{dim} columns and \code{size} rows.
}
\author{
  Christian Lasarczyk
}
\seealso{
  This function is used as a basis for
  \code{\link{spotCreateDesignLhd}}.
}
\keyword{internal}

