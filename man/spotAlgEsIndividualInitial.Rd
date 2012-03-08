\name{spotAlgEsIndividualInitial}
\alias{spotAlgEsIndividualInitial}
\title{Individual Initialization}
\usage{
  spotAlgEsIndividualInitial(s, dimension, n, noise = 0,
    fName, gen, low = -1, high = 1, des)
}
\arguments{
  \item{s}{sigma}

  \item{n}{n}

  \item{dimension}{number of target function dimension}

  \item{noise}{noise to be added to fitness value}

  \item{fName}{target function}

  \item{gen}{generation}

  \item{low}{lower limit}

  \item{high}{upper limit}

  \item{des}{des scaling for placement between low and
  high}
}
\value{
  numeric vector \cr - contains x value, sigma value, real
  fitness value, fitness with noise, and generation number
}
\description{
  Creates a new Individual for the Evolution Strategy
  implemented in SPOT.
}
\seealso{
  \code{\link{spotAlgEs}}
}
\keyword{internal}

