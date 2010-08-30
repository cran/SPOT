\name{spotAlgEsIndividualInitial}
\alias{spotAlgEsIndividualInitial}
\title{Individual Initialization...}
\usage{spotAlgEsIndividualInitial(s, dimension, n, noise=0, fName, gen,
    low=-1, high=1, des)}
\description{Individual Initialization}
\details{Creates a new Individual for the Evolution Strategy implemented in SPOT.}
\value{numeric vector \cr
- contains x value, sigma value, real fitness value, fitness with noise, and generation number}
\references{\code{\link{spotAlgEs}} \code{\link{spotAlgEsF}}}
\arguments{\item{s}{sigma}
\item{n}{n}
\item{dimension}{number of target function dimension}
\item{noise}{noise to be added to fitness value}
\item{fName}{function name}
\item{gen}{generation}
\item{low}{lower limit}
\item{high}{upper limit}
\item{des}{des}}
