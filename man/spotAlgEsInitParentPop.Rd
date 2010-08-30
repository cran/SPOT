\name{spotAlgEsInitParentPop}
\alias{spotAlgEsInitParentPop}
\title{spotAlgEsInitParentPop...}
\usage{spotAlgEsInitParentPop(sigmaInit, dimension, nSigma, noise, fName, gen,
    low, high, mue)}
\description{spotAlgEsInitParentPop}
\details{Creates initial parent population}
\value{matrix \cr
- holds the parent population created by this function}
\references{\code{\link{spotAlgEs}} \code{\link{spotAlgEsIndividualInitial}}}
\arguments{\item{sigmaInit}{initial sigma value (standard deviation)}
\item{dimension}{number of target function dimension}
\item{nSigma}{number of standard deviations}
\item{noise}{noise to be added to fitness value}
\item{fName}{function name}
\item{gen}{generation}
\item{low}{lower limit}
\item{high}{upper limit}
\item{mue}{number of parents in the ES}}
