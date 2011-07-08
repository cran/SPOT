\name{spotAlgEsTermination}
\alias{spotAlgEsTermination}
\title{Termination...}
\usage{spotAlgEsTermination(it, maxIt, ge, maxGe, xk, xOpt, term)}
\description{Termination}
\details{Handles the termination functions for the ES.}
\value{\code{boolean} \cr
- TRUE as long as the current value has not yet reached its limit. Once the
given termination criterion is reached the function returns FALSE.}
\references{\code{\link{spotAlgEs}} \code{\link{spotAlgEsHps}}}
\arguments{\item{it}{iteration}
\item{maxIt}{Maximum number of iterations}
\item{ge}{generation}
\item{maxGe}{Maximum number of generations}
\item{xk}{current best value of the optimization run}
\item{xOpt}{target value of the optimization run}
\item{term}{string that tells which termination criterion matters: \code{"gen"} terminates after given number of generations.
\code{"iter"} terminates after given number of iterations.  \code{"hps"} terminates at fiven target value.}}
