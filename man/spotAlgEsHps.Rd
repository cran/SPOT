\name{spotAlgEsHps}
\alias{spotAlgEsHps}
\title{Termination hps...}
\usage{spotAlgEsHps(xk, xOpt)}
\description{Termination hps}
\details{Termination function for the ES. Terminates the ES at a given target value.}
\value{\code{boolean} \cr
- TRUE as long as the current value has not yet reached its limit. Once the
given termination criterion is reached the function returns FALSE.}
\arguments{\item{xk}{current best value of the optimization run}
\item{xOpt}{target value of the optimization run}}
