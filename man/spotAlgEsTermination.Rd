\name{spotAlgEsTermination}
\alias{spotAlgEsTermination}
\title{Termination...}
\usage{spotAlgEsTermination(it, maxIt, ge, maxGe, xk, xOpt, term)}
\description{Termination}
\details{Termination function for the ES}
\arguments{\item{it}{iteration}
\item{maxIt}{Maximum number of iterations}
\item{ge}{generation}
\item{maxGe}{Maximum number of generations}
\item{xk}{current best value of the optimization run}
\item{xOpt}{target value of the optimization run}
\item{term}{string that tells the termination criterion type: "gen", "iter" or "hps"}}
