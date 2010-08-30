\name{spotNoisyBraninFunction}
\alias{spotNoisyBraninFunction}
\title{noisy Branin function...}
\usage{spotNoisyBraninFunction(x, noise=0)}
\description{noisy Branin function}
\details{This is the noisy implementation of the Branin function used by some SPOT demos}
\value{number \code{y} \cr
- \code{y} is the function value of the corresponding vector \code{x}}
\references{\code{\link{SPOT}} \code{\link{spot}} \code{\link{demo}} \code{\link{spotFuncStartBranin}}
\code{\link{spotFuncStartBranin}}  \code{\link{rnorm}} \code{\link{spotAlgEsF}}}
\arguments{\item{x}{two dimensional vector that will be evauluated by the branin function}
\item{noise}{this number is multiplied with \code{rnorm(1)}, the result is added to the function value.}}
