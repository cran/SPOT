\name{spotAlgEsF}
\alias{spotAlgEsF}
\title{Function Call to Fitness functions...}
\usage{spotAlgEsF(x, fType)}
\description{Function Call to Fitness functions}
\details{This function is used by others to call the different fitness functions:
\code{\link{spotAlgEsNoisyBraninFunction}}, \code{\link{spotAlgEsSphere}}, \code{\link{spotAlgEsSphere1}}, \code{\link{spotAlgEsBanana}}, \code{\link{spotAlgEsWild}}, \code{\link{spotAlgEsRastrigin}}.}
\value{number \code{y} \cr
- \code{y} is the value of the corresponding \code{x} vector}
\arguments{\item{x}{vector to be evaluated by the fitness function}
\item{fType}{name of fitness function, choose by \code{branin, sphere, sphere1, banana, wild, rastrigin}}}
