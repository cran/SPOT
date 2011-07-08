\name{spotRosenbrockFunction}
\alias{spotRosenbrockFunction}
\title{noisy Rosenbrock function...}
\usage{spotRosenbrockFunction(x)}
\description{noisy Rosenbrock function}
\details{This is the noisy implementation of the Rosenbrock function used by some SPOT demos}
\value{number \code{y} \cr
- \code{y} is the function value of the corresponding vector \code{x}}
\references{\code{\link{SPOT}} \code{\link{spot}} \code{\link{demo}} \code{\link{spotFuncStartMexicanHat}}
\code{\link{spotFuncStartRosenbrock}}  \code{\link{rnorm}} \code{\link{spotAlgEsF}}

Maple:
f := (1-x)^2+100*(y-x^2)^2
plot3d(f, x = -1.5 .. 1.5, y = -.5 .. 2)}
\arguments{\item{x}{two dimensional vector that will be evauluated by the branin function}}
