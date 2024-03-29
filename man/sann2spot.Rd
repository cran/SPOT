% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spotTools.R
\name{sann2spot}
\alias{sann2spot}
\title{Interface SANN to SPOT}
\usage{
sann2spot(algpar, par = c(10, 10), fn, maxit = 100, ...)
}
\arguments{
\item{algpar}{\code{matrix} algorithm parameters.}

\item{par}{Initial values for the parameters to be optimized over.}

\item{fn}{A function to be minimized (or maximized),
with first argument the vector of parameters over which minimization is to take place. It should return a scalar result.}

\item{maxit}{Total number of function evaluations: there is no other stopping criterion. Defaults to 10000.}

\item{...}{further arguments for \code{optim}}
}
\value{
\code{matrix} of results (performance values)
}
\description{
Provide an interface for tuning SANN.
The interface function receives a \code{matrix} where each row is  proposed parameter setting (`temp`, `tmax`),
and each column specifies the parameters.
It generates a $(n,1)$-matrix as output, where $n$ is the number of (`temp`, `tmax`) parameter settings.
}
\examples{
sphere <- function(x){sum(x^2)}
algpar <- matrix(c(1:10, 1:10), 10,2)
sann2spot(algpar, fn = sphere)

}
