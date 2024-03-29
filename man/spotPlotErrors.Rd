% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reportSeverity.R
\name{spotPlotErrors}
\alias{spotPlotErrors}
\title{spotPlotErrors}
\usage{
spotPlotErrors(
  alternative = "greater",
  lower = -3,
  upper = 3,
  mu0 = 0,
  mu1 = 1,
  sigma = 1,
  n = NULL,
  xbar = 0,
  alpha = 0.05,
  beta = NULL
)
}
\arguments{
\item{alternative}{One of greater, less, or two.sided. The greater is the default.}

\item{lower}{lower limit of the plot}

\item{upper}{upper limit of the plot}

\item{mu0}{mean of the null}

\item{mu1}{mean of the alternative. See also parameter \code{beta}.}

\item{sigma}{standard deviation}

\item{n}{sample size}

\item{xbar}{observed mean}

\item{alpha}{error of the first kind}

\item{beta}{error 2nd kind. Default \code{NULL}. If specified, then parameter 
\code{mu1} will be ignored and \code{mu1} will be calculated based on
\code{beta}.}
}
\value{
description of return value
}
\description{
Visualize the alpha, beta errors and the power of the test
}
\examples{
spotPlotErrors(lower=490,upper=510,mu0=500,mu1=504,sigma=2.7,n=9,xbar=502.22)
spotPlotErrors(lower=140,upper=155,mu0=150,mu1=148,sigma=10,n=100,xbar=149,alternative="less")
}
