% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/buildBO.R
\name{buildBO}
\alias{buildBO}
\title{Bayesian Optimization Model Interface}
\usage{
buildBO(x, y, control = list())
}
\arguments{
\item{x}{matrix of input parameters. Rows for each point, columns for each parameter.}

\item{y}{one column matrix of observations to be modeled.}

\item{control}{list of control parameters}
}
\value{
an object of class \code{"spotBOModel"}, 
with a \code{predict} method and a \code{print} method.
}
\description{
Bayesian Optimization Model Interface
}
