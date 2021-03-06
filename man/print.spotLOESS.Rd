% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/buildLOESS.R
\name{print.spotLOESS}
\alias{print.spotLOESS}
\title{Print method for loess model}
\usage{
\method{print}{spotLOESS}(x, digits = max(3L, getOption("digits") - 3L), ...)
}
\arguments{
\item{...}{not used}

\item{object}{fit of the model, an object of class \code{"spotLOESS"}, produced by \code{\link{buildLOESS}}.}
}
\description{
Wrapper for \code{summary.loess}.
}
\seealso{
\code{\link{buildLOESS}}
}
\keyword{internal}
