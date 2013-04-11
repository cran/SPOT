\name{spotReportSensY}
\alias{spotReportSensY}
\title{Sensitivity Report Helper Function}
\usage{
  spotReportSensY(B, fit, roi, nsens)
}
\arguments{
  \item{B}{best solution column}

  \item{fit}{random forest fit}

  \item{roi}{internal parameter for the initial region of
  interest, or: \code{spotConfig$alg.roi}}

  \item{nsens}{number of parameters estimated with
  sensitivity}
}
\description{
  Helper function for the sensitivity report
}
\seealso{
  \code{\link{SPOT}} \code{\link{spot}}
  \code{\link{spotStepReport}} \code{\link{spotReportSens}}
}
\keyword{internal}

