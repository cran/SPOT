\name{spotStepReport}
\alias{spotStepReport}
\title{SPOT Step Report}
\usage{
  spotStepReport(spotConfig)
}
\arguments{
  \item{spotConfig}{the list of all parameters is given, it
  is forwarded to the call of the report-function}
}
\description{
  Forth and last step for SPOT, that is by default a call
  of \link{spotReportDefault}
}
\details{
  This step provides a very basic report about the
  .res-file, based on settings in the \code{spotConfig} The
  mainly used parameters of \code{spotConfig} is
  \code{spotConfig$report.func}, specifying which report
  shall be called. The user can specify his own report and
  should set the value {report.func} in the configuration
  file according to the specification rules given. If
  nothing is set, the default report is used.
}
\seealso{
  \code{\link{SPOT}} \code{\link{spot}}
  \link{spotReportDefault} \code{\link{spotGetOptions}}
}

