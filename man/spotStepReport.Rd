\name{spotStepReport}
\alias{spotStepReport}
\title{SPOT Step Report...}
\usage{spotStepReport(spotConfig)}
\description{SPOT Step Report}
\details{Forth and last step for SPOT, that is by default a call of \link{spotReportDefault}

This step provides a very basic report about the .res-file, based on settings in the \code{spotConfig}
The used parameters of \code{spotConfig} are just \code{spotConfig$report.func} and code{spotConfig$report.path}
specifying which report shall be called. The user can specify his own report and should set the 
values {report.func} and {report.path} in the configuration file according to the specification rules
given. If nothing is set, the default report is used.}
\references{\code{\link{SPOT}} \code{\link{spot}} \link{spotReportDefault} \code{\link{spotGetOptions}}}
\arguments{\item{spotConfig}{the list of all parameters is given, it is forwarded to the call of the reportfunction}}
