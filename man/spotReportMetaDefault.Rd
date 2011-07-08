\name{spotReportMetaDefault}
\alias{spotReportMetaDefault}
\title{Default Report Function for Meta Runs...}
\usage{spotReportMetaDefault(spotConfig)}
\description{Default Report Function for Meta Runs}
\details{Function to generate a simple report for meta runs.

This function draws a scatterplot matrix (based on car), printing it to screen or pdf. 
If the \code{report.io.pdf} setting is TRUE
the graphic is printed to a pdf file (usually named like your .conf file, and placed in the same folder)
if \code{report.io.screen} is set TRUE the graphic is printed to the screen. Both can be FALSE or TRUE
at the same time. If the user doesnt specify those values, the defaults will be used as shown in
\code{\link{spotGetOptions}}, which means there will be only screen output, and no pdf.}
\references{\code{\link{SPOT}} \code{\link{spot}} \code{\link{spotStepReport}}}
\arguments{\item{spotConfig}{the configuration list of all spot parameters}}
