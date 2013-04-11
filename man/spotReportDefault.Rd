\name{spotReportDefault}
\alias{spotReportDefault}
\title{Default Report}
\usage{
  spotReportDefault(spotConfig)
}
\arguments{
  \item{spotConfig}{the configuration list of all spot
  parameters}
}
\value{
  list spotConfig with changed values
}
\description{
  Function generates a simple report.
}
\details{
  This function is used when no report function is
  requested by the user. It creates some text output and
  also draws a tree, printing it to screen or pdf. If the
  \code{report.io.pdf} setting is TRUE the graphic is
  printed to a pdf file (usually named like your .conf
  file, and placed in the same folder) if
  \code{report.io.screen} is set TRUE the graphic is
  printed to the screen. Both can be FALSE or TRUE at the
  same time. If the user does not specify those values, the
  defaults will be used as shown in
  \code{\link{spotGetOptions}}, which means there will be
  only screen output, and no pdf.

  In case of multi objective optimization this function
  will report the hyper volume indicator, and write the
  Pareto front and the Pareto set to spotConfig$mco.val and
  spotConfig$mco.par. spotReportDefault is currently the
  only recommendable report function for multi objective
  SPOT, besides custom functions created by users.
}
\seealso{
  \code{\link{spot}}, \code{\link{spotStepReport}}
}

