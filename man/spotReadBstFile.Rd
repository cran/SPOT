\name{spotReadBstFile}
\alias{spotReadBstFile}
\title{Read .bst File}
\usage{
  spotReadBstFile(spotConfig)
}
\arguments{
  \item{spotConfig}{the list of all parameters is given,
  used here \code{spotReadBstFile}: the name pf the .bst
  file to be read \code{spotConfig$io.columnSep}: Separator
  for the columns}
}
\value{
  data.frame \code{bstData} \cr - \code{bstData} holds a
  column "y" with the results and all columns with
  column-names derived from .roi file (should be the
  parameters of the algorithm
}
\description{
  This function reads the .bst file of the current project.
  The data is used for plots and reports. Will usually not
  be used if no result/best files are written.
}
\seealso{
  \code{\link{SPOT}} \code{\link{spot}}
  \code{\link{spotPlotBst}}
}

