\name{spotReadRoi}
\alias{spotReadRoi}
\title{Spot Read ROI}
\usage{
  spotReadRoi(roiFile, sep, verbosity = 0)
}
\arguments{
  \item{roiFile}{this file contains the roi}

  \item{sep}{this is used as a column separator}

  \item{verbosity}{\code{io.verbosity} for this specified
  output string}
}
\value{
  data.frame \code{aroi} \cr - \code{roi} contains the data
  from the roi file
}
\description{
  help function spotReadRoi reads region of interest from
  the .roi-file
}

