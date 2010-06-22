\name{spotReadAroi}
\alias{spotReadAroi}
\title{Spot Read Aroi...}
\usage{spotReadAroi(spotConfig)}
\description{Spot Read Aroi}
\details{help function spotReadAroi reads actual region of interest from the .aroi-file}
\value{data.frame \code{aroi} \cr
- \code{aroi} contains the data from the aroi file}
\arguments{\item{spotConfig}{all parameters, only two are used: \cr
\code{spotConfig$io.columnSep}: the column separator should not be empty for writing table to .des-file \cr
\code{spotConfig$io.aroiFileName}: the filename the design should be read from}}
