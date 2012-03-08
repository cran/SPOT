\name{spotGetRawResData}
\alias{spotGetRawResData}
\title{Get Raw Result Data}
\usage{
  spotGetRawResData(spotConfig)
}
\arguments{
  \item{spotConfig}{the list of all parameters is given,
  used ones are: \code{spotConfig$io.resFileName:} result
  file where data are read from
  \code{spotConfig$io.columnSep:} column separator as
  determined by getConfig parameter to the function call
  \code{\link{spotPrepareData}}}
}
\value{
  data.frame \code{rawResData} \cr - \code{rawResData}
  contains values from the result file
}
\description{
  spotGetRawResData is based on
  \code{\link{spotPrepareData}}
}
\details{
  The result (.res) file is read and the data is returned
  as is
}
\seealso{
  \code{\link{spotPrepareData}}
}

