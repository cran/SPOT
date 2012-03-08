\name{spotPrepareData}
\alias{spotPrepareData}
\title{Prepare Data for SPOT}
\usage{
  spotPrepareData(spotConfig)
}
\arguments{
  \item{spotConfig}{the list of all parameters is given,
  used parameters are: \code{spotConfig$io.resFileName}
  \code{spotConfig$io.columnSep} \code{spotConfig$alg.roi}
  \code{spotConfig$alg.resultColumn}
  \code{spotConfig$seq.transformation.func()}
  \code{spotConfig$seq.merge.func()}}
}
\value{
  list \code{resultList} \cr - \code{resultList} is a list
  of values (not sorted in increasing order!)  \code{x}:
  vector of vector (=matrix) of input values, the "points"
  \code{mergedY}: vector of merged fitness values, the
  result column (each row of x has a corresponding Y-value)
  \code{count}: the number of repeats \code{CONFIG}: the
  unique CONFIG number for each unique design point
  (sdev=NA if count=1) \code{pNames}: names of the
  parameters as derived from roi-file \code{step.last}: the
  maximum number of the step-column of the result-file
  \code{STEP}: SPOT STEP when design point was created
}
\description{
  spotPrepareData prepares the data from .res-file
}
\details{
  The result (.res) file is read and the data are prepared
  for further processing results in an unsorted list of
  several values
}
\seealso{
  \code{\link{SPOT}} \code{\link{spot}}
}

