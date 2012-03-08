\name{spotGetMergedDataMatrixB}
\alias{spotGetMergedDataMatrixB}
\title{Get Merged Data Matrix B}
\usage{
  spotGetMergedDataMatrixB(mergedData, spotConfig)
}
\arguments{
  \item{mergedData}{the Data prepared as done with
  \code{\link{spotPrepareData}}}

  \item{spotConfig}{the list of all parameters is given,
  but used only as input parameter to the function call of
  code{\link{spotPrepareData}}}
}
\value{
  data.frame \code{B} \cr - \code{B} holds a column "y"
  with the results and all columns with column-names
  derived from .roi file (should be the parameters of the
  algorithm. Values are sorted with respect to the y values
  (increasing)
}
\description{
  The merged data that are the result of
  \code{\link{spotPrepareData}}must be the first input
  parameter These data prepared and results in "Matrix B"
  that consits of the y-vector of the results and the
  x-Matrix of inputs that are bound together to a matrix B
}
\seealso{
  \code{\link{spotPrepareData}}
}

