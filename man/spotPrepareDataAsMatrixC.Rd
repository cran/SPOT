\name{spotPrepareDataAsMatrixC}
\alias{spotPrepareDataAsMatrixC}
\title{Prepare Data As Matrix C}
\usage{
  spotPrepareDataAsMatrixC(spotConfig)
}
\arguments{
  \item{spotConfig}{the list of all parameters is given,
  but used only as input parameter to the function call of
  \code{\link{spotPrepareData}}}
}
\value{
  Matrix \code{C} \cr - \code{C} holdsg a column "y" with
  the results and all columns with column-names derived
  from .roi file (should be the parameters of the
  algorithm) plus the columns "count", "sdev" and "CONFIG"
}
\description{
  The result (.res) file is read and the data are prepared
  for further procesing results in "Matrix C" that consits
  of the y-vector of the results and the x-Matrix of inputs
  plus the columns "count", "sdev" and "CONFIG", that are
  bound together to a matrix C
}
\seealso{
  \code{\link{spotPrepareData}}
}
\keyword{internal}

