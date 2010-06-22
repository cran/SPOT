\name{spotGetRawDataMatrixB}
\alias{spotGetRawDataMatrixB}
\title{Get Raw Data Matrix B...}
\usage{spotGetRawDataMatrixB(spotConfig)}
\description{Get Raw Data Matrix B}
\details{The result (.res) file is read and the data are prepared for further procesing
results in "Matrix B" that consits of the y-vector of the results and the x-Matrix
of inputs that are bound together to a matrix B}
\value{data.frame \code{B}  \cr
- \code{B} holds a column "y" with the results
and all columns with column-names derived from .roi file (should be the parameters
of the algorithm. Values are sorted with respect to the y values (increasing)}
\references{\code{\link{spotPrepareData}}}
\arguments{\item{spotConfig}{the list of all parameters is given, also used as input
parameter to the function call spotGetRawResData(spotConfig) the other ones in use are: 
\code{spotConfig$alg.roi:} the roi data provided as matrix
\code{spotConfig$alg.resultColumn:} name of the result column}}
