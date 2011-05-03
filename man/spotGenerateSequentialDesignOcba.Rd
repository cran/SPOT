\name{spotGenerateSequentialDesignOcba}
\alias{spotGenerateSequentialDesignOcba}
\title{function spotGenerateSequentialDesignOcba()...}
\usage{spotGenerateSequentialDesignOcba(spotConfig)}
\description{function spotGenerateSequentialDesignOcba()}
\details{Creates a new design. Design points are determined with respect to the current result file. 
Number of repeats are adapted according to the OCBA approach. 
Uses the functions \link{spotPrepareData},\link{spotGetRawDataMatrixB},\link{spotGetMergedDataMatrixB},\link{spotWriteLines}
\link{spotWriteBest},\link{spotPlotBst},\link{spotSafelyAddSource},\link{spotOcba}
returns a sequential design to be written to the file <xxx>.des (will be determined from calling function)}
\value{data.frame \code{design} \cr
- \code{design} contains one or more new design points to be calculated}
\arguments{\item{spotConfig}{the list of all parameters is given, but the used ones are: \cr
spotConfig$io.resFileName: is checked for existence. If not found, function fails with error\cr
spotConfig$algSourceSrcPath: needed for the error message \cr
spotConfig$userConfFileName: needed for the error message \cr
spotConfig$io.verbosity: needed for command window output \cr}}
