\name{spotWriteDes}
\alias{spotWriteDes}
\title{Write Design...}
\usage{spotWriteDes(spotConfig, des)}
\description{Write Design}
\details{help function that simply writes Data to the .des-file

Result/Effects: 
rewrites the design-file for the next call of the \code{\link{spotStepRunAlg}}}
\references{\code{\link{spotStepRunAlg}}}
\arguments{\item{spotConfig}{all parameters, only two are used:
\code{spotConfig$io.columnSep}: the column separator should not be empty for writing table to .des-file
\code{spotConfig$io.desFileName}: the filename the design should be written to}
\item{des}{design provided by any spotCreateDesignXXX()-function}}
