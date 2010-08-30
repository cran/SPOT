\name{spotWriteLines}
\alias{spotWriteLines}
\title{Spot Write Lines...}
\usage{spotWriteLines(set.io.verbosity, io.verbosity, myString, con=stderr())}
\description{Spot Write Lines}
\details{This help function writes the string given in "myString" 
only if user gives the io.verbosity to do so}
\references{\code{\link{SPOT}} \code{\link{writeLines}}}
\arguments{\item{set.io.verbosity}{\code{spotConfig$io.verbosity} should be passed here. Global flag to drive the \code{io.verbosity} of the programm}
\item{io.verbosity}{\code{io.verbosity} for this specified output string}
\item{myString}{the string to be written to stdout}
\item{con}{defines the output stream, defaults to \code{stderr()}}}
