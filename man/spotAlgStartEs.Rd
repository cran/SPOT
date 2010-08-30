\name{spotAlgStartEs}
\alias{spotAlgStartEs}
\title{ES function call for SPOT...}
\usage{spotAlgStartEs(io.apdFileName, io.desFileName, io.resFileName)}
\description{ES function call for SPOT}
\details{SPOT uses this function for some demos to call an Evolution Strategy. The ES can use 
different fitness functions. The results are written to the res file.
This function is needed as an interface, to ensure the right information
are passed from SPOT to the target algorithm (e.g. the ES) and vice versa.}
\references{\code{\link{SPOT}} \code{\link{spotAlgEs}} \code{\link{spotAlgEsF}}}
\arguments{\item{io.apdFileName}{name of the apd file}
\item{io.desFileName}{name of the des file}
\item{io.resFileName}{name of the res file}}
