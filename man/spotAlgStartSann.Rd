\name{spotAlgStartSann}
\alias{spotAlgStartSann}
\title{SANN function call for SPOT...}
\usage{spotAlgStartSann(io.apdFileName, io.desFileName, io.resFileName)}
\description{SANN function call for SPOT}
\details{SPOT uses this function for some demos to call the \code{\link{optim}} function with the SANN
method, which means Simulated Annealing. The SANN uses \code{\link{spotFuncStartBranin}} as 
a target function.
This function is needed as an interface, to ensure the right information
are passed from SPOT to the target algorithm(e.g. the SANN) and vice versa.}
\references{\code{\link{SPOT}} \code{\link{spot}} \code{\link{demo}} \code{\link{optim}}
\code{\link{spotFuncStartBranin}}}
\arguments{\item{io.apdFileName}{name of the apd file}
\item{io.desFileName}{name of the des file}
\item{io.resFileName}{name of the res file}}
