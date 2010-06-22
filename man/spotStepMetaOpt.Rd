\name{spotStepMetaOpt}
\alias{spotStepMetaOpt}
\title{SPOT Step Meta...}
\usage{spotStepMetaOpt(spotConfig)}
\description{SPOT Step Meta}
\details{The \code{meta} task calls spotStepMetaOpt which itself calls the task \code{auto} 
with several different fixed
parameters to provide a mixed optimization mechanism: analyse a fully qualified 
test of some parameters and the intelligent optimization of other parameters.
e.g. the number of the dimension of a problem etc.

To start this step you could for example do this:\cr
\code{spot("configFileName.conf","meta")}\cr 
An additional \code{<configFileName>.meta} is needed. This meta file should have lines with 
vectors of values that are to be given to parameters used in the .apd file. \cr
\code{maxit=c(100,200,300,400,500)}\cr
\code{x0=list(c(1,1),c(2,2),c(5,5),c(10,10))}\cr
will lead to .apd files with each combination for the params starting with:\cr
\code{maxit=100}\cr
\code{x0=c(1,1)}\cr
to\cr
\code{maxit=500}\cr
\code{x0=c(10,10)}\cr
as in this example there are five values for \code{maxit} and four for \code{x0}
a set of 20 different projects is evaluated. The results will be written to 
\code{<configFileName>.fbs}}
\references{\code{\link{spotStepAutoOpt}} 
\code{\link{spotGetOptions}}}
\arguments{\item{spotConfig}{the list of all parameters is given}}
