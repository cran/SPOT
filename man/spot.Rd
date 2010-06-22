\name{spot}
\alias{spot}
\title{SPOT main function for the use of the SPO-Toolbox...}
\usage{spot(configFile, spotTask="auto", srcPath=NA)}
\description{SPOT main function for the use of the SPO-Toolbox}
\details{Sequential Parameter Optimization Toolbox (SPOT) provides a toolbox for the 
sequential optimization of parameter driven tasks  
MUST be called with at least the first parameter specified (configFile)

The path given with the \code{userConfigFile} also fixes the working directory used
throughout the run of all SPOT functions. All files that are needed for input/output
can and will be given relative to the path of the userConfigFile (this also holds for 
the binary of the algorithm)}
\note{\code{spot} expects char vectors as input}
\references{\code{\link{SPOT}} \code{\link{spot}} \code{\link{spotStepAutoOpt}}  \code{\link{spotStepInitial}}
\code{\link{spotStepSequential}} \code{\link{spotStepRunAlg}} \code{\link{spotStepReport}} 
\code{\link{spotPrepare}} \code{\link{spotPrepareSystem}}}
\arguments{\item{configFile}{the absolute path including filespecifier, there is no default, this value MUST be given.}
\item{spotTask}{[init|seq|run|auto|rep] the switch for the tool used, default is "auto"}
\item{srcPath}{the absolute path to user written sources that extend SPOT, the default(NA) will search for sources in the path <.libPath()>/SPOT/R}}
