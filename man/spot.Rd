\name{spot}
\alias{spot}
\title{Main function for the use of SPOT...}
\usage{spot(configFile="NULL", spotTask="auto", srcPath=NA, spotConfig=NA)}
\description{Main function for the use of SPOT}
\details{Sequential Parameter Optimization Toolbox (SPOT) provides a toolbox for the 
sequential optimization of parameter driven tasks. 
Use \code{\link{spotOptim}} for a \code{\link{optim}} like interface

The path given with the \code{userConfigFile} also fixes the working directory used
throughout the run of all SPOT functions. All files that are needed for input/output
can and will be given relative to the path of the userConfigFile (this also holds for 
the binary of the algorithm). This refers to files that are specified in the configFile
by the user. 

It is of major importance to understand that spot by default expects to optimize noisy functions. That means, the default settings of spot,
which are also used in spotOptim, include repeats of the initial and sequentially created design points. Also, as a default OCBA
is used to spread the design points for optimal usage of the function evaluation budget. OCBA will not work when there is no variance in the data.
So if the user wants to optimize non-noisy functions, the following settings should be used:\cr
\code{spotConfig$spot.ocba <- FALSE}\cr
\code{spotConfig$seq.design.maxRepeats <- 1}\cr
\code{spotConfig$init.design.repeats <- 1}\cr}
\note{\code{spot()} expects char vectors as input, e.g. \code{spot("c:/configfile.conf","auto")}}
\references{\code{\link{SPOT}} \code{\link{spotOptim}} \code{\link{spotStepAutoOpt}}  \code{\link{spotStepInitial}}
\code{\link{spotStepSequential}} \code{\link{spotStepRunAlg}} \code{\link{spotStepReport}} 
\code{\link{spotPrepare}} \code{\link{spotPrepareSystem}}}
\arguments{\item{configFile}{the absolute path including filespecifier, there is no default, this value should always be given}
\item{spotTask}{[init|seq|run|auto|rep] the switch for the tool used, default is "auto"}
\item{srcPath}{the absolute path to user written sources that extend SPOT, the default(NA) will search for sources in the path <.libPath()>/SPOT/R}
\item{spotConfig}{a list of parameters used to configure spot, default is spotConfig=NA, which means the configuration will only be read from the \code{configFile}, not given by manual user input. 		
Notice that parameters given in spotConfig will overwrite both default values assigned by SPOT, AND values defined in the Config file
However, values not passed by spotConfig will still be used as defaults. If you want to see those defaults, look at \code{\link{spotGetOptions}}}}
