\name{spotPrepare}
\alias{spotPrepare}
\title{Prepares the configuration (spotConfig) for SPOT...}
\usage{spotPrepare(srcPath, configFile, spotConfigUser)}
\description{Prepares the configuration (spotConfig) for SPOT}
\details{Set some globally important parameters for SPOT to run, creating the
parameter list (spotConfig) for SPOT from given usersConfigFile  (.conf -file)}
\note{For developers: this function also manages the include of all functions needed - 
in the packaged version this is already done when package is installed.}
\value{list \code{spotConfig} \cr
- \code{spotConfig} is the list of spot parameters created by this function}
\references{\code{\link{SPOT}} \code{\link{spotGetOptions}} \code{\link{spot}}}
\arguments{\item{srcPath}{the absolute path to the SPOT sources}
\item{configFile}{the absolute path including filespecifier}
\item{spotConfigUser}{a list of parameters used to configure spot, usually  spotConfigUser=NA will be passed to this function, which means the configuration will only be read from the \code{configFile}, not given by manual user input. 		
Notice that parameters given in spotConfigUser will overwrite both default values assigned by SPOT, AND values defined in the config file
However, values not passed by spotConfigUser will still be used as defaults. If you want to see those defaults, look at \code{\link{spotGetOptions}}}}
