\name{spotPrepare}
\alias{spotPrepare}
\title{Prepares the configuration (spotConfig) for SPOT...}
\usage{spotPrepare(srcPath, configFile)}
\description{Prepares the configuration (spotConfig) for SPOT}
\details{Set some globally important parameters for SPOT to run, creating the
parameter list (spotConfig) for SPOT from given usersConfigFile  (.conf -file)}
\note{For developers: this function also manages the include of all functions needed - 
in the packaged version this is already done when package is installed.}
\value{list \code{spotConfig} \cr
- \code{spotConfig} is the list of spot parameters created by this function}
\references{\code{\link{SPOT}} \code{\link{spotGetOptions}}}
\arguments{\item{srcPath}{the absolute path to the SPOT sources}
\item{configFile}{the absolute path including filespecifier}}
