\name{spotPrepareSystem}
\alias{spotPrepareSystem}
\title{SPOT Prepare System - loads all required packages for SPOT...}
\description{SPOT Prepare System - loads all required packages for SPOT}
\details{installs and loads all packages that are needed for the core functionality of SPOT
(hard coded in the function). All user defined Plugins needs to call
\code{\link{spotInstAndLoadPackages}} to add their dependencies properly\cr
This function is only provided for use in non-packaged version, for all packages 
are listed in the "Depends line" of DESCRIPTION}
