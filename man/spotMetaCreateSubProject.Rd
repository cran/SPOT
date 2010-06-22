\name{spotMetaCreateSubProject}
\alias{spotMetaCreateSubProject}
\title{Spot Meta Creat Sub Project...}
\usage{spotMetaCreateSubProject(spotConfig, projectName, apdLines)}
\description{Spot Meta Creat Sub Project}
\details{Help function that simply creates a project- sub-directory 
for one line of the factorial design for the .meta-file and
copies the .roi, the .apd and the .conf file to that subDir.
It is only called from \code{\link{spotStepMetaOpt}}}
\value{string \cr
- holding the directory created}
\arguments{\item{spotConfig}{spotConfig$io.apdFilename, spotConfig$io.roiFileName 
and spotConfig$configFileName are used for copying}
\item{projectName}{name derived from the factors of ONE row of the factorial design
holding all the parameters for the Meta calls}
\item{apdLines}{a vector of lines to be added to the apdFile that is copied here}}
