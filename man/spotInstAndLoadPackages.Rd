\name{spotInstAndLoadPackages}
\alias{spotInstAndLoadPackages}
\title{Spot install and load required packages...}
\usage{spotInstAndLoadPackages(packageList,
    reposLoc="http://cran.r-project.org")}
\description{Spot install and load required packages}
\details{Help function that installs and loads the packages that are given by a list
new predictors should use this function to install their packages, to make sure
that they are only installed if necessary}
\arguments{\item{packageList}{a list of strings holding the names of the packages that should 
installed if necessary and then loaded for use}
\item{reposLoc}{["http://cran.r-project.org"] a string of the location,from where 
the package is to be downloaded - the default is the cran R-Project page, but 
if special packages are needed from other locations this can be set here too}}
\examples{spotInstAndLoadPackages("rsm")
spotInstAndLoadPackages(c('FrF2',  'DoE.wrapper'))
spotInstAndLoadPackages("rsm","http://cran.r-project.org")}
