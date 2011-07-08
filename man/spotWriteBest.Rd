\name{spotWriteBest}
\alias{spotWriteBest}
\title{Spot Write Best...}
\usage{spotWriteBest(B, spotConfig)}
\description{Spot Write Best}
\details{Help function that simply writes data to the .bst-file 
(appending or creating - depends on the existance of the .bst-file)

Result/Effects: 
adds  one row to the best file}
\arguments{\item{B}{matrix}
\item{spotConfig}{all parameters, the only one of interest is the name of the
file for the best-data to be stored in: \code{spotConfig$io.bstFileName}}}
