\name{spotCreateDesignDoeR3}
\alias{spotCreateDesignDoeR3}
\title{Create a factorial design with resolution III...}
\usage{spotCreateDesignDoeR3(spotConfig, noDesPoints=100, repeats=1)}
\description{Create a factorial design with resolution III}
\details{Uses the function FrF2 from the FrF2-package to generate a (fractional) factorial design

The dimension is determined from the number of rows of the .roi - file 
(each row in the roi file defines a variable).}
\value{matrix \code{M} \cr
- \code{M} has \code{dimension} columns and \code{noDesPoints} rows}
\arguments{\item{spotConfig}{list of spotConfiguration, where only the table $alg.roi (.roi-file) is used,}
\item{noDesPoints}{optional parameter gives the number of design points}
\item{repeats}{is obsolete for "doe" for dow gives a deterministic reurn value. The parameter is given to meet the standard interface to design creating functions (default is 1)}}
