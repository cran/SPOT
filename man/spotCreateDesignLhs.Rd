\name{spotCreateDesignLhs}
\alias{spotCreateDesignLhs}
\title{Create a resolution III design with center point...}
\usage{spotCreateDesignLhs(spotConfig, noDesPoints=NaN, repeats=NaN)}
\description{Create a resolution III design with center point 
based on the number of dimensions and the number of design points}
\details{Uses the function maxminLHS from the lhs to generate a  Latin Hypercube Design. 
This function attempts to optimize the sample  
by maximizing the minium distance between design points (maximin criteria).

The dimension is determined from the number of rows of the .roi - file 
(each row in the roi file defines a variable).}
\value{matrix \code{M} \cr
- \code{M} has \code{dimension} columns and \code{noDesPoints} rows
with entries corresponding to the region of interest.}
\arguments{\item{spotConfig}{list of spotConfiguration}
\item{noDesPoints}{number of design points, default is NaN}
\item{repeats}{number of repeats, default is NaN}}
