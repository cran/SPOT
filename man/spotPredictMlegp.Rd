\name{spotPredictMlegp}
\alias{spotPredictMlegp}
\title{Spot Predictor Kriging...}
\usage{spotPredictMlegp(rawB, mergedB, largeDesign, spotConfig)}
\description{Spot Predictor Kriging}
\details{Kriging model based on mlegp}
\value{data.frame \code{lhd} \cr
- \code{lhd} is a sorted (with respect to fitness, i.e., smallest estimated function value) largeDesign   
Best is first}
\references{\code{\link{SPOT}}}
\arguments{\item{rawB}{matrix of raw x and y values}
\item{mergedB}{matrix of merged x and y values, does not have replicate entries}
\item{largeDesign}{design points to be evaluated by the meta model}
\item{spotConfig}{the list of all parameters is given}}
