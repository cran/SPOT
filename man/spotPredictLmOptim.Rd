\name{spotPredictLmOptim}
\alias{spotPredictLmOptim}
\title{Spot Predictor Linear Model Optimized...}
\usage{spotPredictLmOptim(rawB, mergedB, lhd, spotConfig)}
\description{Spot Predictor Linear Model Optimized}
\details{A linear prediction model with optimization on the response surface
and automated adaptation of the region of interest is used to predict 
improved design points}
\value{data.frame \code{steepestDesc} \cr
- \code{steepestDesc} is a sorted (with respect to fitness, i.e., smallest estimated function value) largeDesign. 
Best is first}
\references{\code{\link{SPOT}}}
\arguments{\item{rawB}{matrix of raw x and y values}
\item{mergedB}{matrix of merged x and y values, does not have replicate entries}
\item{lhd}{design points to be evaluated by the meta model}
\item{spotConfig}{the list of all parameters is given}}
