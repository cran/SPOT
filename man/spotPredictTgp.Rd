\name{spotPredictTgp}
\alias{spotPredictTgp}
\title{SPOT Predictor Tgp...}
\usage{spotPredictTgp(rawB, mergedB, lhd, spotConfig)}
\description{SPOT Predictor Tgp}
\details{This function implements a  model for prediction, based on Mat's tgp}
\value{data.frame \code{lhd} \cr
- \code{lhd} is a sorted (with respect to fitness, i.e., smallest estimated function value) largeDesign. 
Best is first}
\references{\code{\link{SPOT}}}
\arguments{\item{rawB}{unmerged data}
\item{mergedB}{merged data}
\item{lhd}{new design points which should be predicted}
\item{spotConfig}{global list of all options, needed to provide data for calling functions}}
