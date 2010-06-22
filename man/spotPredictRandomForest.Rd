\name{spotPredictRandomForest}
\alias{spotPredictRandomForest}
\title{SPOT Predictor Random Forest...}
\usage{spotPredictRandomForest(rawB, mergedB, largeDesign, spotConfig)}
\description{SPOT Predictor Random Forest}
\details{A prediction model based on rpart, using a random forest}
\value{data.frame \code{lhd} \cr
- \code{lhd} is a sorted (with respect to fitness, i.e., smallest estimated function value) largeDesign. 
Best is first}
\references{\code{\link{SPOT}}}
\arguments{\item{rawB}{unmerged data}
\item{mergedB}{merged data}
\item{largeDesign}{new design points which should be predicted}
\item{spotConfig}{global list of all options, needed to provide data for calling functions}}
