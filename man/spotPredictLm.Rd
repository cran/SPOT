\name{spotPredictLm}
\alias{spotPredictLm}
\title{Spot Predictor Linear Model...}
\usage{spotPredictLm(rawB, mergedB, lhd, spotConfig)}
\description{Spot Predictor Linear Model}
\details{A simple linear prediction model

This function implements a linear model for prediction. Depending on the numbers of variables 
either no interactions,  interaction  between the variables may be used or a full quadratic model is
provided.}
\value{data.frame \code{lhd} \cr 
- \code{lhd} is a sorted (with respect to fitness, i.e., smallest estimated function value) largeDesign. 
Best is first}
\references{\code{\link{SPOT}}}
\arguments{\item{rawB}{unmerged data}
\item{mergedB}{merged data}
\item{lhd}{new design points which should be predicted}
\item{spotConfig}{global list of all options, needed to provide data for calling functions}}
