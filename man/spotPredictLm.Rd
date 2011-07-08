\name{spotPredictLm}
\alias{spotPredictLm}
\title{Spot Predictor Linear Model...}
\usage{spotPredictLm(rawB, mergedB, lhd, spotConfig)}
\description{Spot Predictor Linear Model}
\details{A simple linear prediction model

This function implements a linear model for prediction. Depending on the numbers of variables 
either no interactions,  interaction  between the variables may be used or a full quadratic model is
provided.}
\value{returns the list \code{spotConfig} with two new entries:\cr
spotConfig$seq.modelFit fit of the Krig model used with predict() \cr
spotConfig$seq.largeDesignY the y values of the large design, evaluated with the fit}
\references{\code{\link{SPOT}}}
\arguments{\item{rawB}{unmerged data}
\item{mergedB}{merged data}
\item{lhd}{new design points which should be predicted}
\item{spotConfig}{global list of all options, needed to provide data for calling functions}}
