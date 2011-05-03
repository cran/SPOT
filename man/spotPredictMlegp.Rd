\name{spotPredictMlegp}
\alias{spotPredictMlegp}
\title{Spot Predictor Kriging...}
\usage{spotPredictMlegp(rawB, mergedB, largeDesign, spotConfig)}
\description{Spot Predictor Kriging}
\details{Kriging model based on mlegp}
\value{returns the list \code{spotConfig} with two new entries:\cr
spotConfig$seq.modelFit fit of the Krig model used with predict() \cr
spotConfig$seq.largeDesignY the y values of the large design, evaluated with the fit}
\references{\code{\link{SPOT}}}
\arguments{\item{rawB}{matrix of raw x and y values}
\item{mergedB}{matrix of merged x and y values, does not have replicate entries}
\item{largeDesign}{design points to be evaluated by the meta model}
\item{spotConfig}{the list of all parameters is given}}
