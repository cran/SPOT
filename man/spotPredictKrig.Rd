\name{spotPredictKrig}
\alias{spotPredictKrig}
\title{spotPredictKrig...}
\usage{spotPredictKrig(rawB, mergedB, largeDesign, spotConfig)}
\description{spotPredictKrig}
\details{Kriging based on fields}
\value{returns the list \code{spotConfig} with two new entries:\cr
spotConfig$seq.modelFit fit of the Krig model used with predict() \cr
spotConfig$seq.largeDesignY the y values of the large design, evaluated with the fit}
\arguments{\item{rawB}{unmerged data}
\item{mergedB}{merged data}
\item{largeDesign}{new design points which should be predicted}
\item{spotConfig}{global list of all options, needed to provide data for calling functions}}
