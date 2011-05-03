\name{spotPredictRandomForestMlegp}
\alias{spotPredictRandomForestMlegp}
\title{SPOT Predictor Random Forest combined with Mlegp...}
\usage{spotPredictRandomForestMlegp(rawB, mergedB, largeDesign, spotConfig)}
\description{SPOT Predictor Random Forest combined with Mlegp}
\details{A prediction model based on rpart, using a random forest and mlegp}
\value{returns the list \code{spotConfig} with two new entries:\cr
spotConfig$seq.modelFit fit of the Krig model used with predict() \cr
spotConfig$seq.largeDesignY the y values of the large design, evaluated with the fit}
\references{\code{\link{SPOT}}}
\arguments{\item{rawB}{unmerged data}
\item{mergedB}{merged data}
\item{largeDesign}{new design points which should be predicted}
\item{spotConfig}{global list of all options, needed to provide data for calling functions}}
