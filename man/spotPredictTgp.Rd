\name{spotPredictTgp}
\alias{spotPredictTgp}
\title{SPOT Predictor Tgp...}
\usage{spotPredictTgp(rawB, mergedB, lhd, spotConfig)}
\description{SPOT Predictor Tgp}
\details{This function implements a  model for prediction, based on Mat's tgp}
\value{returns the list \code{spotConfig} with a new entry:\cr
spotConfig$seq.modelFit fit of the Krig model used with predict() \cr
spotConfig$seq.largeDesignY the y values of the large design, evaluated with the fit}
\references{\code{\link{SPOT}}}
\arguments{\item{rawB}{unmerged data}
\item{mergedB}{merged data}
\item{lhd}{new design points which should be predicted}
\item{spotConfig}{global list of all options, needed to provide data for calling functions}}
