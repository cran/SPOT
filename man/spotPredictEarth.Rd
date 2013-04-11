\name{spotPredictEarth}
\alias{spotPredictEarth}
\title{Meta Model Interface: Multivariate Adaptive Regression Spline}
\usage{
  spotPredictEarth(rawB, mergedB, design, spotConfig,
    fit = NULL)
}
\arguments{
  \item{rawB}{unmerged data}

  \item{mergedB}{merged data}

  \item{design}{new design points which should be
  predicted}

  \item{spotConfig}{global list of all options, needed to
  provide data for calling functions}

  \item{fit}{if an existing model fit is supplied, the
  model will not be build based on data, but only evaluated
  with the model fit (on the design data). To build the
  model, this parameter has to be NULL. If it is not NULL
  the parameters mergedB and rawB will not be used at all
  in the function.}
}
\value{
  returns the list \code{spotConfig} with two new
  entries:\cr spotConfig$seq.modelFit fit of the earth
  model used with predict() \cr spotConfig$seq.largeDesignY
  the y values of the design, evaluated with the fit
}
\description{
  Prediction based on earth package, using Multivariate
  Adaptive Regression Spline models Can be used both for
  single and multi objective SPOT.
}
\details{
  This is a model that can incorporate parameters which are
  marked as FACTORS (i.e. categorical parameters) in the
  region of interest, see \code{\link{spotROI}}. Please
  note that the design used to train the MARS model should
  contain all levels of the factor variable. FACTORS are
  not ordered, and therefore are impossible to extrapolate
  on. If new data is given in the \code{design} variable
  which contains unseen FACTOR levels, please note that
  this will probably create NA values in the prediction.
  (With the exception that no NA values are created if the
  concerned FACTOR is not selected as a predictor by
  \code{earth}) NA values might yield errors in your SPOT
  run, ending it prematurely. It is therefore recommended
  to build a initial design which contains at least one
  example of each FACTOR level.
}
\seealso{
  \code{\link{spotPredictLmFactor}}
}

