\name{spotPredictLmOptim}
\alias{spotPredictLmOptim}
\title{Meta Model Interface: Spot Predictor Linear Model Optimized}
\usage{
  spotPredictLmOptim(rawB, mergedB, lhd, spotConfig,
    externalFit = NULL)
}
\arguments{
  \item{rawB}{matrix of raw x and y values}

  \item{mergedB}{matrix of merged x and y values, does not
  have replicate entries}

  \item{lhd}{design points to be evaluated by the meta
  model}

  \item{spotConfig}{the list of all parameters is given}

  \item{externalFit}{if an existing model fit is supplied,
  the model will not be build based on data, but only
  evaluated with the model fit (on the lhd data). To build
  the model, this parameter has to be NULL. If it is not
  NULL the paramters mergedB and rawB will not be used at
  all in the function.

  If spotConfig does not contain the list element
  seq.useCanonicalPath (boolean), it will be created with
  the default value FALSE. This setting tells if to start
  at saddle point in both directions (canonical path
  analysis). If spotConfig does not contain the list
  element seq.useGradient (boolean), it will be created
  with the default value FALSE. This setting tells if
  gradient information is used or not.}
}
\value{
  returns the list \code{spotConfig} with two new
  entries:\cr spotConfig$seq.modelFit fit of the Krig model
  used with predict() \cr spotConfig$seq.largeDesignY the y
  values of the large design, evaluated with the fit
}
\description{
  A linear prediction model with optimization on the
  response surface and automated adaptation of the region
  of interest is used to predict improved design points
}
\seealso{
  \code{\link{SPOT}}
}

