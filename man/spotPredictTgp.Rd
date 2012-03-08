\name{spotPredictTgp}
\alias{spotPredictTgp}
\title{Meta Model Interface: Treed Gaussian Processes}
\usage{
  spotPredictTgp(rawB, mergedB, lhd, spotConfig,
    externalFit = NULL)
}
\arguments{
  \item{rawB}{unmerged data}

  \item{mergedB}{merged data}

  \item{lhd}{new design points which should be predicted}

  \item{spotConfig}{global list of all options, needed to
  provide data for calling functions}

  \item{externalFit}{if an existing model fit is supplied,
  the model will not be build based on data, but only
  evaluated with the model fit (on the lhd data). To build
  the model, this parameter has to be NULL. If it is not
  NULL the paramters mergedB and rawB will not be used at
  all in the function.}
}
\value{
  returns the list \code{spotConfig} with a new entry:\cr
  spotConfig$seq.modelFit fit of the Krig model used with
  predict() \cr spotConfig$seq.largeDesignY the y values of
  the large design, evaluated with the fit
}
\description{
  This function implements a model for prediction, based on
  Mat's tgp
}
\seealso{
  \code{\link{SPOT}}
}

