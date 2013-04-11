\name{spotPredictMCO}
\alias{spotPredictMCO}
\title{Meta Model Interface: Multi Criteria Modeling}
\usage{
  spotPredictMCO(rawB, mergedB, design, spotConfig,
    fit = NULL)
}
\arguments{
  \item{rawB}{unmerged data}

  \item{mergedB}{merged data}

  \item{design}{new design points which should be
  predicted}

  \item{spotConfig}{global list of all options, needed to
  provide data for calling functions. The most important
  elements in this list are here:\cr
  \code{spotConfig$mco.configs} list of model
  configurations, e.g.
  \code{=list(list(seq.predictionModel.func="spotPredictForrester",seq.forr.lambda.upval=-5),list(seq.predictionModel.func="spotPredictForrester",seq.forr.lambda.upval=-1))}\cr
  In this example, two Kriging models are specified for
  each of two objectives, but with different settings for
  the lower boundary of lambda. Else, different models
  could be specified, e.g.,
  \code{=list(list(seq.predictionModel.func="spotPredictForrester"),list(seq.predictionModel.func="spotPredictLm"))}}

  \item{fit}{if an existing model fit is supplied, the
  model will not be build based on data, but only evaluated
  with the model fit (on the design data). To build the
  model, this parameter has to be NULL. If it is not NULL
  the parameters mergedB and rawB will not be used at all
  in the function.}
}
\value{
  returns the list \code{spotConfig} with two new
  entries:\cr spotConfig$seq.modelFit fit of the model used
  with the predictor functions \cr
  spotConfig$seq.largeDesignY the y values of the design,
  evaluated with the fit
}
\description{
  This interface function is supposed to be used for Multi
  Criteria Problems. It can be employed when the user wants
  to specify different models for each of the objectives,
  instead of modeling all the objectives with the same
  technique. The user has therefore to specify a list of
  configurations, where the different models and their
  settings are specified.
}
\references{
  Forrester, Alexander I.J.; Sobester, Andras; Keane, Andy
  J. (2008). Engineering Design via Surrogate Modelling - A
  Practical Guide. John Wiley & Sons.
}
\seealso{
  \code{\link{forrBuilder}} \code{\link{forrRegPredictor}}
  \code{\link{forrReintPredictor}}
}

