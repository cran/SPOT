\name{spotPredictMLP}
\alias{spotPredictMLP}
\title{Meta Model Interface: Multi-layer Perceptron}
\usage{
  spotPredictMLP(rawB, mergedB, design, spotConfig,
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
  Meta model based on monmlp package for multi layer
  perceptrons
}

