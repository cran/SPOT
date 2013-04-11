\name{spotPredictMlegp}
\alias{spotPredictMlegp}
\title{Meta Model Interface: Maximum Likelihood Estimation for Gaussian Processes, Kriging}
\usage{
  spotPredictMlegp(rawB, mergedB, design, spotConfig,
    fit = NULL)
}
\arguments{
  \item{rawB}{matrix of raw x and y values}

  \item{mergedB}{matrix of merged x and y values, does not
  have replicate entries}

  \item{design}{design points to be evaluated by the meta
  model}

  \item{spotConfig}{the list of all parameters is given.}

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
  with predict() \cr spotConfig$seq.largeDesignY the y
  values of the design, evaluated with the fit
}
\description{
  Kriging model based on mlegp package. This function uses
  two settings, which are stored in the spotConfig
  parameter:\cr \code{spotConfig$seq.mlegp.constantMean}
  Use constant mean (mu) in mlegp (=1) or linear model
  (=0); 1 by default\cr
  \code{spotConfig$seq.mlegp.min.nugget} minimum value of
  nugget term; 0 by default\cr\cr If those settings are not
  in spotConfig their mentioned defaults will be used.
  \cr\cr If the numeric value of
  \code{spotConfig$mlegp.reduce} is smaller than the
  observations in \code{mergedB},
  \code{spotConfig$mlegp.reduce} will specify how many
  samples should be drawn without replacement from mergedB.
  This can prevent explosion of time consumption in this
  function. Mlegp can be used both for single and multi
  objective SPOT.
}
\seealso{
  \code{\link{SPOT}}
}

