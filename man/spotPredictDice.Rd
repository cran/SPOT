\name{spotPredictDice}
\alias{spotPredictDice}
\title{Meta Model Interface: Dice Kriging}
\usage{
  spotPredictDice(rawB, mergedB, largeDesign, spotConfig,
    externalFit = NULL)
}
\arguments{
  \item{rawB}{matrix of raw x and y values}

  \item{mergedB}{matrix of merged x and y values, does not
  have replicate entries}

  \item{largeDesign}{design points to be evaluated by the
  meta model}

  \item{spotConfig}{the list of all parameters is given.}

  \item{externalFit}{if an existing model fit is supplied,
  the model will not be build based on data, but only
  evaluated with the model fit (on the largeDesign data).
  To build the model, this parameter has to be NULL. If it
  is not NULL the paramters mergedB and rawB will not be
  used at all in the function.}
}
\value{
  returns the list \code{spotConfig} with two new
  entries:\cr spotConfig$seq.modelFit fit of the Krig model
  used with predict() \cr spotConfig$seq.largeDesignY the y
  values of the large design, evaluated with the fit
}
\description{
  Kriging meta model based on the DiceKriging package. It
  usually provides good performance, but is very unstable.
}
\seealso{
  \code{\link{SPOT}}
}

