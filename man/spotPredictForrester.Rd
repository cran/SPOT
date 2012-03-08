\name{spotPredictForrester}
\alias{spotPredictForrester}
\title{Meta Model Interface: Forresters Kriging}
\usage{
  spotPredictForrester(rawB, mergedB, largeDesign,
    spotConfig, externalFit = NULL)
}
\arguments{
  \item{rawB}{unmerged data}

  \item{mergedB}{merged data}

  \item{largeDesign}{new design points which should be
  predicted}

  \item{spotConfig}{global list of all options, needed to
  provide data for calling functions. This also contains a
  list, with settings for forrester:\cr
  \code{spotConfig$seq.forrester$Option} What to predict,
  can be: "Pred"(default), "NegLogExpImp", "NegProbImp",
  "RMSE"\cr \code{spotConfig$seq.forrester$loval} Lower
  boundary for theta, default is 1e-3\cr
  \code{spotConfig$seq.forrester$upval} Upper boundary for
  theta, default is 100\cr
  \code{spotConfig$seq.forrester$algtheta} algorithm used
  to find theta, default is "cmaes"\cr
  \code{spotConfig$seq.forrester$budgetalgtheta} Budget for
  the above mentioend algorithm, default is 1000}

  \item{externalFit}{if an existing model fit is supplied,
  the model will not be build based on data, but only
  evaluated with the model fit (on the largeDesign data).
  To build the model, this parameter has to be NULL. If it
  is not NULL the paramters mergedB and rawB will not be
  used at all in the function.}
}
\value{
  returns the list \code{spotConfig} with two new
  entries:\cr spotConfig$seq.modelFit fit of the model used
  with predict() \cr spotConfig$seq.largeDesignY the y
  values of the large design, evaluated with the fit
}
\description{
  Kriging predictor based on matlab code by Forrester
  et.al. Can be used both for single and multi objective
  SPOT.
}
\references{
  Forrester, Alexander I.J.; Sobester, Andras; Keane, Andy
  J. (2008). Engineering Design via Surrogate Modelling - A
  Practical Guide. John Wiley & Sons.
}
\seealso{
  \code{\link{spotDetermineTheta}}
  \code{\link{spotNormalizeMatrix2}}
  \code{\link{spotNormalizeMatrix}}
  \code{\link{spotReverseNormalizeMatrix}}
  \code{\link{spotRegPredictor}}
  \code{\link{spotRegLikelihood}}
}

