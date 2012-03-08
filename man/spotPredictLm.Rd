\name{spotPredictLm}
\alias{spotPredictLm}
\title{Meta Model Interface: Linear Model}
\usage{
  spotPredictLm(rawB, mergedB, lhd, spotConfig,
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
  returns the list \code{spotConfig} with two new
  entries:\cr spotConfig$seq.modelFit fit of the Krig model
  used with predict() \cr spotConfig$seq.largeDesignY the y
  values of the large design, evaluated with the fit
}
\description{
  A linear prediction model, which will use higher order
  interactions if data is sufficient. Can be used both for
  single and multi objective SPOT.
}
\details{
  This function implements a linear model for prediction.
  Depending on the numbers of variables either no
  interactions, interaction between the variables may be
  used or a full quadratic model is provided.
}
\seealso{
  \code{\link{SPOT}}
}

