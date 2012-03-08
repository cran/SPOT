\name{spotPredictTest}
\alias{spotPredictTest}
\title{Meta Model Interface: Test}
\usage{
  spotPredictTest(rawB, mergedB, largeDesign, spotConfig,
    externalFit = NULL)
}
\arguments{
  \item{rawB}{unmerged data}

  \item{mergedB}{merged data}

  \item{largeDesign}{new design points which should be
  predicted}

  \item{spotConfig}{global list of all options, needed to
  provide data for calling functions}

  \item{externalFit}{if an existing model fit is supplied,
  the model will not be build based on data, but only
  evaluated with the model fit (on the largeDesign data).
  To build the model, this parameter has to be NULL. If it
  is not NULL the paramters mergedB and rawB will not be
  used at all in the function.}
}
\value{
  returns the list \code{spotConfig} with two new
  entries:\cr spotConfig$seq.modelFit fit of the earth
  model used with predict() \cr spotConfig$seq.largeDesignY
  the y values of the large design, evaluated with the fit
}
\description{
  This model does prediction by sampling on the real target
  function. Supposed to be used for testing purpose only.
  Do not use.
}
\keyword{internal}

