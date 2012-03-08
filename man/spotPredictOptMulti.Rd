\name{spotPredictOptMulti}
\alias{spotPredictOptMulti}
\title{Optimize predicted meta model}
\usage{
  spotPredictOptMulti(startPoint, spotConfig)
}
\arguments{
  \item{startPoint}{initial points for the optimization}

  \item{spotConfig}{list of all options, needed to provide
  data for calling functions.

  This function uses the parameter
  \code{spotConfig$seq.modelFit}. This is supposed to be a
  fit, that can be evaluated by the predict() interface.
  The parameter \code{spotConfig$seq.predictionOpt.method}
  will be used to choose the optimization method to be used
  to find the minimum of the fitted model:\cr
  "optim-BFGS"\cr "optim-L-BFGS-B"\cr "optim-CG"\cr
  "optim-NM"\cr "optim-SANN"\cr "pso"\cr "cmaes"\cr
  "genoud"\cr "DEoptim"\cr "subplex"\cr "bobyqa"\cr
  "newuoa"\cr "uobyqa"\cr}
}
\value{
  returns the list \code{spotConfig} with two new
  entries:\cr spotConfig$optDesign are the parameters of
  the new minimal design point \cr spotConfig$optDesignY is
  the associated value of the objective function
}
\description{
  Optimizes an existing fit of a model to get an optimal
  new design point. Executed after building the prediction
  model in the sequential SPOT step.
}

