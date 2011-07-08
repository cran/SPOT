\name{spotPredictOptMulti}
\alias{spotPredictOptMulti}
\title{SPOT Optimize predicted model...}
\usage{spotPredictOptMulti(startPoint, spotConfig)}
\description{SPOT Optimize predicted model}
\details{Optimizes a fit of a model to get an optimal new design point. Executed after building the prediction model.}
\value{returns the list \code{spotConfig} with two new entries:\cr
spotConfig$optDesign are the parameters of the new minimal design point \cr
spotConfig$optDesignY is the associated value of the objective function}
\arguments{\item{startPoint}{initial point of optimization}
\item{spotConfig}{list of all options, needed to provide data for calling functions.

This function uses the parameter \code{spotConfig$seq.modelFit}. This is supposed to be a fit, that can be evaluated by the predict() interface.
The parameter \code{spotConfig$seq.predict.subMethod} will be used to choose the optimization method to be used to find the minimum of the fitted model:\cr
"optim-BFGS"\cr
"optim-L-BFGS-B"\cr
"optim-CG"\cr
"optim-NM"\cr
"optim-SANN"\cr
"pso"\cr
"cmaes"\cr
"genoud"\cr
"DEoptim"\cr
"subplex"\cr
"bobyqa"\cr
"newuoa"\cr
"uobyqa"\cr}}
