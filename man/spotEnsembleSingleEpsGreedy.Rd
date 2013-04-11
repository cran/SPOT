\name{spotEnsembleSingleEpsGreedy}
\alias{spotEnsembleSingleEpsGreedy}
\title{Single Ensemble: Epsilon Greedy}
\usage{
  spotEnsembleSingleEpsGreedy(rawB, mergedB, design,
    spotConfig, fit = NULL)
}
\arguments{
  \item{rawB}{unmerged data}

  \item{mergedB}{merged data}

  \item{design}{new design points which should be
  predicted}

  \item{spotConfig}{global list of all options, needed to
  provide data for calling functions}

  \item{fit}{if an existing model ensemble fit is supplied,
  the models will not be build based on data, but only
  evaluated with the existing fits (on the design data). To
  build the model, this parameter has to be NULL. If it is
  not NULL the parameters mergedB and rawB will not be used
  at all in the function.}
}
\value{
  returns the list \code{spotConfig}
}
\description{
  Eps. Greedy - Select the model with the greatest success
  with probability 1-epsilon (greedy decision), select a
  random model with probability epsilon (explorative
  decision). That means, the algorithm is completely greedy
  with epsilon=0, and completely explorative with
  epsilon=1\cr The default reward function is
  \code{spotConfig$seq.ensemble.feed.func<-\link{spotFeedback.reward.bern}}.\cr
  The value of epsilon
  \code{spotConfig$seq.greed.epsilon<-0.5}.
}
\details{
  This is a "single ensemble", meaning that in every
  sequential step only one model in the ensemble is trained
  and evaluated. The target is to actively "learn" which of
  the models are most suitable, based on their individual
  success.\cr The models used are specified in the
  \code{spotConfig} list, for instance:\cr
  \code{spotConfig$seq.ensemble.predictors =
  c(spotPredictRandomForest, spotPredictEarth,
  spotPredictForrester, spotPredictDace)}\cr To specify the
  settings of each individual model, set:\cr
  \code{seq.ensemble.settings =
  list(list(setting=1),list(setting=2),list(setting=3),list(setting=4))}\cr
  Any parameters set in each of the corresponding lists
  (here: 4 individual lists) will overwrite settings in the
  main \code{spotConfig} list, when the concerned model
  function is called.
}
\references{
  - Joannes Vermorel and Mehryar Mohri. 2005. Multi-armed
  bandit algorithms and empirical evaluation. In
  Proceedings of the 16th European conference on Machine
  Learning (ECML'05), Joao Gama, Rui Camacho, Pavel B.
  Brazdil, Alipio Mario Jorge, and Luis Torgo (Eds.).
  Springer-Verlag, Berlin, Heidelberg, 437-448.\cr - M.
  Friese, M. Zaefferer, T. Bartz-Beielstein, O. Flasch, P.
  Koch, W. Konen, and B. Naujoks. Ensemble based
  optimization and tuning algorithms. In F. Hoffmann and E.
  Huellermeier, editors, Proceedings 21. Workshop
  Computational Intelligence, p. 119-134.
  Universitaetsverlag Karlsruhe, 2011.
}

