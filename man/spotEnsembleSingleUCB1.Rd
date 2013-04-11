\name{spotEnsembleSingleUCB1}
\alias{spotEnsembleSingleUCB1}
\title{Single Ensemble: UCB1}
\usage{
  spotEnsembleSingleUCB1(rawB, mergedB, design, spotConfig,
    fit = NULL)
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
  UCB1 - Upper Confidence Bound 1 \cr Use the model that
  maximizes \code{xi + sqrt( 2 * ln(n) / ni )}, where
  code{xi} is the average reward of the model \code{i},
  \code{n} is the number of sequential steps (i.e. number
  of times any model was used), and \code{ni} is the number
  of times the model \code{i} was chosen. The default
  reward function is
  \code{spotConfig$seq.ensemble.feed.func<-\link{spotFeedback.reward.bern}}.
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
  - Peter Auer, Nicolo Cesa-Bianchi, and Paul Fischer.
  Finite-time analysis of the multiarmed bandit problem.
  Machine Learning, 47((2-3)), 2002.\cr - Volodymyr
  Kuleshov and Doina Precup. Algorithms for the multi-armed
  bandit problem. Submitted, 2010. URL:
  http://www.cs.mcgill.ca/~vkules/bandits.pdf\cr - M.
  Friese, M. Zaefferer, T. Bartz-Beielstein, O. Flasch, P.
  Koch, W. Konen, and B. Naujoks. Ensemble based
  optimization and tuning algorithms. In F. Hoffmann and E.
  Huellermeier, editors, Proceedings 21. Workshop
  Computational Intelligence, p. 119-134.
  Universitaetsverlag Karlsruhe, 2011.
}

