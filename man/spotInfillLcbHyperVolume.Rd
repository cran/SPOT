\name{spotInfillLcbHyperVolume}
\alias{spotInfillLcbHyperVolume}
\title{Hypervolume Lower Confidence Bound Infill Criterion}
\usage{
  spotInfillLcbHyperVolume(resy, resvar, y, ref = NULL)
}
\arguments{
  \item{resy}{predicted objective values}

  \item{resvar}{predicted variance}

  \item{y}{the current Pareto front}

  \item{ref}{reference point, if not given will be chosen
  as maximum of observed values plus one}
}
\value{
  returns the contribution (or penalty) for each row in
  resy
}
\description{
  This multi objective infill criterion is similar to the
  SMS-EGO infill criterion by Ponweiser (2008). It
  aggregates the objective values for each point by
  calculating the hypervolume contribution. As a first step
  the lower confidence bound is calculated, decreasing the
  predicted objective values by their predicted variance.
  Unlike SMS-EGO, epsilon dominance is not employed here.
  Also, the penalties for dominated points are calculated
  differently: The hypervolume between the dominated points
  and the current true Pareto front is used.
}
\note{
  An optimizer like pso will work signif. better than cmaes
  with this infill criterion.
}
\references{
  W. Ponweiser, T. Wagner, D. Biermann, and M. Vincze.
  Multiobjective optimization on a limited budget of
  evaluations using model-assisted -metric selection. In
  PPSN, pages 784-794, 2008.
}

