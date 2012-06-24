\name{esOptim}
\alias{esOptim}
\title{esOptim: optim-like ES interface}
\usage{
  esOptim(par, fn, gr = NULL, ..., lower, upper,
    method = NULL, control = list())
}
\arguments{
  \item{par}{is a point in search intervall (defines
  dimension)}

  \item{fn}{is the target function}

  \item{gr}{gradient function, not used by this function}

  \item{...}{additional parameters to be passed on to
  \code{fn}}

  \item{lower}{is a vector that defines the lower boundary
  of search space}

  \item{upper}{is a vector that defines the upper boundary
  of search space}

  \item{method}{this parameter is not used in the current
  version.}

  \item{control}{is a list of additional settings.

  The \code{control} list can contain the following
  settings: \describe{ \item{maxit}{number of iterations,
  stopping criterion, default is \code{100}}
  \item{mue}{number of parents, default is \code{10}}
  \item{nu}{number, default is \code{10}}
  \item{dimension}{dimension number of the target function,
  default is \code{2}} \item{mutation}{string of mutation
  type, default is \code{1}} \item{sigmaInit}{initial sigma
  value (standard deviation), default is \code{1.0}}
  \item{nSigma}{number of standard deviations, default is
  \code{1}} \item{tau0}{number, default is \code{0.0}}
  \item{tau}{number, learning parameter for self adaption,
  default is \code{1.0}} \item{rho}{number of parents
  involved in the procreation of an offspring (mixing
  number), default is \code{"bi"}} \item{sel}{number of
  selected individuals, default is \code{1}}
  \item{stratReco}{value, Recombination operator for
  strategy variables, default is \code{1}}
  \item{objReco}{value, Recombination operator for object
  variables, default is \code{2}} \item{maxGen}{number of
  generations, stopping criterion, default is \code{Inf}}
  \item{seed}{number, random seed, default is \code{1}}
  \item{noise}{number, value of noise added to fitness
  values, default is \code{0.0}} \item{lowerLimit}{number,
  lower limit for search space, default is \code{-1.0}}
  \item{upperLimit}{number, upper limit for search space,
  default is \code{1.0}} \item{verbosity}{defines output
  verbosity of the ES, default is \code{0}}
  \item{plotResult}{boolean, specifies if results are
  plotted, default is \code{FALSE}}
  \item{logPlotResult}{boolean, defines if plot results
  should be logarithmic, default is \code{FALSE}}
  \item{term}{a string, defines which termination criterion
  should be used, default is \code{"iter"}}
  \item{sigmaRestart}{number, value of sigma on restart,
  default is \code{0.1}} \item{preScanMult}{initial
  population size is multiplied by this number for a
  pre-scan, default is \code{1}}
  \item{globalOpt}{termination criterion on reaching a
  desired optimum value, default is
  \code{rep(0,dimension)}} }}
}
\value{
  This function returns a list with:\cr \code{par}
  parameters of the found solution\cr \code{value} target
  function value of the found solution\cr
}
\description{
  This is an interface to the Evolution Strategy used as a
  target algorithm by some SPOT demos. It is build like the
  \code{\link{optim}} interface.
}
\seealso{
  \code{\link{optim}} \code{\link{spotOptim}}
}

