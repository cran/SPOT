\name{spotSmsEmoa}
\alias{spotSmsEmoa}
\title{SMS-EMOA: S-Metric-Selection Evolutionary Multiobjective Optimization Algorithm}
\usage{
  spotSmsEmoa(f, lower, upper, ...,
    control = list(mu = 100L, sbx.n = 15, sbx.p = 0.7, pm.n = 25, pm.p = 0.3))
}
\arguments{
  \item{f}{target function to be optimized of type f(x)=y
  where both x and y are vectors.  The target function
  should return a vector of length(y) containing NA's if
  the input vector x contains NA values.}

  \item{lower}{the lower boundary vector of the decision
  space}

  \item{upper}{the upper boundary vector of the decision
  space}

  \item{...}{further settings relayed to \code{f}}

  \item{control}{list of parameters (defaults are: mu=100L,
  sbx.n=15, sbx.p=0.7, pm.n=25, pm.p=0.3)}
}
\value{
  list with archive of solutions, active pareto front and
  others
}
\description{
  Straight forward SMS-EMOA implementation. This function
  is used to optimize several surrogate models when doing
  multi objective optimization with SPOT. See:
  \code{\link{spotParetoOptMulti}}.
}
\author{
  O. Mersmann
}
\seealso{
  N. Beume, B. Naujoks, and M.Emmerich. \emph{SMS-EMOA:
  Multiobjective selection based on dominated hypervolume}.
  European Journal of Operational Research,
  181(3):1653--1669, 2007. \cr \cr See the following link
  for up to date version of this implementation:
  \url{https://git.p-value.net/emoa.git/plain/examples/sms_emoa.r}
}

