\name{spotOptimizationInterfaceMco}
\alias{spotOptimizationInterfaceMco}
\title{spotOptimizationInterfaceMco}
\usage{
  spotOptimizationInterfaceMco(par, fn, gr = NULL, lower,
    upper, method, control, ref, ...)
}
\arguments{
  \item{par}{is a point (vector) in the decision space of
  \code{fn}, par is not used (yet)}

  \item{fn}{is the target function of type \code{y = f(x,
  ...)}}

  \item{gr}{gradient function, gr is not used (yet)}

  \item{lower}{is a vector that defines the lower boundary
  of search space}

  \item{upper}{is a vector that defines the upper boundary
  of search space}

  \item{method}{is a string that describes which method is
  to be used, as implemented in this function. Else it can
  be a function, which is a custom optimization function
  created by the user. See details.}

  \item{control}{is a list of additional settings. See
  details.}

  \item{ref}{reference point. Please provide this even with
  methods that do not use it (e.g. "nsga2"), to specify the
  dimension of the objective space.}

  \item{...}{additional parameters to be passed on to
  \code{fn}}
}
\value{
  This function returns a list with:\cr \code{par}
  parameters of the found solutions, e.g. the Pareto set\cr
  \code{value} target function values of the found
  solutions, e.g. the Pareto front\cr \code{counts} number
  of evaluations of \code{fn}
}
\description{
  This function is an interface fashioned like the
  \code{\link{optim}} function. It is used in SPOT to
  access several different multi-criteria optimization
  methods.
}
\details{
  The control list contains:\cr \code{fevals} stopping
  criterion, number of evaluations allowed for \code{fn}
  (defaults to 100) \cr \code{popsize} population size or
  number of particles (default depends on method) \cr
  \code{restarts} whether or not to do restarts, default is
  FALSE \cr Also note that the parameter \code{method} will
  be used to choose the optimization method from the
  following list:\cr "nsga2" - the nsga2 function from the
  \code{mco} Package \cr "sms-emoa" - The basic sms-emoa in
  the \code{SPOT} package\cr All of the above methods use
  bound constraints. For references and details on the
  specific methods, please check the documentation of the
  packages that provide them.\cr\cr Furthermore, the user
  can choose to use a function instead of a string for the
  \code{method}. The used function should have the same
  parameters and arguments as documented for this very
  function, i.e. \code{spotOptimizationInterfaceMco}.
}
\seealso{
  \code{\link{spotOptimizationInterface}}
  \code{\link{spotOptim}}
}

