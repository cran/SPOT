\name{forrBuilder}
\alias{forrBuilder}
\title{Build Forrester Kriging}
\usage{
  forrBuilder(X, y, loval = 0.001, upval = 100,
    algtheta = "NLOPT_LN_NELDERMEAD", budgetalgtheta = 100,
    lb = NULL, ub = NULL, opt.p = FALSE, lambda.loval = -6,
    lambda.upval = 0)
}
\arguments{
  \item{X}{design matrix (sample locations)}

  \item{y}{vector of observations at X}

  \item{loval}{lower boundary for theta, default is
  \code{1e-3}}

  \item{upval}{upper boundary for theta, default is
  \code{100}}

  \item{algtheta}{algorithm used to find theta, default is
  \code{"NLOPT_LN_NELDERMEAD"} which is a bounded simplex
  method from the package nloptr. Else, any from the list
  of possible \code{method} values in
  \code{\link{spotOptimizationInterface}} can be chosen.}

  \item{budgetalgtheta}{budget for the above mentioned
  algorithm, default is \code{100}. The value will be
  multiplied with the length of the model parameter vector
  to be optimized.}

  \item{lb}{lower boundary of the design space. Will be
  extracted from the matrix \code{X} if not given.}

  \item{ub}{upper boundary of the design space. Will be
  extracted from the matrix \code{X} if not given.}

  \item{opt.p}{boolean that specifies whether the exponents
  (\code{p}) should be optimized. Else they will be set to
  two. Default value is \code{FALSE}. Default is highly
  recommended as the implementation of this feature is not
  yet well tested and might be faulty.}

  \item{lambda.loval}{lower boundary for regularization
  constant (nugget), default is \code{-6}.
  (lambda=10^lambda, e.g. 10^-6)}

  \item{lambda.upval}{upper boundary for regularization
  constant (nugget), default is \code{0}.
  (lambda=10^lambda, e.g. 1)}
}
\value{
  a fit (list), with the options and found parameters for
  the model which has to be passed to the predictor
  function:\cr \code{X} sample locations (scaled to values
  between 0 and 1)\cr \code{y} observations at sample
  locations (see parameters)\cr \code{loval} lower boundary
  for theta (see parameters)\cr \code{upval} upper boundary
  for theta (see parameters)\cr \code{algtheta} algorithm
  to find theta (see parameters)\cr \code{budgetalgtheta}
  budget for the above mentioned algorithm (see
  parameters)\cr \code{opt.p} boolean that specifies
  whether the exponents (\code{p}) were optimized (see
  parameters)\cr \code{normalizeymin} minimum in normalized
  space\cr \code{normalizeymax} maximum in normalized
  space\cr \code{normalizexmin} minimum in input space\cr
  \code{normalizexmax} maximum in input space\cr
  \code{dmodeltheta} vector of activity parameters\cr
  \code{Theta} log_10 vector of activity parameters (i.e.
  \code{log10(dmodeltheta)})\cr \code{dmodellambda}
  regularization constant (nugget) \cr \code{Lambda} log_10
  of regularization constant (nugget) (i.e.
  \code{log10(dmodellambda)})\cr \code{yonemu}
  \code{Ay-ones*mu} \cr \code{ssq} sigma square\cr
  \code{mu} mean mu\cr \code{Psi} matrix large Psi\cr
  \code{Psinv} inverse of Psi\cr \code{nevals} number of
  Likelihood evaluations during MLE
}
\description{
  This function builds a Kriging model based on code by
  Forrester et al.. By default exponents (p) are fixed at a
  value of two, and a nugget (or regularization constant)
  is used.
}
\examples{
## Create design points
x = cbind(runif(20)*15-5,runif(20)*15)
## Compute observations at design points (for Branin function)
y = as.matrix(apply(x,1,spotBraninFunction))
## Create model with default settings
fit = forrBuilder(x,y)
## Print model parameters
print(fit)
}
\references{
  Forrester, Alexander I.J.; Sobester, Andras; Keane, Andy
  J. (2008). Engineering Design via Surrogate Modelling - A
  Practical Guide. John Wiley & Sons.
}
\seealso{
  \code{\link{spotPredictForrester}}
  \code{\link{forrRegPredictor}}
  \code{\link{forrReintPredictor}}
}

