\name{spotOptim}
\alias{spotOptim}
\title{spotOptim: optim-like spot interface}
\usage{
  spotOptim(par = NULL, fn, gr = NULL, ..., lower = -Inf,
    upper = Inf, method, control = list())
}
\arguments{
  \item{par}{is a point in search interval (defines
  dimension)}

  \item{fn}{is the target function (it can also be a string
  with the name of a spot interface function, like
  "\code{\link{spotFuncStartBranin}}")}

  \item{gr}{gradient function, not implemented yet}

  \item{...}{additional parameters to be passed on to
  \code{fn}}

  \item{lower}{is a vector that defines the lower boundary
  of search space}

  \item{upper}{is a vector that defines the upper boundary
  of search space}

  \item{method}{is a string that describes which method is
  to be used.}

  \item{control}{is a list of additional settings.
  \code{maxit} is the number of function evaluations, all
  the other settings will simply be passed to SPOT (see
  \code{\link{spotGetOptions}} for details)}
}
\value{
  This function returns a list with:\cr \code{par}
  parameters of the found solution\cr \code{value} target
  function value of the found solution\cr
}
\description{
  Besides \code{\link{spot}} this is one of the main
  interfaces for using the SPOT package. It is build like
  the \code{\link{optim}} interface.
}
\details{
  It is of important to note that spot by default expects
  to optimize noisy functions. That means, the default
  settings of spot, which are also used in spotOptim,
  include repeats of the initial and sequentially created
  design points. Also, as a default OCBA is used to spread
  the design points for optimal usage of the function
  evaluation budget. OCBA will not work when there is no
  variance in the data. So if the user wants to optimize
  non-noisy functions, the following settings should be
  used:\cr \code{control$spot.ocba <- FALSE}\cr
  \code{control$seq.design.maxRepeats <- 1}\cr
  \code{control$init.design.repeats <- 1}\cr

  A call to a noisy function could look like this: \cr
  \code{objFunction<-function(x){y=(x[1]+2)^2*(x[2]-4)^2+runif(1)}}
  \cr
  \code{spotOptim(par=c(1,1),fn<-objFunction,lower=c(-10,-10),upper=c(10,10),method="spotPredictRandomForest",control=list(maxit=50))}

  A call to a non-noisy function could look like this: \cr
  \code{objFunction<-function(x){y=(x[1]+2)^2*(x[2]-4)^2}}
  \cr
  \code{spotOptim(par=c(1,1),fn<-objFunction,lower=c(-10,-10),upper=c(10,10),method="spotPredictRandomForest",control=list(maxit=50,spot.ocba=FALSE,seq.design.maxRepeats=1,init.design.repeats=1))}
}
\seealso{
  \code{\link{spot}} \code{\link{spotOptimInterface}}
  \code{\link{spotOptimizationInterface}}
  \code{\link{spotOptimizationInterfaceMco}}
}

