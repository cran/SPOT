\name{spotGlgInit}
\alias{spotGlgInit}
\title{Initialize Gaussian Landscape}
\usage{
  spotGlgInit(dimension = 2, nGaussian = 10,
    lower = rep(0, dimension), upper = rep(1, dimension),
    globalvalue = 1, ratio = 0.8, seed = 1)
}
\arguments{
  \item{dimension}{dimensionality of the landscapes input
  space. Default is 2.}

  \item{nGaussian}{number of Gaussian components in the
  landscape. Default is 10.}

  \item{lower}{lower boundary of the landscape, defaults to
  \code{rep(0,dimension)}.}

  \item{upper}{upper boundary of the landscape, defaults to
  \code{rep(1,dimension)}.}

  \item{globalvalue}{the global maximum value, i.e. the
  maximum of the Gaussian component with the largest value.
  Default is 1.}

  \item{ratio}{maximum ratio of the local maxima, local
  optima are randomly generated within
  \code{[0,globalvalue*ratio]}. Has to be larger than 0 and
  smaller than 1. Defaults to 0.8.}

  \item{seed}{seed for the random number generator used
  before creation of the landscape. Generator status will
  be saved and reset afterwards.}
}
\value{
  returns a list, with the following items:\cr \code{mean}
  Matrix containing the mean vectors of the Gaussian
  components in the landscape, i.e. the locations of the
  local maxima of the functions. First vector (i.e. first
  row) will be the global maximum. \code{covinv} Inverse of
  covariance matrix of each Gaussian component, stored as
  3-dimensional array. \code{opt} optimal values, i.e.
  maxima of the Gaussian components \code{ngauss} number of
  Gaussian components \code{d} is the \code{dimension}
}
\description{
  This function is based on the Gaussian Landscape
  Generator by Bo Yuan and Marcus Gallagher. It randomly
  initializes a Gaussian Landscape with the specified
  parameters.
}
\examples{
## Create a landscape with default settings:
	landscape <- spotGlgInit()
	## Create a landscape with larger boundaries and more Gaussian components
	landscape <- spotGlgInit(2, 100, -5, 5, 10, 0.8)
}
\author{
  Original Matlab code by Bo Yuan, ported to R by Martin
  Zaefferer
}
\references{
  B. Yuan and M. Gallagher (2003) "On Building a Principled
  Framework for Evaluating and Testing Evolutionary
  Algorithms: A Continuous Landscape Generator". In
  Proceedings of the 2003 Congress on Evolutionary
  Computation, IEEE, pp. 451-458, Canberra, Australia.
}
\seealso{
  \code{\link{spotGlgCreate}}, \code{\link{spotGlgEval}}
}

