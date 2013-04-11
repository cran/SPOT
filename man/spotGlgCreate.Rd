\name{spotGlgCreate}
\alias{spotGlgCreate}
\title{Create Gaussian Landscape}
\usage{
  spotGlgCreate(dimension = 2, nGaussian = 10,
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
  returns a function. The function takes a point (vector)
  as input, with as many values as specified in
  \code{dimension}. The function returns a single scalar
  value, which is the Landscape value at the current point.
  The function has several attributes which reflect the
  items returned by \code{\link{spotGlgInit}}. reflect the
  values returned by
}
\description{
  This function is based on the Gaussian Landscape
  Generator by Bo Yuan and Marcus Gallagher. It creates a
  new Gaussian Landscape every time it is called. This
  Landscape can be evaluated like a function.
}
\examples{
## Create a landscape function with default settings:
	landscapeFun <- spotGlgCreate()
	## Plot the landscape (uncomment before running this example)
	#spotSurf3d(landscapeFun)
}
\references{
  B. Yuan and M. Gallagher (2003) "On Building a Principled
  Framework for Evaluating and Testing Evolutionary
  Algorithms: A Continuous Landscape Generator". In
  Proceedings of the 2003 Congress on Evolutionary
  Computation, IEEE, pp. 451-458, Canberra, Australia.
}

