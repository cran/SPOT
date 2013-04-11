\name{spotGlgEval}
\alias{spotGlgEval}
\title{Gaussian Landscape Evaluation}
\usage{
  spotGlgEval(x, glg)
}
\arguments{
  \item{x}{matrix of sample sites, containing one point in
  each row.}

  \item{glg}{list of values defining the Gaussian
  Landscape, created by \code{spotGlgInit}.}
}
\value{
  returns a list, with the following items:\cr \code{value}
  value of the combined landscape \code{components} value
  of each component
}
\description{
  This function is based on the Gaussian Landscape
  Generator by Bo Yuan and Marcus Gallagher. It randomly
  evaluates one or several points in a Gaussian Landscape
  created by \code{spotGlgInit}.
}
\examples{
## Create a landscape with default settings:
	landscape <- spotGlgInit()
	## Create a landscape with larger boundaries and more Gaussian components
	value <-  spotGlgEval(c(0.5,0.5),landscape)
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
  \code{\link{spotGlgCreate}}, \code{\link{spotGlgInit}}
}

