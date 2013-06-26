\name{spotGlgEvalRot}
\alias{spotGlgEvalRot}
\title{Gaussian Landscape Evaluation (rotated)}
\usage{
  spotGlgEvalRot(x, glg)
}
\arguments{
  \item{x}{matrix of sample sites, containing one point in
  each row.}

  \item{glg}{list of values defining the Gaussian
  Landscape, created by \code{spotGlgInit}.}
}
\value{
  returns a list, with the following items:\cr \code{value}
  vector of 2 values, first belongs to non-rotated
  landscape. \code{components} value of each component
  (will be NA)
}
\description{
  This function is based on the Gaussian Landscape
  Generator by Bo Yuan and Marcus Gallagher. It randomly
  evaluates one or several points in a combination of two
  landscapes, where the second is a rotation of the first.
}
\references{
  B. Yuan and M. Gallagher (2003) "On Building a Principled
  Framework for Evaluating and Testing Evolutionary
  Algorithms: A Continuous Landscape Generator". In
  Proceedings of the 2003 Congress on Evolutionary
  Computation, IEEE, pp. 451-458, Canberra, Australia.
}
\seealso{
  \code{\link{spotGlgCreateRot}},
  \code{\link{spotGlgInit}}, \code{\link{spotGlgEval}},
  \code{\link{spotGlgEvalN}}
}

