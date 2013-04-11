\name{spotForceFactorX}
\alias{spotForceFactorX}
\title{Force into Factors: X}
\usage{
  spotForceFactorX(x, type, xNames)
}
\arguments{
  \item{x}{sample locations}

  \item{type}{string of type information, factors should be
  "FACTOR", rest is ignored}

  \item{xNames}{names of the parameters in x}
}
\value{
  returns a list with component \code{x} (data frame x, now
  with factors) \code{levels} (list of levels of the
  corresponding factors).
}
\description{
  Forces the type of parameters in the sample locations x
  to factors, if type[i] is specified to "FACTOR"
}
\keyword{internal}

