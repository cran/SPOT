\name{spotForceFactorDesign}
\alias{spotForceFactorDesign}
\title{Force into Factors: Design}
\usage{
  spotForceFactorDesign(design, type, xNames, lvls)
}
\arguments{
  \item{design}{prediction locations}

  \item{type}{string of type information, factors should be
  "FACTOR", rest is ignored}

  \item{xNames}{names of the parameters in x}

  \item{lvls}{levels to be used when forcing the parameter
  values to factor type}
}
\value{
  returns the design variable, but now with factors where
  necessary.
}
\description{
  Forces the type of parameters in the design to be
  predicted to factors, if type[i] is specified to "FACTOR"
}
\keyword{internal}

