\name{spotInitializePredictor}
\alias{spotInitializePredictor}
\title{Initialize Predictor}
\usage{
  spotInitializePredictor(design, designType, roi,
    pkgs = NULL, name, verbosity)
}
\arguments{
  \item{design}{matrix, dataframe or vector that contains
  an experimental design of one or more observations and
  variables.}

  \item{designType}{specify "data.frame" to force the
  design into a dataframe. specify "as.matrix" to force a
  matrix.}

  \item{roi}{region of interest, a data frame or matrix
  that has a row for each parameter. It describes the
  region in which the design is chosen. Row names are
  mandatory, and describe the parameter names.}

  \item{pkgs}{package(s) that need to be loaded. Can be
  vector of several packages. If this is NULL, nothing is
  loaded (default).}

  \item{name}{name of the predictor, used only for a
  startup message.}

  \item{verbosity}{this specifies whether startup message
  is produced \code{(verbosity >= 3)} or not
  \code{(verbosity < 3)}.}
}
\value{
  returns the \code{design}, with the chosen format
  (matrix, dataframe) and transposed if number of columns
  do not fit to the \code{roi}.
}
\description{
  Initialization function that should be called at the
  beginning of any call to the surrogate models (e.g.
  \code{spotPredictRandomForest}, \code{spotPredictDace},
  ...).
}
\keyword{internal}

