\name{spotRegPredictor}
\alias{spotRegPredictor}
\title{Predict with Forrester Model}
\usage{
  spotRegPredictor(x, ModelInfo)
}
\arguments{
  \item{x}{design matrix to be predicted (largeDesign in
  SPOT)}

  \item{ModelInfo}{fit of the Kriging model (settings and
  parameters)}
}
\value{
  metric, dependent on the setting of the string
  \code{ModelInfo$Option}\cr "Pred"(default) yields the
  predicted value, "NegLogExpImp" expected improvement,
  "NegProbImp", "RMSE"
}
\description{
  Predict values with forrester
}
\seealso{
  \code{\link{spotPredictForrester}}
  \code{\link{spotDetermineTheta}}
}
\keyword{internal}

