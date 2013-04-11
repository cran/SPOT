\name{spotModel.predict}
\alias{spotModel.predict}
\title{Model Prediction Interface}
\usage{
  spotModel.predict(X, fit)
}
\arguments{
  \item{X}{Matrix of locations to be predicted at. Rows for
  points, columns for variables.}

  \item{fit}{a list of settings, including the model fit.
  This can either be a fit created by
  \code{\link{spotModel.train}} or a spotConfig list as
  returned by \code{spot}.}
}
\value{
  the list of settings, including the model fit. This list
  can be employed as the "fit" parameter in
  \code{\link{spotModel.predict}}.
}
\description{
  This function is used to interface the
  \code{spotPredict*} (e.g.:
  \code{\link{spotPredictRandomForest}}) functions. It
  serves as a simple interface to predict new data with on
  a surrogate models.
}
\examples{
## simple test function
sphereFunction <- function(X) rowSums(X^2)
## sample locations
Xtrain <- matrix(runif(20),,2)
## evaluate on test function
y <- sphereFunction(Xtrain)
## train model
model <- spotModel.train(Xtrain,y,"spotPredictLm")
## evaluate model at a new sample location
newy <- spotModel.predict(c(0.5,0.5),model)
}
\seealso{
  \code{\link{spotModel.train}},
  \code{\link{spotModel.func}}
}

