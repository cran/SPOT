\name{spotModel.func}
\alias{spotModel.func}
\title{Model Prediction Interface}
\usage{
  spotModel.func(X, y, method = "spotPredictTree",
    settings = NULL)
}
\arguments{
  \item{X}{Matrix of sample locations. Rows for points,
  columns for variables.}

  \item{y}{Observations at sample locations (vector)}

  \item{method}{which model function should be used
  specified as a string, e.g., "spotPredictRandomForest",
  "spotPredictLm", "spotPredictForrester". Most other
  "spotPredict*" functions provided by SPOT should work.}

  \item{settings}{additional settings, as used by the
  corresponding \code{spotPredict*} function. NULL means
  default model settings are used.}
}
\value{
  a function of type y=f(X), where \code{X} is new data,
  \code{f} is the surrogate model, and \code{y} the
  predicted values.
}
\description{
  spotMode.func creates a function which represents the
  chosen surrogate model (see \code{method} parameter). The
  created function will yield values predicted at given new
  locations.
}
\examples{
## simple test function
sphereFunction <- function(X) rowSums(X^2)
## sample locations
Xtrain <- matrix(runif(20),,2)
## evaluate on test function
y <- sphereFunction(Xtrain)
## train model
model <- spotModel.func(Xtrain,y,"spotPredictLm")
## evaluate model at a new sample location
newy <- model(c(0.5,0.5))
## plot model (based on 10*10 point matrix)
spotSurf3d(model,s=10)
}
\seealso{
  Alternatively, \code{\link{spotModel.train}} and
  \code{\link{spotModel.predict}} can be used to do a
  similar job.
}

