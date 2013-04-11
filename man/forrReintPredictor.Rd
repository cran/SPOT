\name{forrReintPredictor}
\alias{forrReintPredictor}
\title{Predict Forrester Model (Re-interpolating)}
\usage{
  forrReintPredictor(x, ModelInfo, pred.all = FALSE)
}
\arguments{
  \item{x}{design matrix to be predicted}

  \item{ModelInfo}{fit of the Kriging model (settings and
  parameters)}

  \item{pred.all}{if TRUE return all (RMSE and prediction,
  in a dataframe), else return only prediction}
}
\value{
  Returned value is dependent on the setting of
  \code{pred.all}\cr TRUE: data.frame with columns f
  (function values) and s (RMSE)\cr FALSE: vector of
  function values only
}
\description{
  Kriging predictor with re-interpolation to avoid stalling
  the optimization process which employs this model as a
  surrogate. This is supposed to be used with deterministic
  experiments, which do need a non-interpolating model that
  avoids predicting non-zero error at sample locations.
  This can be useful when the model is deterministic (i.e.
  repeated evaluations of one parameter vector do not yield
  different values) but does have a "noisy" structure (e.g.
  due to computational inaccuracies, systematical error).
}
\details{
  Please note that this re-interpolation implementation
  will not necessarily yield values of exactly zero at the
  sample locations used for model building. Slight
  deviations can occur.
}
\examples{
## Create design points
x = cbind(runif(20)*15-5,runif(20)*15)
## Compute observations at design points (for Branin function)
y = as.matrix(apply(x,1,spotBraninFunction))
## Create model
fit = forrBuilder(x,y)
## first estimate error with regressive predictor
sreg = forrRegPredictor(x,fit,TRUE)$s
## now estimate error with re-interpolating predictor
sreint = forrReintPredictor(x,fit,TRUE)$s
print(sreg)
print(sreint)
## sreint should be close to zero, significantly smaller than sreg
}
\seealso{
  \code{\link{forrBuilder}} \code{\link{forrCoBuilder}}
  \code{\link{forrRegPredictor}}
}

