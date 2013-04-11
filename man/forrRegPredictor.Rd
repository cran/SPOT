\name{forrRegPredictor}
\alias{forrRegPredictor}
\title{Predict Forrester Model}
\usage{
  forrRegPredictor(x, ModelInfo, pred.all = FALSE)
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
  Predict samples on a Forrester Kriging model.
}
\examples{
## Create design points
x = cbind(runif(20)*15-5,runif(20)*15)
## Compute observations at design points (for Branin function)
y = as.matrix(apply(x,1,spotBraninFunction))
## Create model
fit = forrBuilder(x,y)
## Create candidate design points
xx = cbind(runif(20)*15-5,runif(20)*15)
## Predict candidates
y1 = forrRegPredictor(xx,fit)$f
## Plot model (in comments, due to time consumption)
#fn <- function(x){forrRegPredictor(as.matrix(x),fit)$f}
#spotSurf3d(fn,c(-5,0),c(10,15))
## Plot real function
#spotSurf3d(function(x){apply(x,1,spotBraninFunction)},c(-5,0),c(10,15))
}
\seealso{
  \code{\link{forrBuilder}}
  \code{\link{forrReintPredictor}}
}

