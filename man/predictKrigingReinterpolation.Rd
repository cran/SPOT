% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/buildKrigingForrester.R
\name{predictKrigingReinterpolation}
\alias{predictKrigingReinterpolation}
\title{Predict Kriging Model (Re-interpolating)}
\usage{
predictKrigingReinterpolation(object, newdata, ...)
}
\arguments{
\item{object}{Kriging model (settings and parameters) of class \code{kriging}.}

\item{newdata}{design matrix to be predicted}

\item{...}{not used}
}
\value{
list with predicted mean \code{y}, uncertainty \code{s} (optional) and expected improvement \code{ei} (optional).
Whether \code{s} and \code{ei} are returned is specified by the vector of strings \code{object$target},
which then contains "s" and "ei.
}
\description{
Kriging predictor with re-interpolation to avoid stalling the optimization process
which employs this model as a surrogate.
This is supposed to be used with deterministic experiments,
which do need a non-interpolating model that avoids predicting non-zero error at sample locations.
This can be useful when the model is deterministic
(i.e. repeated evaluations of one parameter vector do not yield different values)
but does have a "noisy" structure (e.g. due to computational inaccuracies, systematical error).
}
\details{
Please note that this re-interpolation implementation will not necessarily yield values of
exactly zero at the sample locations used for model building. Slight deviations can occur.
}
\examples{
\donttest{
## Create design points
x <- cbind(runif(20)*15-5,runif(20)*15)
## Compute observations at design points (for Branin function)
y <- funBranin(x)
## Create model
fit <- buildKriging(x,y,control=list(reinterpolate=FALSE))
fit$target <- c("y","s")
## first estimate error with regressive predictor
sreg <- predict(fit,x)$s
## now estimate error with re-interpolating predictor
sreint <- predictKrigingReinterpolation(fit,x)$s
## equivalent:
fit$reinterpolate <- TRUE
sreint2 <- predict(fit,x)$s
print(sreg)
print(sreint)
print(sreint2)
## sreint should be close to zero, significantly smaller than sreg
}
}
\seealso{
\code{\link{buildKriging}}, \code{\link{predict.kriging}}
}
\keyword{internal}
