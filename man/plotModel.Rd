% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotModel.R
\name{plotModel}
\alias{plotModel}
\title{Surface plot of a model}
\usage{
plotModel(
  object,
  which = if (ncol(object$x) > 1 & tolower(type) != "singledim") {
     1:2
 } else {
 
       1
 },
  constant = object$x[which.min(object$y), ],
  xlab = paste("x", which, sep = ""),
  ylab = "y",
  type = "filled.contour",
  ...
)
}
\arguments{
\item{object}{fit created by a modeling function, e.g., \code{\link{buildRandomForest}}.}

\item{which}{a vector with two elements, each an integer giving the two independent variables of the plot 
(the integers are indices of the respective data set).}

\item{constant}{a numeric vector that states for each variable a constant value that it will take on
if it is not varied in the plot. This affects the parameters not selected by the \code{which} parameter.
By default, this will be fixed to the best known solution, i.e., the one with minimal y-value, according
to \code{which.min(object$y)}. The length of this numeric vector should be the same as the number of columns in \code{object$x}}

\item{xlab}{a vector of characters, giving the labels for each of the two independent variables.}

\item{ylab}{character, the value of the dependent variable predicted by the corresponding model.}

\item{type}{string describing the type of the plot:  \code{"filled.contour"} (default), \code{"contour"}, 
\code{"persp"} (perspective), or \code{"persp3d"} plot.
Note that "persp3d" is based on the plotly package and will work in RStudio, but not in the standard RGui.}

\item{...}{additional parameters passed to the \code{contour} or \code{filled.contour} function.}
}
\description{
A (filled) contour or perspective plot of a fitted model.
}
\examples{
## generate random test data
testfun <- function (x) sum(x^2)
set.seed(1)
k <- 30
x <- cbind(runif(k)*15-5,runif(k)*15,runif(k)*2-7,runif(k)*5+22)
y <- as.matrix(apply(x,1,testfun))
fit <- buildLM(x,y)
plotModel(fit)
plotModel(fit,type="contour")
plotModel(fit,type="persp")
plotModel(fit,which=c(1,4))
plotModel(fit,which=2:3)

}
\seealso{
\code{\link{plotFunction}}, \code{\link{plotData}}
}
