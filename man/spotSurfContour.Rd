\name{spotSurfContour}
\alias{spotSurfContour}
\title{spotSurfContour}
\usage{
  spotSurfContour(f = function(x) {     sum(x^2) },
    lo = c(0, 0), up = c(1, 1), s = 100, xlab = "x1",
    ylab = "x2", title = " ", levels = NULL, points1,
    points2, ...)
}
\arguments{
  \item{f}{function to be plotted. The function should
  either be able to take two vectors or one matrix
  specifying sample locations. i.e. \code{y=f(X)} or
  \code{y=f(x2,x1)} where Z is a two column matrix
  containing the sample locations \code{x1} and \code{x2}.}

  \item{lo}{lower boundary for x1 and x2 (defaults to
  \code{c(0,0)}).}

  \item{up}{upper boundary (defaults to \code{c(1,1)}).}

  \item{s}{number of samples along each dimension. e.g.
  \code{f} will be evaluated \code{s^2} times.}

  \item{xlab}{lable of first axis}

  \item{ylab}{lable of second axis}

  \item{title}{title of the plot}

  \item{levels}{number of levels for the plotted function
  value. Will be set automatically with default NULL.}

  \item{points1}{can be omitted, but if given the points in
  this matrix are added to the plot in form of dots}

  \item{points2}{can be omitted, but if given the points in
  this matrix are added to the plot in form of crosses}

  \item{...}{additional parameters passed to \code{f}}
}
\description{
  Simple surface plot, using the filled.contour function.
}
\examples{
spotSurfContour(function(x){apply(x,1,spotBraninFunction)},c(-5,0),c(10,15))
}
\seealso{
  \code{\link{spotSurf3d}}
}

