\name{spotModelDescentLm}
\alias{spotModelDescentLm}
\title{Steepest Descent on RSM (linear model)}
\usage{
  spotModelDescentLm(startPoint, spotConfig)
}
\arguments{
  \item{startPoint}{not used here}

  \item{spotConfig}{list of all options, needed to provide
  data for calling functions.}
}
\value{
  returns the list \code{spotConfig} with two new
  entries:\cr \code{spotConfig$optDesign} are the
  parameters of the new minimal design point \cr
  \code{spotConfig$optDesignY} is the associated value of
  the objective function
}
\description{
  Optimizes an existing fit of a linear model created by
  the rsm function. Uses steepest descent method and
  adaption of ROI alternatingly.
}
\seealso{
  \code{\link{spotModelParetoOptim}},
  \code{\link{spotModelOptim}}
}

