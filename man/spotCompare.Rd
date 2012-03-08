\name{spotCompare}
\alias{spotCompare}
\title{Spot Feature Comparison}
\usage{
  spotCompare(configFile, repeats = 2, NoNoiseFun = NULL)
}
\arguments{
  \item{configFile}{vector of filenames to be evaulated}

  \item{repeats}{number of repeats for each file (with
  different spot.seed)}

  \item{NoNoiseFun}{function to compute the result without
  noise. If NULL or left to default this will have no
  effect. If a function name is passed as a string, the
  according function will be called with a noice variable
  set to zero. This way the result can be evaluated without
  noise}
}
\value{
  performance, a matrix of y-Values for each single run.
  One Column for each file.
}
\description{
  Simple Function to compare performance of different spot
  configurations. Example:
  perf=spotCompare(c("test.conf","test2.conf","test3.conf"),10)
}
\keyword{internal}

