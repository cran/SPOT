\name{spotRgpTargetFunction}
\alias{spotRgpTargetFunction}
\title{Target function for RGP / SPOT tuning}
\usage{
  spotRgpTargetFunction(populationSize = 100,
    tournamentSize = 10, time = 120)
}
\arguments{
  \item{populationSize}{the \code{populationSize}
  parameter, as used by \code{symbolicRegression()} in
  \code{rgp}}

  \item{tournamentSize}{the \code{tournamentSize}
  parameter, as used by \code{makeTournamentSelection()} in
  \code{rgp}}

  \item{time}{the time for the stopping criterion of the
  rgp run, in seconds}
}
\value{
  this function returns the RMSE (fitness) of the best
  individual in the population
}
\description{
  This target function is used by
  \code{\link{spotAlgStartRgp}} to start rgp with different
  population or tournament sizes.
}
\seealso{
  \code{\link{spotAlgStartRgp}}
}

