\name{spotPlotBst}
\alias{spotPlotBst}
\title{Plot Best Solution Found so far}
\usage{
  spotPlotBst(spotConfig)
}
\arguments{
  \item{spotConfig}{the list of all parameters is given,
  used here \code{alg.roi}: the region of interest reduced
  to a matrix}
}
\description{
  Function used to continuously plot the actually retrieved
  best value throughout a SPOT run. The number of variables
  shown is limited to twelve - make sure the relevant
  variables belong to the first lines of your .roi-file.
}
\seealso{
  \code{\link{SPOT}} \code{\link{spot}}
  \code{\link{spotReadBstFile}}
}
\keyword{internal}

