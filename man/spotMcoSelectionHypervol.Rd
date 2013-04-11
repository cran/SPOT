\name{spotMcoSelectionHypervol}
\alias{spotMcoSelectionHypervol}
\title{Sorting by NDS-rank and Hypervolume Contribution, with known points}
\usage{
  spotMcoSelectionHypervol(largeDesign, designY, newsize,
    mergedX, mergedY, ref = NULL)
}
\arguments{
  \item{largeDesign}{the design matrix in the parameter
  space, to be sorted by the associated y-values for each
  objective}

  \item{designY}{objective value matrix. Contains objective
  values associated to largeDesign}

  \item{newsize}{this is the number of points that need to
  be selected, i.e. the seq.design.new.size}

  \item{mergedX}{position of the already known points in
  parameter space (vector of parameter values)}

  \item{mergedY}{y-values of the already known points
  (vector of objective values)}
}
\value{
  largeDesign \cr - The sorted large design
}
\description{
  Sorts the large design for the purpose of multi objective
  optimization with SPOT. First non dominated sorting (NDS)
  rank is used. If the choice of points for the next
  sequential step is not clear by NDS rank, the hypervolume
  contribution of the competing points is recalculated
  sequentially to remove those with the smallest
  contribution.
}
\details{
  In contrast to \code{\link{spotMcoSort}}, this function
  considers the known points in \code{mergedX} and
  \code{mergedY} so that new points will rather be chosen
  in between known points, thus producing a better Pareto
  front. To do so, the known points are added to the set of
  solutions. To ensure that they are not removed, they
  receive infinite hypervolume contribution, and are not
  counted when determining the number of NDS ranks to be
  considered.
}
\keyword{internal}

