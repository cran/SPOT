\name{spotMcoSort}
\alias{spotMcoSort}
\title{Sorting by NDS-rank and Hypervolume Contribution}
\usage{
  spotMcoSort(largeDesign, designY, newsize)
}
\arguments{
  \item{largeDesign}{the design matrix in the parameter
  space, to be sorted by the associated y-values for each
  objective}

  \item{designY}{objective value matrix. Contains objective
  values associated to largeDesign}

  \item{newsize}{this is the number of points that need to
  be selected, i.e. the seq.design.new.size}
}
\value{
  largeDesign \cr - The sorted large design
}
\description{
  Sorts the large design for the purpose of multi objective
  optimization with SPOT. First non dominated sorting rank
  (NDS) is used. If the choice of points for the next
  sequential step is not clear by nds rank, the hypervolume
  contribution of the competing points is recalculated
  sequentially to remove those with the smallest
  contribution.
}

