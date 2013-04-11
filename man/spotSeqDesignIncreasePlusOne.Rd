\name{spotSeqDesignIncreasePlusOne}
\alias{spotSeqDesignIncreasePlusOne}
\title{Increase Sequential Design Repeats Plus One}
\usage{
  spotSeqDesignIncreasePlusOne(actRepeats)
}
\arguments{
  \item{actRepeats}{must hold the number of repeats
  recommended last time.}
}
\value{
  number \code{repeats} \cr - \code{repeats} is the new
  recommended number of repeats
}
\description{
  This function increases the repeats done in each
  sequential design step of SPOT by adding 1. Increasing
  repeats ensures that SPOT prediction is getting more
  precise with growing step size.
}
\seealso{
  \code{\link{spot}}
  \code{\link{spotSeqDesignIncreaseMultTwo}}
}
\keyword{internal}

