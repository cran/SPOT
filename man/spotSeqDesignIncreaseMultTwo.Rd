\name{spotSeqDesignIncreaseMultTwo}
\alias{spotSeqDesignIncreaseMultTwo}
\title{Increase Sequential Design Repeats Mult Two}
\usage{
  spotSeqDesignIncreaseMultTwo(actRepeats)
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
  sequential design step of SPOT by multiplying by 2.
  Increasing repeats ensures that SPOT prediction is
  getting more precise with growing step size.
}
\seealso{
  \code{\link{SPOT}} \code{\link{spot}}
  \code{\link{spotSeqDesignIncreasePlusOne}}
}
\keyword{internal}

