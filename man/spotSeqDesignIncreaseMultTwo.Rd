\name{spotSeqDesignIncreaseMultTwo}
\alias{spotSeqDesignIncreaseMultTwo}
\title{Increase Sequential Design Mult Two...}
\usage{spotSeqDesignIncreaseMultTwo(actRepeats)}
\description{Increase Sequential Design Mult Two}
\details{This function increases the repeats done in each sequential design step of SPOT by multiplying by 2.
Increasing repeats ensures that SPOT prediction is getting more precise with growing step size.}
\value{number \code{repeats} \cr
- \code{repeats} is the new recommended number of repeats}
\references{\code{\link{SPOT}} \code{\link{spot}} \code{\link{spotSeqDesignIncreasePlusOne}}}
\arguments{\item{actRepeats}{must hold the number of repeats recommended last time.}}
