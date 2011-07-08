\name{spotSeqDesignIncreasePlusOne}
\alias{spotSeqDesignIncreasePlusOne}
\title{Increase Sequential Design Plus One...}
\usage{spotSeqDesignIncreasePlusOne(actRepeats)}
\description{Increase Sequential Design Plus One}
\details{This function increases the repeats done in each sequential design step of SPOT by adding 1.
Increasing repeats ensures that SPOT prediction is getting more precise with growing step size.}
\value{number \code{repeats} \cr
- \code{repeats} is the new recommended number of repeats}
\references{\code{\link{SPOT}} \code{\link{spot}} \code{\link{spotSeqDesignIncreaseMultTwo}}}
\arguments{\item{actRepeats}{must hold the number of repeats recommended last time.}}
