\name{spotOcba}
\alias{spotOcba}
\title{spot OCBA...}
\usage{spotOcba(samp.mean, samp.var, samp.count, budget.add, iz=NA, verbose=0)}
\description{spot OCBA}
\details{Spreads the budget in an optimal way for the different design points,
considering a minimization problem}
\references{\code{\link{spotGenerateSequentialDesignOcba}}}
\arguments{\item{samp.mean}{vector of mean values, length nd}
\item{samp.var}{vector of variances, length nd}
\item{samp.count}{vector of repeats performed alredy, length nd}
\item{budget.add}{additional number of repeats, distributed among the nd design points}
\item{iz}{indifference zone}
\item{verbose}{verbosity {0|1|2|3} 0 is no printing}}
