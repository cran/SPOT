\name{spotStepAutoOpt}
\alias{spotStepAutoOpt}
\title{SPOT Step Auto Opt...}
\usage{spotStepAutoOpt(spotConfig)}
\description{SPOT Step Auto Opt}
\details{spotStepAutoOpt is the default task called, when spot is started.  

The \code{auto} task calls the tasks \code{init} and \code{run} once
and loops \code{auto.loop.steps} times over the steps \code{seq}  and \code{run}
finalising the function with a call of the report function. Instead of \code{auto.loop.steps}
also \code{auto.loop.nevals} can be used as a stopping criterion.}
\references{\code{\link{SPOT}} \code{\link{spot}} \code{\link{spotStepInitial}}
\code{\link{spotStepSequential}} \code{\link{spotStepRunAlg}} \code{\link{spotStepReport}} 
\code{\link{spotGetOptions}}}
\arguments{\item{spotConfig}{the list of all parameters is given, it is forwarded to the call of the reportfunction
the used parameters of spotConfig are just spotConfig$auto.loop.steps
specifying the number of meta modells that should be calculated}}
