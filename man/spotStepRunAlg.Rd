\name{spotStepRunAlg}
\alias{spotStepRunAlg}
\title{SPOT Step Algorithm Call...}
\usage{spotStepRunAlg(spotConfig)}
\description{SPOT Step Algorithm Call}
\details{This is the second SPOT Step after step "initial" - but also needed 
after each step "sequential", and is a call frame for the algorithm-call.

The algorithm is the heart of what the user must provide, but SPOT should be 
able to handle them in the most flexible manner. So this is the interface 
providing several possibilities to include an algorithm as (unix)-shell or as 
R-function (the latter could also provide a system call, so it should be able
to integrate user written algorithms as flexible as possible. 
Result/Effects: external, user written function is called and expected to
write the results into the file: spotConfig$io.resFileName}
\references{\code{\link{SPOT}} \code{\link{spot}} \code{\link{spotStepInitial}}
\code{\link{spotStepSequential}}}
\arguments{\item{spotConfig}{the list of all parameters is given, but the used ones are:\cr 
\code{spotConfig$alg.language} switches between R-source or (unix)-shell/binary -call \cr
\code{spotConfig$algSourceSrcPath} needed for an R-function call: 
must hold the path to the R-File - either relative to the config-file or
as absolute path \cr
\code{spotConfig$alg.func} the name of the R-Function (or the binary/shell) accordingly\cr
\code{spotConfig$io.apdFileName} filename for the problem definition of the algorithm, 
first parameter of the generically defined R-function spotConfig$alg.func \cr
\code{spotConfig$io.desFileName} filename for the input of the algorithm, 
second parameter of the generically defined R-function spotConfig$alg.func \cr
\code{spotConfig$io.resFileName} filename for the output of the algorithm
third parameter of the generically defined R-function spotConfig$alg.func\cr
\code{spotConfig$io.verbosity} verbosity for command window output, which is passed to the output function}}
