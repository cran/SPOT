\name{spotStepInitial}
\alias{spotStepInitial}
\title{SPOT Step: Initial (First SPOT- Step)...}
\usage{spotStepInitial(spotConfig)}
\description{SPOT Step: Initial (First SPOT- Step)}
\details{Creates a sequential design based on the results derived so far. Therefor it is
essential to have another design evaluated before and have a .res file to use.
afterwards the design is extended by 4 columns: CONFIG, REPEATS,STEP, SEED 

uses the functions \code{spotConfig$init.design.func} and \code{link{spotWriteDes}}
that writes a  design to the file <xxx>.des}
\arguments{\item{spotConfig}{the list of all parameters is given, but the used ones are: \cr 
\code{spotConfig$init.design.func} holds the spotCreateDesign<XXX> function to be used 
for building an initial design. \cr 
\code{spotConfig$init.design.size} number of points that should be created for the initial design \cr
\code{spotConfig$init.design.retries} gives the number of trials to find a design with the greates minimal distance, (default is 1)\cr 
\code{spotConfig$init.design.repeats} number of repeats for one initial design-point\cr
\code{spotConfig$io.colname.repeats} column name for data of "spotConfig$init.design.repeats"\cr
\code{spotConfig$io.colname.step} if given a second Column is created \cr
\code{spotConfig$alg.seed} seed value for reproducable runs\cr
\code{spotConfig$srcPath} source path as given when spot() is called (or uses default)\cr
\code{spotConfig$io.verbosity} verbosity for command window output, which is passed to the output function}}
