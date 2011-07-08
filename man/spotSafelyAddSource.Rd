\name{spotSafelyAddSource}
\alias{spotSafelyAddSource}
\title{Spot Safely Add Source...}
\usage{spotSafelyAddSource(srcPath, functionName, set.io.verbosity)}
\description{Spot Safely Add Source}
\details{spotSafelyAddSource is a call frame for all user written 
R-functions 

All these functions to be sourced in any case, MUST follow this rule: 
The name of the file MUST be THE SAME as the function that should be  
included (plus extension of the two characters ".R" to indicate the R-File) 
This interface may be used for adding
- spotCreateDesign<xxx> functions
- spotPredictor<xxx> functions
- spotReport<xxx> functions
- but also for the algorithm, where the srcPath cannot be \code{spotConfig$srcPath}
Result/Effects: sources external, user written function or does nothing if the 
function alreadey exists
returns Error if 
1) file is not found or 
2) function is not part of the existing file 
after including a function you can use it by 
eval(call(functionName, <functionParamList>))}
\value{boolean \cr 
- The boolean tells if adding the source was successfull (TRUE) or not (FALSE)}
\arguments{\item{srcPath}{location where to find the R-file to be sourced (mostly: \code{spotConfig$srcPath})}
\item{functionName}{the name of a R-function that MUST be part of a sourceFile
with the same name, mostly}
\item{set.io.verbosity}{needed only for transfer to \code{\link{spotWriteLines}}, mostly \code{spotConfig$io.verbosity}}}
