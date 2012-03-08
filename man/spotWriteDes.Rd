\name{spotWriteDes}
\alias{spotWriteDes}
\title{Spot Write Design}
\usage{
  spotWriteDes(des, verbosity, sep, filename)
}
\arguments{
  \item{des}{design provided by any
  spotCreateDesignXXX()-function}

  \item{verbosity}{for values greater than two, a message
  is given}

  \item{sep}{the column separator (should not be empty for
  writing table to .des-file)}

  \item{filename}{the filename the design should be written
  to}
}
\description{
  help function that simply writes Data to the .des-file
}
\details{
  Result/Effects: rewrites the design-file for the next
  call of the \code{\link{spotStepRunAlg}}
}
\seealso{
  \code{\link{spotStepRunAlg}}
}

