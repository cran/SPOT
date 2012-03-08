\name{spotAlgEsStratMutation}
\alias{spotAlgEsStratMutation}
\title{spotAlgEsStratMutation}
\usage{
  spotAlgEsStratMutation(strat, tau0, tau, sigmaRestart,
    sigmaInit)
}
\arguments{
  \item{strat}{Strat}

  \item{tau0}{Tau0}

  \item{tau}{Tau, learning parameter for self adaption}

  \item{sigmaRestart}{Sigma on restart}

  \item{sigmaInit}{initial sigma value (standard
  deviation)}
}
\value{
  number \code{s} \cr - \code{s} is the new sigma value
}
\description{
  Mutation function for the ES.
}
\keyword{internal}

