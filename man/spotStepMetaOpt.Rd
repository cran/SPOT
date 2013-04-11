\name{spotStepMetaOpt}
\alias{spotStepMetaOpt}
\title{SPOT Step Meta}
\usage{
  spotStepMetaOpt(spotConfig)
}
\arguments{
  \item{spotConfig}{the list of all parameters is given}
}
\description{
  Attention: This feature is work in progress,
  documentation is not up to date.
}
\details{
  The \code{meta} task calls spotStepMetaOpt which itself
  calls \code{\link{spot}} with several different fixed
  parameters to provide a mixed optimization mechanism:
  analyse a fully qualified test of some parameters and the
  intelligent optimization of other parameters. e.g. the
  number of the dimension of a problem etc.

  To start this step you could for example do this:\cr
  \code{spot("configFileName.conf","meta")}\cr
}
\seealso{
  \code{\link{spotGetOptions}}
}

