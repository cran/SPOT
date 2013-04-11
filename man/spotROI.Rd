\name{spotROI}
\alias{spotROI}
\title{Region Of Interest Constructor}
\usage{
  spotROI(lower, upper, type = "FLOAT", varnames = NULL,
    dimROI = NULL)
}
\arguments{
  \item{lower}{vector or a single number, specifying lower
  boundary of ROI variables}

  \item{upper}{vector or a single number, specifying upper
  boundary of ROI variables}

  \item{type}{vector of strings or single string,
  specifying the data type of the variables. Can be:
  "FLOAT", "INT", "FACTOR"}

  \item{varnames}{vector or NULL, telling the name of each
  variable. Can be NULL, so that default variable names
  will be used.}

  \item{dimROI}{defines the number of variables. If dimROI
  is set (not NULL), the other vectors should have
  length=dimROI, or length=1.}
}
\value{
  returns a data frame containing the ROI ubfirnatuib
}
\description{
  This function can be used to construct a region of
  interest (ROI), to be passed on to spot(). Note, upper ==
  lower is allowed, but no element in upper should be
  smaller than the corresponding element in lower.
}
\examples{
## without varnames or dimROI
alg.roi <- spotROI(c(0,0),c(1,1),c("FLOAT","FLOAT"))
## with varnames
alg.roi <- spotROI(c(0,0),c(1,1),c("FLOAT","FLOAT"),c("VARX1","VARX3"))
## lower and upper only
alg.roi <- spotROI(c(0,1,-2),10)
alg.roi <- spotROI(c(0,1,-2),c(2,3,100))
## with dimROI
alg.roi <- spotROI(-10,10,"FLOAT",dimROI=4)
## with varnames and dimROI
alg.roi <- spotROI(-10,10,"FLOAT",c("x1","x2","x3","x4"),dimROI=4)
}

