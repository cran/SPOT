% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funContinuous.R
\name{funPowellS}
\alias{funPowellS}
\title{funPowellS (No. 19, More No. 13)}
\usage{
funPowellS(x)
}
\arguments{
\item{x}{matrix (dim 1x4) of points to evaluate with the function.
Rows for points and columns for dimension.}
}
\value{
1-column matrix with resulting function values
}
\description{
Powells 4-dim Test Function
}
\examples{
x1 <- matrix(c(0,0,0,0),1,)
funPowellS(x1)
x2 <- matrix(c(3,-1,0,1),1,)
funPowellS(x2)
x3 <- matrix(c(0,0,0,-2),1,)
funPowellS(x3)
# optimization run with SPOT and 15 evaluations
res_fun <- spot(,funPowellS,c(-4,-4,-4,-4 ),c(5,5,5,5),control=list(funEvals=15))
res_fun

}
\references{
More, J. J., Garbow, B. S., and  Hillstrom, K. E. (1981).
Testing unconstrained optimization software.
Trond Steihaug and Sara Suleiman
Global convergence and the Powell singular function
\emph{ACM Transactions on Mathematical Software (TOMS)}, \emph{7}(1), 17-41.
\doi{10.1145/355934.355936}
\url{http://owos.gm.fh-koeln.de:8055/bartz/optimization-ait-master-2020/blob/master/Jupyter.d/Exercise-VIIa.ipynb}
\url{http://bab10.bartzandbartz.de:8033/bartzbeielstein/bab-optimization-ait-master-2020/-/blob/master/Jupyter.d/01spotNutshell.ipynb}
\url{https://www.mat.univie.ac.at/~neum/glopt/bounds.html}


Powells Test function, M. J. D. Powell, 1962
An automatic method for finding the local minimum of a function.
\emph{The Computer Journal}, \emph{3}(3), 175-184.
\url{https://www.sfu.ca/~ssurjano/powell.html}
}
