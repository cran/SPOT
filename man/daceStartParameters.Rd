% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/buildKrigingDACE.R
\name{daceStartParameters}
\alias{daceStartParameters}
\title{Start parameter setup DACE}
\usage{
daceStartParameters(n, m, nugget, corr)
}
\arguments{
\item{n}{number of known design points}

\item{m}{dimension (length) of each point}

\item{nugget}{Value for nugget. Default is -1, which means the nugget will be optimized during MLE. In that case, a lower limit of 0.5 and an upper limit of 1 as well as a starting value of 0.999 will added to the three output vectors (theta, lower and upper bounds). This is only relevant for correlation functions that use a nugget (\code{\link{corrnoisygauss}},\code{\link{corrnoisykriging}} )}

\item{corr}{The choice of correlation function (which defines the length and values of theta and bounds): \code{\link{corrnoisykriging}} (default), \code{\link{corrkriging}}, \code{\link{corrnoisygauss}}, \code{\link{corrgauss}}, \code{\link{correxp}}, \code{\link{correxpg}}, \code{\link{corrlin}}, \code{\link{corrcubic}},\code{\link{corrspherical}},\code{\link{corrspline}}. Can also be user supplied (if in the right form).}
}
\value{
returns a list with the following elements: \cr
			\code{theta} Starting point for the internal parameter estimation\cr
			\code{lb} lower bound\cr
			\code{ub} upper bound
}
\description{
This function returns a starting guess for theta, as well as suitable lower and upper bounds.
The result depends on dimensionality of the problem, number of design points, the nugget value and the choice of correlation function.
}
\seealso{
\code{\link{buildKrigingDACE}}
}
\keyword{internal}
