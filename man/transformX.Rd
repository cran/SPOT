% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spotTools.R
\name{transformX}
\alias{transformX}
\title{Transform input}
\usage{
transformX(xNat = NA, fn = vector())
}
\arguments{
\item{xNat}{\code{matrix} with natural variables. Default: \code{NA}.}

\item{fn}{vector of transformation functions names (\code{char}). Default: Empty
vector (\code{vector()}).}
}
\value{
\code{matrix} of transformed parameters
}
\description{
Transform input variables
}
\examples{
f2 <- function(x){2^x}
fn <- c("identity", "exp", "f2")
xNat <- diag(3)
transformX(xNat, fn)

fn <- append(fn, c("sin", "cos", "tan"))
xNat <- cbind(xNat, xNat)
transformX(xNat, fn)
}
