% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funContinuous.R
\name{funIshigami}
\alias{funIshigami}
\title{Ishigami Test Function (No. 7)}
\usage{
funIshigami(x, a = 7, b = 0.1)
}
\arguments{
\item{x}{(\code{m,3})-matrix of points to evaluate with the function.
Values should be >= -pi and <= pi, i.e., x_i in [-pi,pi].}

\item{a}{coefficient (optional), with default value 7}

\item{b}{coefficient (optional), with default value 0.1}
}
\value{
1-column matrix with resulting function values
}
\description{
An implementation of the 3-dim Ishigami function.

f(x) = sin(x_1) + a sin^2(x_2) + b x_3^4sin(x_1)

The Ishigami function of Ishigami & Homma (1990) is used as an example for
uncertainty and sensitivity analysis methods,
because it exhibits strong nonlinearity and nonmonotonicity.
It also has a peculiar dependence on x_3, as described by Sobol' & Levitan (1999).
The independent distributions of the input random variables are usually:
x_i ~ Uniform[-pi, pi ], for all i = 1, 2, 3.
}
\examples{
x1 <- matrix(c(-pi, 0, pi),1,)
funIshigami(x1)

}
\references{
Ishigami, T., & Homma, T. (1990, December).
An importance quantification technique in uncertainty analysis for computer models.
In Uncertainty Modeling and Analysis, 1990. Proceedings.,
First International Symposium on (pp. 398-403). IEEE.

Sobol', I. M., & Levitan, Y. L. (1999). On the use of variance reducing
multipliers in Monte Carlo computations of a global sensitivity index.
Computer Physics Communications, 117(1), 52-61.
}
