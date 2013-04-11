\name{dacePredictor}
\alias{dacePredictor}
\title{DACE predictor}
\usage{
  dacePredictor(x, fit, GRAD = FALSE, MSE = FALSE,
    GRADMSE = FALSE)
}
\arguments{
  \item{x}{\code{mx} candidate points of dimension \code{n}
  to be predicted. That is, a matrix with \code{mx} rows
  and \code{n} columns.}

  \item{fit}{the model fit, as returned by
  \code{\link{daceBuilder}}}

  \item{GRAD}{specify whether gradient of response should
  be computed (default is FALSE, which means no). Even if
  GRAD is TRUE, the gradient will only be computed in case
  of a single design point \code{mx==1}.}

  \item{MSE}{specify whether estimated MSE of response
  should be computed (default is FALSE, which means no)}

  \item{GRADMSE}{specify whether gradient of MSE should be
  computed (default is FALSE, which means no).  Even if
  GRADMSE is TRUE, the gradient will only be computed in
  case of a single design point \code{mx==1}.}
}
\value{
  returns a list with the following elements:
  \item{\code{f}}{ Predicted response \code{y} at design
  points \code{x} (always)} \item{\code{df}}{ Gradient of
  response \code{y} at design points \code{x} (only if:
  \code{GRAD==TRUE} and \code{mx==1})} \item{\code{s}}{
  Estimated MSE (only if: \code{MSE==TRUE})}
  \item{\code{ds}}{ Gradient of MSE (only if:
  \code{GRADMSE==TRUE} and \code{mx==1})}
}
\description{
  Predicts y(x) for a given DACE model (i.e. as created by
  \code{\link{daceBuilder}}). The user can choose whether
  to predict only mean or if he is also interested in
  gradient, mean squared error MSE, or the MSE gradient.
}
\examples{
## Create design points
x = cbind(runif(20)*15-5,runif(20)*15)
## Compute observations at design points (for Branin function)
y = apply(x,1,spotBraninFunction)
## Create model
fit = daceBuilder(x,y)
## Create candidate design points
xx = cbind(runif(20)*15-5,runif(20)*15)
## Predict candidates
y1 = dacePredictor(xx,fit)$f
## Plot residuals
plot(y1 - apply(xx,1,spotBraninFunction))
## Plot model (in comments, due to time consumption)
#fn <- function(x){dacePredictor(as.matrix(x),fit)$f}
#spotSurf3d(fn,c(-5,0),c(10,15))
## Plot real function
#spotSurf3d(function(x){apply(x,1,spotBraninFunction)},c(-5,0),c(10,15))
}
\author{
  The authors of the original DACE Matlab toolbox
  \url{http://www2.imm.dtu.dk/~hbni/dace/} are Hans Bruun
  Nielsen \email{hbn@imm.dtu.dk}, Soren Nymand Lophaven and
  Jacob Sondergaard. \cr Additional code for generalization
  to different models by Tobias Wagner
  \email{wagner@isf.de}. \cr Porting and adaptation to R
  and further extensions by Martin Zaefferer
  \email{martin.zaefferer@fh-koeln.de}.
}
\references{
  S.~Lophaven, H.~Nielsen, and J.~Sondergaard. {DACE---A
  Matlab Kriging Toolbox}. Technical Report
  IMM-REP-2002-12, Informatics and Mathematical Modelling,
  Technical University of Denmark, Copenhagen, Denmark,
  2002.
}
\seealso{
  \code{\link{spotPredictDace}} \code{\link{daceBuilder}}
}

