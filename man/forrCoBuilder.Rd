\name{forrCoBuilder}
\alias{forrCoBuilder}
\title{Build Forrester Co-Kriging}
\usage{
  forrCoBuilder(Xe, ye, Xc, yc, fitC, loval = 0.001,
    upval = 100, algtheta = "NLOPT_LN_NELDERMEAD",
    budgetalgtheta = 100, lb = NULL, ub = NULL,
    opt.p = FALSE, lambda.loval = -6, lambda.upval = 0,
    rho.loval = -5, rho.upval = 5)
}
\arguments{
  \item{Xe}{design matrix (expensive sample locations)}

  \item{ye}{1-row matrix of expensive observations at Xe}

  \item{Xc}{design matrix (cheap sample locations). The
  bottom of this matrix should contain expensive samples.}

  \item{yc}{1-row matrix of cheap observations at Xc.}

  \item{fitC}{object of class \code{forr}, containing a
  Kriging model build through the cheap observations}

  \item{loval}{lower boundary for theta, default is
  \code{1e-3}}

  \item{upval}{upper boundary for theta, default is
  \code{100}}

  \item{algtheta}{algorithm used to find theta, default is
  \code{"NLOPT_LN_NELDERMEAD"} which is a bounded simplex
  method from the package nloptr. Else, any from the list
  of possible \code{method} values in
  \code{\link{spotOptimizationInterface}} can be chosen.}

  \item{budgetalgtheta}{budget for the above mentioned
  algorithm, default is \code{100}. The value will be
  multiplied with the length of the model parameter vector
  to be optimized.}

  \item{lb}{lower boundary of the design space. Will be
  extracted from the matrix \code{Xe} if not given.}

  \item{ub}{upper boundary of the design space. Will be
  extracted from the matrix \code{Xe} if not given.}

  \item{opt.p}{boolean that specifies whether the exponents
  (\code{p}) should be optimized. Else they will be set to
  two. Default value is \code{FALSE}. Default is highly
  recommended as the implementation of this feature is not
  yet well tested and might be faulty.}

  \item{lambda.loval}{lower boundary for regularization
  constant (nugget), default is \code{-6}.
  (lambda=10^lambda, e.g. 10^-6)}

  \item{lambda.upval}{upper boundary for regularization
  constant (nugget), default is \code{0}.
  (lambda=10^lambda, e.g. 1)}

  \item{rho.loval}{lower boundary for rho, default is
  \code{-5}.}

  \item{rho.upval}{upper boundary for rho, default is
  \code{5}.}
}
\value{
  a fit (list) of class \code{coforr}. This contains
  Co-Kriging specific parameters, as well as two fits of
  class \code{forr} which represent the cheap and expensive
  models.
}
\description{
  This function builds a Co-Kriging model based on code by
  Forrester et al.. Please note that the expensive sample
  locations should be contained in the cheap sample
  locations. Furthermore, it has to be made sure that the
  correlated functions do not yield identical values. That
  is, \code{ye} and \code{yc} should have common sample
  locations, but different values. The sample locations
  only evaluated on the cheap function can be chosen
  arbitrarily.
}
\examples{
## This is the one-variable example described by Forrester et al.
	## The "expensive" function to be modeled is
	ovar <- function(x){(x*6-2)^2*sin((x*6-2)*2)}
	## The "cheap" function to be modeled is
	covar <- function(x){ A=0.5;B=10;C=-5;D=0;
		A*(((x+D)*6-2)^2)*sin(((x+D)*6-2)*2)+((x+D)-0.5)*B+C
	}
	## construct cheap and expensive sample locations
	xe <- rbind(0,0.4,0.6,1)
	xc <- rbind(0.1,0.2,0.3,0.5,0.7,0.8,0.9,0,0.4,0.6,1)
	## get observations of samples
	ye <- rbind(ovar(xe))
	yc <- rbind(covar(xc))
	## build the Co-Kriging model, with cheap and expensive observations
	set.seed(2)
 fitC <- forrBuilder(xc, yc, 1e-3, 1e2, "NLOPT_LN_NELDERMEAD", 100,0,1,FALSE);
	fit <- forrCoBuilder(xe, ye, xc, yc, fitC, 1e-3, 1e2, "NLOPT_LN_NELDERMEAD", 100,0,1,FALSE)
	## build the ordinary Kriging model with expensive observations only
	fit1 <- forrBuilder(xe, ye, 1e-3, 1e2, "NLOPT_LN_NELDERMEAD", 100,0,1,FALSE)
 ## Predict and plot over whole design space
	x=seq(from=0,to=1,by=0.01)
	yco <- forrCoRegPredictor(as.matrix(x),fit,FALSE)$f
	ypc <- forrRegPredictor(as.matrix(x),fitC,FALSE)$f
	ype <- forrRegPredictor(as.matrix(x),fit,FALSE)$f
	yy <- forrRegPredictor(as.matrix(x),fit1,FALSE)$f
	plot(x,ovar(x),type="l",ylim=c(-15,20),lwd=3)
	points(xe,ye,pch=19,cex=1.5)
	points(xc,yc,cex=1.5)
	lines(x,covar(x),lwd=3)
	lines(x,ype,col="blue",lwd=3) #difference model
	lines(x,ypc,col="red",lty=4,lwd=3) 	#cheap model
	lines(x,yy,col="blue",lty=3,lwd=3)#uncorrected model
	lines(x,yco,col="darkgreen",lty=5,lwd=3) #comodel
	legend("top",lwd=c(3,3,1,1,3,3,3,3),col=c("black","black","black","black","blue","red","blue","darkgreen"),legend=c("Expensive Function", "Cheap Function", "Expensive Observations", "Cheap Observations", "Uncorrected Model", "Cheap Model","Difference Model","Co-Kriging Model"),lty=c(1,1,0,0,3,4,1,5),pch=c(NA,NA,19,1,NA,NA,NA,NA))
	sum((yco-ovar(x))^2)/length(x) #mse
}
\references{
  Forrester, Alexander I.J.; Sobester, Andras; Keane, Andy
  J. (2008). Engineering Design via Surrogate Modelling - A
  Practical Guide. John Wiley & Sons.

  FORRESTER, A.I.J, SOBESTER A. & KEAN, A.J. (2007),
  Multi-Fidelity optimization via surrogate modelling.
  \emph{Proc. R. Soc. A} 463, 3251-3269. \cr Forrester,
  Alexander I.J.; Sobester, Andras; Keane, Andy J. (2008).
  Engineering Design via Surrogate Modelling - A Practical
  Guide. John Wiley & Sons.
}
\seealso{
  \code{\link{spotPredictCoForrester}}
  \code{\link{forrCoRegPredictor}}
  \code{\link{forrBuilder}}
}

