#' @title spotSeverity
#' 
#' @param xbar			sample mean value
#' @param mu0			mean value of the null hypothesis (usually referred to as H0)
#' @param mu1			mean value of the alternative hypothesis (usually referred to as H1)
#' @param n				sample size in each arm, e.g., if 20 samples are available, then \code{n=10} 
#' regardless whether the samples are paired/blocked (\code{paired=TRUE}) or independent
#' (\code{paired=FALSE}). Degrees of freedom will be modified internally
#' according to the setting of the \code{paired} argument.
#' @param sigma			sample s.d. Will be used to determine s.d. of the differences (if 
#' \code{paired==TRUE}) or s.d. of the pooled s.d (if \code{paired==FALSE}).
#' @param alpha			probability of a type I error, given H0 is true 	
#' @param tdist logical. Use Student t Distribution. Default: FALSE
#' @param paired logical. Paired (blocked) data. Default: TRUE
#' 
#' @importFrom stats qt
#' @importFrom stats pt
#'
#' @return an object of class \code{"spotSeverity"},
#' with a \code{summary} method and a \code{print} method.
#' 
#' @export 
#' @examples  
#' s0 <- spotSeverity(xbar=0.4, mu0=0.0, mu1=0.6, n=25, sigma=1, alpha=0.03)
#' print(s0)
#' s1 <- spotSeverity(xbar=0.4, mu0=0.6, mu1=0.6, n=25, sigma=1, alpha=0.03)
#' print(s1)
#' s2 <- spotSeverity(xbar=0, mu0=0.6, mu1=0.6, n=25, sigma=1, alpha=0.03)
#' print(s2)
#' 
#' ## Example from Mayo, p345
#' spotSeverity(xbar=90, mu0=0, mu1= 200, n=200, sigma = 450, alpha = 0.025, 
#' paired = FALSE, tdist = FALSE)
#' 
#' ## Example from Vena02a to compare with results from t.test()
#' ## library("BHH2")
#' ## data(shoes.data)
#' ## A <- shoes.data$matA
#' ## B <- shoes.data$matB
#'  A <- c(13.2, 8.2, 10.9, 14.3, 10.7, 6.6, 9.5, 10.8, 8.8, 13.3)
#'  B <- c(14, 8.8, 11.2, 14.2, 11.8, 6.4, 9.8, 11.3, 9.3, 13.6)
#' t.paired <- t.test(x = A, y = B, var.equal = TRUE, paired = TRUE, 
#' alternative = "greater",  conf.level = 0.95)
#' xbar <- mean(A-B)
#' n <- length(A)
#' sigma <- sd(A-B) 
#' s.paired <- spotSeverity(xbar=xbar,mu0=0, mu1= 1, n=n, sigma = sigma, 
#' alpha = 0.025, tdist = TRUE)
#' 
spotSeverity <- function(xbar,
                         mu0,
                         mu1, 
                         n, 
                         sigma, 
                         alpha,
                         tdist = FALSE,
                         paired = TRUE){
  method <- df <- stderr <- NULL
  if(paired==TRUE){
    df <- n-1
  }else{ 
    ## independent, paired == false
    method <- " Two Sample t-test"
     df <- 2*n-2
    sigma <- sqrt(2)*sigma
  }
  stderr <- sigma/sqrt(n)
  if(tdist){
    if(paired == TRUE){
      method <- "Paired t-test"}
    #statistic <- (xbar-mu0)*sqrt(n)/sigma
    statistic <- (xbar-mu0)/stderr
    quantPoint <- qt(p = 1-alpha, df = df)
    #cutOffPoint <- quantPoint*sigma/sqrt(n)+mu0
    cutOffPoint <- quantPoint*stderr+mu0
    rejectH0 <- statistic > quantPoint
    p.value <- 1- pt( q = statistic, df = df)
    severity <- pt( q = (xbar-mu1)/stderr , df = df)
    ncp <- (mu1-mu0)/stderr
    power <- 1-pt( q =  quantPoint, df= df, ncp = ncp)
    # power <- pt( q = (mu1-mu0)/stderr, df= df)
  }
  else{
    if(paired == TRUE){
      method <- "Paired z-test"}
    statistic <- (xbar-mu0)/stderr 
    quantPoint <- qnorm(p= 1-alpha)
    cutOffPoint <- quantPoint*stderr+mu0
    rejectH0 <- statistic > quantPoint
    p.value <- 1- pnorm( q = statistic)
    severity <- pnorm(q = (xbar-mu1)/stderr)
    power <- pnorm(q = (mu1-mu0)/stderr - quantPoint)
    # power <- pnorm(q = (mu1-mu0)/stderr)
  }
  
  if (rejectH0){
    sev <- list(severity=severity,
                decision="H0 rejected")
  } else{
    sev <- list(severity=1-severity,
                decision="H0 not rejected")
  }
  sev$quantPoint <- quantPoint
  sev$statistic <- statistic
  sev$cutOffPoint <- cutOffPoint
  sev$stderr <- stderr
  sev$p.value <- p.value
  sev$power <- power
  sev$xbar <- xbar
  sev$mu0 <- mu0
  sev$mu1 <- mu1
  sev$n <- n
  sev$df <- df
  sev$sigma <- sigma
  sev$alpha <- alpha
  sev$tdist <- tdist
  sev$paired <- paired
  sev$method <- method
  sev$alternative <- "greater"
  sev$rejectH0 <- rejectH0
  class(sev) <- "spotSeverity"
  return(sev)
}

#' Print method for spotSeverity
#'
#' Wrapper for \code{print.spotSeverity}.
#'
#' @param object an object of class \code{"spotSeverity"}, produced by \code{\link{spotSeverity}}.
#' @param ... not used
#'
#' @export
#' @keywords internal
print.spotSeverity <- function(x, ...) {
  str(x)
}

#' @title Plot method for spotSeverity
#' @param x severity object
#' @param add	default value is FALSE
#' @param rangeLeft	range default:\code{-1}
#' @param rangeRight	range default:\code{1}
#' @param plotSev logical. plot severity. Default: TRUE
#' @param plotPow logical. plot power. Default: FALSE
#' @param cl color, e.g.,  \code{c("black", "red", "green", " blue" , "brown", 
#' "cyan", "darkred", "gray",  "green", "magenta", "orange")}
#' @param xlab x axis label
#' @param ylab y axis label
#' @param ... additional parameters
#'
#' @importFrom graphics points
#' @importFrom graphics plot
#' @importFrom graphics abline
#' @return 		description of return value
#' 
#' @examples 
#' ### Example from D G Mayo and A Spanos. 
#' ### Severe Testing as a Basic Concept in a Neyman–Pearson Philosophy of Induction. 
#' ### British Journal for the Philosophy of Science, 57:323–357, 2006. (fig 2):
#' x0 <- 12.1
#' mu1 <- seq(11.9,13,0.01)
#' n <- 100
#' sigma <- 2
#' alpha <- 0.025
#' tdist <- FALSE
#' plot(mu1, spotSeverity(xbar=x0, mu0=0, mu1=mu1, n=n, sigma=sigma, alpha=alpha, 
#' tdist=tdist)$severity, type = "l", ylim=c(0,1), col="blue")
#' abline(h=0)
#' abline(h=1)
#'  abline(h=0.95)
#' abline(v=12.43)
#' ### plot power:
#' mu0 <- 12
#' points(mu1, spotPower(alpha, mu0, mu1, n, sigma), type = "l", ylim=c(0,1), 
#' col="green")
#' abline(v=12.72)
#' 
#' ## Fig 13.11 in Span19a
#' p <- spotSeverity(xbar=10, mu0=10, mu1= 10.2, n=100, sigma = 1, alpha = 0.05, tdist = FALSE)
#' plot(p, rangeLeft = 10, rangeRight = 10.5, plotPow = TRUE)
#' 
#' @export 
plot.spotSeverity <- function(x,
                              add=FALSE,
                              rangeLeft=-1,
                              rangeRight=1,
                              plotSev = TRUE,
                              plotPow = FALSE,
                              cl = "black",
                              xlab="x",
                              ylab="y",
                              ...){
  xbar <- x$xbar
  mu0 <- x$mu0
  mu1 <- seq(rangeLeft, rangeRight, length.out = 100)
  n <- x$n
  sigma <- x$sigma
  alpha <- x$alpha
  tdist <- x$tdist
  paired <- x$paired
  if(add){
    if(plotSev){
    points(mu1, spotSeverity(xbar=xbar, mu0=mu0, mu1=mu1, n=n, sigma=sigma, alpha=alpha, tdist=tdist)$severity, type = "l", ylim=c(0,1), col=cl)}
    if(plotPow){
    points(mu1, spotSeverity(xbar=xbar, mu0=mu0, mu1=mu1, n=n, sigma=sigma, alpha=alpha, tdist=tdist)$power, type = "l", ylim=c(0,1), col=cl)      
    }
  }else{
    if(plotSev){
    plot(mu1, spotSeverity(xbar=xbar, mu0=mu0, mu1=mu1, n=n, sigma=sigma, alpha=alpha, tdist=tdist)$severity, type = "l", xlab=xlab, ylab=ylab, lwd=2, ylim=c(0,1), col = cl)
    }
    if(plotPow){
      plot(mu1, spotSeverity(xbar=xbar, mu0=mu0, mu1=mu1, n=n, sigma=sigma, alpha=alpha, tdist=tdist)$power, type = "l", xlab=xlab, ylab=ylab, lwd=2, ylim=c(0,1), col = cl)
    }
   }
}

#' @title getPower
#' 
#' @description Implements basic power calculations in R
#' See also: \url{https://www.cyclismo.org/tutorial/R/power.html}
#'  
#' @param mu0			mean value of the null hypothesis (usually referred to as H0)
#' @param mu1			mean value of the alternative hypothesis (usually referred to as H1)
#' @param n				sample size
#' @param sigma			sample s.d.
#' @param alpha			error 	
#' @param tdist logical. Use Student t Distribution. Default: FALSE
#' @param alternative	a character string specifying the alternative hypothesis, must be one of "two.sided", "greater" (default) or "less". 
#'  
#' @importFrom stats qt
#' @importFrom stats power.t.test
#' 
#' @examples 
#' ## Power should be approx. 0.9183621:
#' getPower(mu0=5, mu1=6.5, n=20, sigma=2, alpha=0.05, tdist = FALSE, 
#' alternative = "two.sided")
#' ## Power should be approx. 0.8887417:
#' getPower(mu0=5, mu1=6.5, n=20, sigma=2, alpha=0.05, tdist = TRUE, 
#' alternative = "two.sided")
#' ## Compare with results from power.t.test
#' powerVal <- power.t.test(n=20, delta=1.5, sd=2, sig.level=0.05, type="one.sample",
#' alternative="two.sided",strict = TRUE)
#' powerVal$power
#' 
#' @export
getPower <-
  function(mu0,
           mu1,
           n,
           sigma,
           alpha,
           tdist = FALSE,
           alternative = "greater") {
    if (alternative == "two.sided") {
      alpha <- alpha / 2
    }
    if (tdist == TRUE) {
      lowerQuantPoint <- qt(alpha, n - 1)
      upperQuantPoint <- qt(1 - alpha, n - 1)
    }
    else {
      lowerQuantPoint <- qnorm(alpha)
      upperQuantPoint <- qnorm(1 - alpha)
    }
    if (alternative == "greater") {
      if (tdist == TRUE) {
        return(1 - pt(upperQuantPoint - sqrt(n) * (mu1 - mu0) / sigma), df=n-1)}
      else{
        return(1 - pnorm(upperQuantPoint - sqrt(n) * (mu1 - mu0) / sigma))
      }
    }
    else if (alternative == "less") {
      if (tdist == TRUE) {
        return(pt(lowerQuantPoint - sqrt(n) * (mu1 - mu0) / sigma), df=n-1)}
      else{
        return(pnorm(lowerQuantPoint - sqrt(n) * (mu1 - mu0) / sigma))
      }
    }
    else{
      if (tdist == TRUE) {
        return(pt(lowerQuantPoint - sqrt(n) * (mu1 - mu0) / sigma, df=n-1) + 
                 (1 - pt(upperQuantPoint - sqrt(n) * (mu1 - mu0) / sigma, df=n-1)))}
      else{
        return(pnorm(lowerQuantPoint - sqrt(n) * (mu1 - mu0) / sigma) + 
                 (1 - pnorm(upperQuantPoint - sqrt(n) * (mu1 - mu0) / sigma)))
      }
    }
  }



#' @title getSampleSize
#' 
#' @description Implements sample size calculations in R
#' See also: \url{https://www.cyclismo.org/tutorial/R/power.html}
#' and \url{https://influentialpoints.com/Training/statistical_power_and_sample_size.htm}
#' @param mu0			mean value of the null hypothesis (usually referred to as H0)
#' @param mu1			mean value of the alternative hypothesis (usually referred to as H1)  
#' @param alpha		type I error	
#' @param beta		type II error	
#' @param sigma			sample s.d.
#' @param alternative	a character string specifying the alternative hypothesis, must be one of "two.sided", "greater" (default) or "less". 
#'  
#' @returns n number of required samples in each arm of a trial. Note: total number of samples is 2*n.  
#' 
#' @examples 
#' 
#' getSampleSize(mu0 = 0, mu1 = 200, alpha=0.05, beta=0.2, sigma=450, 
#' alternative="two.sided")
#' getSampleSize(mu0 = 8.72, mu1 = 8.72*1.1, alpha=0.05, beta=0.2, sigma=1.3825, 
#' alternative="greater")
#' getSampleSize(mu0 = 8.72, mu1 = 8.72*1.1, alpha=0.05, beta=0.2, sigma=1.3825, 
#' alternative="two.sided")
#' 
#' @export
getSampleSize <-
  function(mu0,
           mu1,
           alpha,
           beta,
           sigma,
           alternative = "greater") {
    delta <- mu1 - mu0
    if (alternative == "two.sided") {
      za2 <- qnorm(1 - alpha / 2)
      zb <- qnorm(1 - beta)
      return(2*((za2 + zb) * sigma / delta) ^ 2)
    }
    else{
      za <- qnorm(1 - alpha)
      zb <- qnorm(1 - beta)
      return(2*((za + zb) * sigma / delta) ^ 2)
    }
  }



#' @title spotPower
#' 
#' @description Calculate power
#'
#' @param alpha		description of alpha
#' @param mu0		description of mu0
#' @param mu1		description of mu1
#' @param n			vector length
#' @param sigma		standart deviation
#' 
#' @importFrom stats pnorm
#' @importFrom stats qnorm
#'
#' @return 		description of return value
#' 
#' @export 
#'
spotPower <- function(alpha, mu0, mu1, n, sigma){
  1 - pnorm( qnorm(1-alpha,0,1) + (mu0-mu1)*sqrt(n)/sigma);
}

#' @title spotPlotPower
#' 
#' @description Plot power 
#'
#' @param y0			First input vector
#' @param y1			Second input vector
#' @param alpha		description of alpha, default value is \code{0.05}
#' @param add			Boolean, default value is \code{FALSE}
#' @param n				number of vector elements that should be evaluated, default value is \code{NA}, which means the whole vector
#' @param rightLimit	description of rightLimit, default value is \code{1} 
#' 
#' @importFrom graphics points
#' @importFrom graphics plot
#' @importFrom graphics abline
#'
#' @return 		description of return value
#' @export 
spotPlotPower <- function(y0, 
                          y1, 
                          alpha=0.05, 
                          add=FALSE, 
                          n=NA, 
                          rightLimit=1){
  ##y1 > y0
  if (is.na(n)){
    n <- length(y0);
  }  
  delta <- y1-y0;
  delta <- delta[1:n];
  sigma <- sd(delta);
  mu1 <- seq(0,rightLimit,length.out=100);
  mu0 <- 0;
  if(add){
    points(mu1, spotPower(alpha, mu0, mu1, n, sigma), type="l", lty=2 , lwd=2, ylim=c(0,1), col="black");
  }else{
    plot(mu1, spotPower(alpha, mu0, mu1, n, sigma), type = "l", ylim=c(0,1));
    abline(h=0);
    abline(h=1);
  }
}

#' @title spotSeverityBasic
#' 
#' @param x0			sample mean value
#' @param mu1			description
#' @param n				description
#' @param sigma			description
#' @param alpha			description	
#' 
#' @importFrom stats qt
#' @importFrom stats pt
#'
#' @return 		description of return value
#' @export 
spotSeverityBasic <- function(x0, 
                         mu1, 
                         n, 
                         sigma, 
                         alpha){
  ## x0 is the sample mean. 
  ## sev <- pnorm( (x0-mu1)*sqrt(n)/sigma)
  sev <- pt( (x0-mu1)*sqrt(n)/sigma , n-1);
  ## rejection of the null:
  ## reject <- x0*sqrt(n)/sigma > qnorm(1-alpha)
  reject <- x0*sqrt(n)/sigma > qt(1-alpha, n-1);
  if (reject){
    print("H0 rejected")
    return(sev)
  }		
  ## acceptance of the null:
  else{
    print("H0 not rected")
    return (1-sev)
  }		
}

#' @title  spotPlotSeverityBasic
#' @param y0			first input vector
#' @param y1			second input vector
#' @param add			default value is FALSE
#' @param n				default value is NA, which means length of y0 will be used for n
#' @param alpha			description
#' @param rightLimit	description of rightLimit, default value is \code{1} 
#'
#' @importFrom graphics points
#' @importFrom graphics plot
#' @importFrom graphics abline
#' @return 		description of return value
#' 
#' @examples 
#' ### Example from D G Mayo and A Spanos. 
#' ### Severe Testing as a Basic Concept in a Neyman–Pearson Philosophy of Induction. 
#' ### British Journal for the Philosophy of Science, 57:323–357, 2006. (fig 2):
#' x0 <- 12.1
#' mu1 <- seq(11.9,13,0.01)
#' n <- 100
#' sigma <- 2
#' alpha <- 0.025
#' plot(mu1, spotSeverityBasic(x0, mu1, n, sigma, alpha), type = "l", ylim=c(0,1), col="blue")
#' abline(h=0)
#' abline(h=1)
#'  abline(h=0.95)
#' abline(v=12.43)
#' ### plot power:
#' mu0 <- 12
#' points(mu1, spotPower(alpha, mu0, mu1, n, sigma), type = "l", ylim=c(0,1), col="green")
#' abline(v=12.72)
#' 
#' @export 
spotPlotSeverityBasic <- function(y0, y1, add=FALSE, n=NA, alpha, rightLimit=1){
  ## y1 > y0
  if (is.na(n)){
    n <- length(y0);
  }  
  delta <- y1-y0;
  delta <- delta[1:n];
  ### The denominator n - 1 is used which gives an unbiased estimator of the sd for i.i.d. observations. 
  sigma <- sd(delta);
  mu1 <- seq(0,rightLimit,length.out=100);
  x0 <- mean(delta);
  print(n)
  print(delta)
  print(x0)
  if(add){
    points(mu1, spotSeverity(x0, mu1, n, sigma, alpha), type = "l", ylim=c(0,1), col="red")
  }else{
    plot(mu1, spotSeverity(x0, mu1, n, sigma, alpha), type = "l", lwd=2, ylim=c(0,1), col = "black")
    abline(h=0)
    abline(h=1)
  }
}

#' @title  spotPlotTest
#' @description Visualize test result, errors, and severity
#' @param alternative One of greater, less, or two.sided. Full plots are currently
#' implemented for less, which is the default.
#' @param lower lower limit of the plot
#' @param upper upper limit of the plot 
#' @param mu0	mean of the null
#' @param mu1 mean of the alternative. See also parameter \code{beta}.
#' @param sigma standard deviation
#' @param n sample size
#' @param xbar observed mean
#' @param alpha	error of the first kind
#' @param beta error 2nd kind. Default \code{NULL}. If specified, then parameter 
#' \code{mu1} will be ignored and \code{mu1} will be calculated based on
#' \code{beta}.
#' 
#'
#' @importFrom graphics points
#' @importFrom graphics plot
#' @importFrom graphics polygon
#' @importFrom stats dnorm
#' @importFrom stats qnorm
#' @return 		description of return value
#' 
#' @examples 
#' spotPlotTest(lower=490, upper=510, mu0=500, mu1=504, sigma=2.7, n=9, xbar=502.22, alpha=0.025)
#' ## The following two plots should be nearly identical:
#' spotPlotTest(lower=490, upper=510, mu0=500, sigma=2.7, n=9, xbar=502.22, alpha=0.025, beta=0.2)
#' spotPlotTest(lower=490, upper=510, mu0=500, mu1=502.5215, sigma=2.7, n=9, xbar=502.22, alpha=0.025)
#' @export
spotPlotTest <- function(alternative = "greater",
                     lower = -3,
                     upper = 3,
                     mu0 = 0,
                     mu1 = 1,
                     sigma =1,
                     n=NULL,
                     xbar = 0,
                     alpha = 0.05,
                     beta=NULL){
  ## Standard error
  se<- sigma/sqrt(n)
  ## Calculate mu1 for a given beta:
  if(!is.null(beta)){
    mu1 <- mu0+qnorm(p=1-alpha)*se-qnorm(p=beta)*se
  }
  xaxis <- seq(lower, upper, length = 1e3)
  yaxis <- dnorm(xaxis, mean=mu0, sd=se)
  plot(xaxis, yaxis, type = "l")
  yaxisH1 <- dnorm(xaxis, mean=mu1, sd=se)
  points(xaxis, yaxisH1, type="l")
  
  lowerQuantPoint <- mu0+qnorm(alpha)*se
  upperQuantPoint<-mu0+qnorm(1-alpha)*se
  
  if (alternative == "less") {
    xaxis <- seq(lower, lowerQuantPoint, length = 1e3)
    yaxis <- c(dnorm(xaxis, mean=mu0, sd=sigma), 0, 0)
    xaxis <- c(xaxis, lowerQuantPoint, lower)
    polygon(xaxis, yaxis, density = 25)
  } else
  {
    if (alternative == "greater") {
      rejectH0 <- xbar>upperQuantPoint
      # power (1-beta):
      xaxis <- seq(upperQuantPoint, upper, length = 1e3)
      yaxis <- c(dnorm(xaxis, mean=mu1, sd=se), 0, 0)
      xaxis <- c(xaxis, upper, upperQuantPoint)
      polygon(xaxis, yaxis, density = 50, lty="solid", col="blue")
      # alpha:
      xaxis <- seq(upperQuantPoint, upper, length = 1e3)
      yaxis <- c(dnorm(xaxis, mean=mu0, sd=se), 0, 0)
      xaxis <- c(xaxis, upper, upperQuantPoint)
      polygon(xaxis, yaxis, density = 50, lty="solid", col="gray")
      # severity:
      if(!rejectH0){
        ## Severity rejection of H0:
        xaxis <- seq(xbar, upper, length = 1e3)
        yaxis <- c(dnorm(xaxis, mean=mu1, sd=se), 0, 0)
        xaxis <- c(xaxis, upper, xbar)
        polygon(xaxis, yaxis, density = 50, lty="solid", col="red")
      }
      else{
        ## Severity no rejection of H0:
        xaxis <- seq(lower, xbar, length = 1e3)
        yaxis <- c(dnorm(xaxis, mean=mu1, sd=se), 0, 0)
        xaxis <- c(xaxis, xbar, lower)
        polygon(xaxis, yaxis, density = 50, lty="solid", col="red")
      }
      abline(v=upperQuantPoint, lty=2)
    }
    else{
      lowerQuantPoint <- mu0 + qnorm(alpha / 2)
      upperQuantPoint <- mu0 + qnorm(1 - alpha / 2)
      xaxis <- seq(lower, lowerQuantPoint, length = 1e3)
      yaxis <- c(dnorm(xaxis, mean=mu0, sd=sigma), 0, 0)
      xaxis <- c(xaxis, lowerQuantPoint, lower)
      polygon(xaxis, yaxis, density = 25)
      xaxis <- seq(upperQuantPoint, upper, length = 1e3)
      yaxis <- c(dnorm(xaxis, mean=mu0, sd=sigma), 0, 0)
      xaxis <- c(xaxis, upper, upperQuantPoint)
      polygon(xaxis, yaxis, density = 25)
    }
  }
}














