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

#' @title spotSeverity
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
spotSeverity <- function(x0, 
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

#' @title  spotPlotSeverity
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
#' plot(mu1, spotSeverity(x0, mu1, n, sigma, alpha), type = "l", ylim=c(0,1), col="blue")
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
spotPlotSeverity <- function(y0, y1, add=FALSE, n=NA, alpha, rightLimit=1){
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
