### Bitte in Spot implementieren:
### spotPower(), spotPlotPower(), spotSeverity(), spotPlotSeverity()
###

### a clean start:
#rm(list=ls());
###
### setwd("/Users/thomasbartz-beielstein/Svn.d/SvnSpot.d/trunk/publications/YossiBook/Sann.d")
###
#library(SPOT)

## define helper functions for power and plotting power:

#TODO: These functions desperately need a description...

###################################################################################################
#' spotPower
#' 
#' short description
#'
#' details 
#'
#' @param alpha		description of alpha
#' @param mu0		description of mu0
#' @param mu1		description of mu1
#' @param n			vector length
#' @param sigma		standart deviation
#'
#' @return 		description of return value
#'
#' @seealso  \code{\link{spotPlotPower}} 
#' \code{\link{spotCompare}}
#' @export
#' @keywords internal
###################################################################################################
spotPower <- function(alpha, mu0, mu1, n, sigma){
	1 - pnorm( qnorm(1-alpha,0,1) + (mu0-mu1)*sqrt(n)/sigma);
}

###################################################################################################
#' spotPlotPower
#' 
#' short description
#'
#' details 
#'
#' @param y0			First input vector
#' @param y1			Second input vector
#' @param alpha			description of alpha, default value is \code{0.05}
#' @param add			Boolean, default value is \code{FALSE}
#' @param n				number of vector elements that should be evaluated, default value is \code{NA}, which means the whole vector
#' @param rightLimit	description of rightLimit, default value is \code{1} 
#'
#' @return 		description of return value
#'
#' @seealso  \code{\link{spotPower}} 
#' \code{\link{spotCompare}}
#' @export
#' @keywords internal
###################################################################################################
spotPlotPower <- function(y0, y1, alpha=0.05, add=FALSE, n=NA, rightLimit=1){
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

###################################################################################################
#' spotSeverity
#' 
#' short description
#'
#' details 
#'
#' @param x0			sample mean value
#' @param mu1			description
#' @param n				description
#' @param sigma			description
#' @param alpha			description	
#'
#' @return 		description of return value
#'
#' @seealso  \code{\link{spotPlotSeverity}}  \code{\link{spotCompare}}
#' @export
#' @keywords internal
###################################################################################################
spotSeverity <- function(x0, mu1, n, sigma, alpha){
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

## seq(0, 1, length.out=10)

###################################################################################################
#' spotPlotSeverity
#' 
#' short description
#'
#' details 
#'
#' @param y0			first input vector
#' @param y1			second input vector
#' @param add			default value is FALSE
#' @param n				default value is NA, which means length of y0 will be used for n
#' @param alpha			description
#' @param rightLimit	description of rightLimit, default value is \code{1} 
#'
#' @return 		description of return value
#'
#' @seealso \code{\link{spotSeverity}}  \code{\link{spotCompare}}
#' @export
#' @keywords internal
###################################################################################################
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






#### First we run SPOT in auto mode to tune SANN:
#spot("rfSann02.conf")
#spot("rfSann02.conf","rep")
#### Then we use these tuned parameter to run SANN 100 times:
#spot("rfSann03.conf", "init")
#spot("rfSann03.conf", "run")
#### Next we run SANN with default parameters 100 times:
#spot("rfSann04.conf", "init")
#spot("rfSann04.conf", "run")
####
#### Prepare data frames and select y values for tuned (rfSann03) and default (rfSann04) SANN settings:
#rf03.df <- read.table("rfSann03.res", header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)
#rf04.df <- read.table("rfSann04.res", header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)
#y3 <- rf03.df$Y
#y4 <- rf04.df$Y
#### A first summary:
#summary(y3)
#summary(y4)
#sd(y3)
#sd(y4)

#########
#### Generate the first plot which displays power and severity for rfSann03 and rfSann04:
#### This gives the figure "severityPowery3y4.pdf":
#alpha <- 0.05
## limit for plots, i.e., significant difference:
#rightLimit <- 1
#### spotPlotPower(y3,y4,alpha, rightLimit=rightLimit)
#### spotPlotPower(y3,y4,alpha,add=TRUE,n=10, rightLimit=rightLimit)
#### spotPlotPower(y3,y4,alpha,add=TRUE,n=20, rightLimit=rightLimit)

#spotPlotSeverity(y3, y4, add=FALSE, n=100, alpha=alpha, rightLimit=rightLimit)
#### spotPlotSeverity(y3, y4, add=TRUE, n=10, alpha=alpha, rightLimit=rightLimit)
#### spotPlotSeverity(y3, y4, add=TRUE, n=20, alpha=alpha, rightLimit=rightLimit)
#### spotPlotSeverity(y3, y4, add=TRUE, n=30, alpha=alpha, rightLimit=rightLimit)
#spotPlotPower(y3,y4,add=TRUE,alpha=alpha, rightLimit=rightLimit)
#### spotPlotPower(y3,y4,add=TRUE, n=10, alpha=alpha, rightLimit=rightLimit)
#abline(h=0.95)

#par(mfrow=c(2,1))
#dens3 <- density(y3)
#print(dens3)
#dens4 <- density(y4)
#print(dens4)
#xlim3 <- range(dens3$x)
#xlim4 <- range(dens4$x)
#ylim3 <- range(dens3$y)
#ylim4 <- range(dens4$y)
#### hist(y5, col="darkgray", xlim = xlim6, ylim = ylim5, breaks= 0.39+(0:10)*0.001, probability = TRUE)
#hist(y3, col="darkgray", xlim = xlim3, ylim = ylim3, probability = TRUE)
#lines(dens3)
#ylim4 <- range(dens4$y)
#hist(y4, col="darkgray", xlim = xlim4, ylim = ylim4, probability = TRUE)#
#lines(dens4)

##########
##########
#### Second experiment with 100 samples:
#### now we increase the number of SANN function evaluations (maxit=1000000).
#### This is used to illustrate a ceiling effect (the problem is too easy, because of the large number of function evaluations:
#### here we run SANN with tuned parameters 100 times:
#spot("rfSann05.conf", "init")
## Warning: the follwing run take several minutes:
#spot("rfSann05.conf", "run")

## now we run SANN with default parameters 100 times:
#spot("rfSann06.conf", "init")
## Warning: the follwing run take several minutes:
#spot("rfSann06.conf", "run")

####
#### Read data from the ceiling effect settings:
#rf05.df <- read.table("rfSann05.res", header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)
#rf06.df <- read.table("rfSann06.res", header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)
#y5 <- rf05.df$Y
#y6 <- rf06.df$Y
#summary(y5)
#summary(y6)

#### Generate a power-severity plot for the ceiling effect setup:
#par(mfrow=c(1,1))
#rightLimit <- 0.000005
#spotPlotSeverity(y5, y6, add=FALSE, n=100, alpha=alpha, rightLimit=rightLimit)
#### spotPlotSeverity(y5, y6, add=TRUE, n=10, alpha=alpha, rightLimit=rightLimit)
#### spotPlotSeverity(y5, y6, add=TRUE, n=20, alpha=alpha, rightLimit=rightLimit)
#### spotPlotSeverity(y5, y6, add=TRUE, n=30, alpha=alpha, rightLimit=rightLimit)
#spotPlotPower(y5,y6,add=TRUE,alpha=alpha, rightLimit=rightLimit)
#### spotPlotPower(y5,y6,add=TRUE, n=10, alpha=alpha, rightLimit=rightLimit)
#abline(h=0.95)
#abline(h=0.05)

#### In addition, generate a t-test:
#t.test(y5, y6, alternative='less', conf.level=.95, paired=TRUE)
#### t.test(y5[1:10], y6[1:10], alternative='less', conf.level=.95, paired=TRUE)
#### t.test(y5[1:20], y6[1:20], alternative='less', conf.level=.95, paired=TRUE)
#### t.test(y5[1:30], y6[1:30], alternative='less', conf.level=.95, paired=TRUE)
#### t.test(y5[1:50], y6[1:50], alternative='less', conf.level=.95, paired=TRUE)
#### t.test(y5[1:100], y6[1:100], alternative='less', conf.level=.95, paired=TRUE)

## A histogram illustrates the importance of EDA tools:

## par(mfrow=c(3,1))
## dens5 <- density(y5)
## xlim5 <- range(dens5$x)
## ylim5 <- range(dens5$y)
## hist(y5, col="darkgray", xlim = xlim5, ylim = ylim5, probability = TRUE)
## lines(dens5)

#par(mfrow=c(2,1))
# dens5 <- density(y5)
#print(dens5)
#dens6 <- density(y6)
#print(dens6)
#xlim5 <- range(dens5$x)
#xlim6 <- range(dens6$x)
# ylim5 <- range(dens5$y)
#ylim6 <- range(dens6$y)
#### hist(y5, col="darkgray", xlim = xlim6, ylim = ylim5, breaks= 0.39+(0:10)*0.001, probability = TRUE)
#hist(y5, col="darkgray", xlim = xlim6, ylim = ylim5, probability = TRUE)
#lines(dens5)
#ylim6 <- range(dens6$y)
#hist(y6, col="darkgray", xlim = xlim6, ylim = ylim5, probability = TRUE)
#lines(dens6)

#########
#### alternatively we plot two simple histograms with frequencies

#par(mfrow=c(2,1))
#hist(y5, col="darkgray", breaks= 0.3975+(0:10)*0.0001, probability = FALSE)
#hist(y6, col="darkgray",  breaks= 0.3975+(0:10)*0.0001, probability = FALSE)


####### Mayo06a example (fig 2):
#x0 <- 12.1
#mu1 <- seq(11.9,13,0.01)
#n <- 100
#sigma <- 2
#alpha <- 0.025
#plot(mu1, spotSeverity(x0, mu1, n, sigma, alpha), type = "l", ylim=c(0,1), col="blue")
#      abline(h=0)
#      abline(h=1)
#      abline(h=0.95)
#abline(v=12.43)
#### plot power:
#mu0 <- 12
# points(mu1, spotPower(alpha, mu0, mu1, n, sigma), type = "l", ylim=c(0,1), col="green")
#abline(v=12.72)





###################################################################################
#' Spot Feature Comparison
#'
#' Simple Function to compare performance of different spot configurations.
#' Example: perf=spotCompare(c("test.conf","test2.conf","test3.conf"),10)
#'
#' @param configFile vector of filenames to be evaulated
#' @param repeats number of repeats for each file (with different spot.seed)
#' @param NoNoiseFun function to compute the result without noise. If NULL or left to default this will
#'						have no effect. If a function name is passed as a string, the according function will be called with a noice variable
#'						set to zero. This way the result can be evaluated without noise
#' @return performance, a matrix of y-Values for each single run. One Column for each file.
#' @export
#' @keywords internal
####################################################################################
spotCompare <- function(configFile,repeats=2,NoNoiseFun=NULL){
	performance<-matrix(data=0,nrow=repeats,ncol=length(configFile)); #pre-allocate performance matrix
	for(ii in 1:length(configFile)){ #loop for each feature test (e.g. one for each config file passed to the function)
		for(k in 1:repeats){ # loop for each feature repeat
			result=spot(configFile[ii],spotConfig=list(spot.seed=k));
			if (is.null(NoNoiseFun)){
				performance[k,ii]=as.numeric(tail(result$alg.currentBest[1],1));#TODO tail can be very slow...				
			}
			else{
				pNames <- row.names(result$alg.roi);
				xx <- as.numeric(tail(result$alg.currentBest[pNames],1));		#TODO tail can be very slow...		
				eval(call(NoNoiseFun,xx,noise=0));
			}
		}		
	}		
	#maybe include evaluation of performance here
	performance;	#return performance 
}
# Example: perf=spotCompare(c("test.conf","test2.conf","test3.conf"),10)  test:RFdefault  test2:Tree  test3:RFoptim
###########################################################
# > perf
             # [,1]        [,2]        [,3]
 # [1,]  1.59724451  1.73726650  2.28536893
 # [2,]  0.64494935 -0.26348443 -0.83248201
 # [3,] -0.74904009 -0.03592597  0.52838493
 # [4,] -0.88334303  0.57483523 -0.67458122
 # [5,]  0.34216395 -0.09189489 -0.23334717
 # [6,]  0.06730034  0.06730034  0.06730034
 # [7,] -0.91823225 -0.51965195 -0.10203528
 # [8,] -0.93141974  0.47899049 -0.16211020
 # [9,] -0.86128966  0.20695078 -0.19727281
# [10,] -0.92568746 -0.70606856 -0.67542038
# > mean(perf[,1])
# [1] -0.2617354
# > mean(perf[,2])
# [1] 0.1448318
# > mean(perf[,3])
# [1] 0.0003805137
####################Config:###################
# alg.func = "spotFuncStartSixHump"
# alg.seed = 1235
# auto.loop.steps = Inf;
# auto.loop.nevals = 200;
# init.design.size = 100;
# io.columnSep = " ";
# seq.design.maxRepeats = 1;
# seq.design.size = 100
# seq.predictionModel.func = "(..tree or RFdefault or RFoptim...)"
# spot.fileMode=FALSE
# spot.seed = 125