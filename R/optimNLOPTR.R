#' @title optimNLOPTR. Minimization by NLOPT
#' 
#' @description #' This is a wrapper that employs the \code{nloptr} function from the package of the same name.
#' The \code{nloptr} function itself is an interface to the \code{nlopt} library, which contains a wide selection
#' of different optimization algorithms.
#'
#' @param x optional matrix of points to be included in the evaluation (only first row will be used)
#' @param fun objective function, which receives a matrix x and returns observations y
#' @param lower boundary of the search space
#' @param upper boundary of the search space
#' @param control named list, with the options for \code{nloptr}. These
#' will be passed to \code{nloptr} as arguments. In addition, the following
#' parameter can be used to set the function evaluation budget:
#' \describe{
#'   \item{\code{funEvals}}{Budget, number of function evaluations allowed. Default: 100.}
#' }
#' @param ... passed to \code{fun}
#'
#' Note that the arguments 
#' \code{x}, \code{fun}, \code{lower} and \code{upper} 
#' will be mapped to the corresponding arguments of \code{nloptr}: 
#' \code{x0}, \code{eval_f}, \code{lb} and \code{ub}.
#'
#' @importFrom nloptr nloptr
#' @importFrom stats runif
#' @importFrom utils str
#'
#' @return list, with elements
#' \describe{
#'   \item{\code{x}}{archive of evaluated solutions}
#'   \item{\code{y}}{archive of observations}
#'   \item{\code{xbest}}{best solution}
#'   \item{\code{ybest}}{best observation}
#'   \item{\code{count}}{number of evaluations of \code{fun}}
#'   \item{\code{message}}{success message}
#' }
#'
#' @examples
#'\donttest{
#' ##simple example:
#' res <- optimNLOPTR(,fun = funSphere,lower = c(-10,-20),upper=c(20,8))
#' res
#' ##with an inequality constraint:
#' contr <- list()  #control list
#' ##specify constraint
#' contr$eval_g_ineq <- function(x) 1+x[1]-x[2]
#' res <- optimNLOPTR(,fun=funSphere,lower=c(-10,-20),upper=c(20,8),control=contr) 
#' res
#'}
#' @export
#' 
#' 
optimNLOPTR <- function(x=NULL,fun,lower,upper,control=list(),...){
 
  #if (length(par)==0) stop("dimension of par is null")
  con<-list(funEvals=100, verbosity = 0)
  con[names(control)] <- control
  control<-con

  #"types" not used here.
  control$types <- NULL
  
  #fixing some required arguments
  if(!is.null(control$funEvals)){
    control$opts$maxeval <- control$funEvals
    control$funEvals <- NULL
  }
  if(is.null(control$opts$algorithm)){
    control$opts$algorithm <- "NLOPT_GN_ORIG_DIRECT_L" #"NLOPT_GN_DIRECT_L"#"NLOPT_GN_ORIG_DIRECT_L"
  }
  #main arguments	
  control$x0 <- x	
  if(is.null(control$x0))	
    control$x0 <- runif(length(lower),lower,upper) #fix start guess	if none provided	
  else
    control$x0 <- control$x0[1,]
  control$lb <- lower
  control$ub <- upper
  # control$eval_f <- function(xx)fun(matrix(xx,1),
  #                                  ...)
  ### NEW in SPOT 2.2.10/12
  f <- function(xx)fun(matrix(xx,1),
                                    ...)
  control$eval_f <- function(xx) {
    xx <- matrix(xx, 1)
    ynew <- as.vector(f(xx))
    ylog <<- rbind(ylog, ynew)
    xlog <<- rbind(xlog, xx)
    return(ynew)
  }
###   END NEW in SPOT 2.2.10/12
  if (control$verbosity > 0){
    print("optimNLOPTR starting point:")
    str(control$x0)
  }
  

  ## initialize logged variables
  xlog <- NULL
	ylog <- NULL

  optimizationResult <- nloptr(x0 = control$x0,
                               eval_f = control$eval_f,
                               eval_grad_f = NULL,
                               lb =  control$lb,
                               ub = control$ub,
                               eval_g_ineq = control$eval_g_ineq,
                               eval_jac_g_ineq = NULL, 
                               eval_g_eq = NULL,
                               eval_jac_g_eq = NULL,
                               opts = list("algorithm" = control$opts$algorithm,
                                           "maxeval" = control$opts$maxeval)
                               )
                               
  ybest <- optimizationResult$objective
  xbest <- optimizationResult$solution	
  count <- optimizationResult$iterations
  msg=optimizationResult$message
  rownames(ylog) <- NULL
  
  if (control$verbosity > 0){
    print("optimNLOPTR finished:")
    str(list(x=xlog,y=ylog,xbest=matrix(xbest,1),ybest=ybest,count=count,msg=msg)
    )
  }
  ### list(x=NA,y=NA,xbest=matrix(xbest,1),ybest=ybest,count=count,msg=msg)
  list(x=xlog,y=ylog,xbest=matrix(xbest,1),ybest=ybest,count=count,msg=msg)
}


optimNLOPTR_OLD<-function(x=NULL,fun,lower,upper,control=list(),...){
	#if (length(par)==0) stop("dimension of par is null")
	con<-list(funEvals=100)
	con[names(control)] <- control
	control<-con
	
	#"types" not used here.
	control$types <- NULL
	
	#fixing some required arguments
	if(!is.null(control$funEvals)){
		control$opts$maxeval <- control$funEvals
		control$funEvals <- NULL
	}
	if(is.null(control$opts$algorithm)){
		control$opts$algorithm <- "NLOPT_GN_ORIG_DIRECT_L" #"NLOPT_GN_DIRECT_L"#"NLOPT_GN_ORIG_DIRECT_L"
	}
	#main arguments	
	control$x0 <- x	
	if(is.null(control$x0))	
		control$x0 <- runif(length(lower),lower,upper) #fix start guess	if none provided	
	else
		control$x0 <- control$x0[1,]
	control$lb <- lower
	control$ub <- upper
	control$eval_f <- function(xx)fun(matrix(xx,1))
	
	optimizationResult <- do.call(nloptr,control)
	ybest <- optimizationResult$objective
	xbest <- optimizationResult$solution	
	count <- optimizationResult$iterations
	msg=optimizationResult$message
  list(x=NA,y=NA,xbest=matrix(xbest,1),ybest=ybest,count=count,msg=msg)
}
