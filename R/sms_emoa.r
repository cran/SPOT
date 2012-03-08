###############################################################################
#' SMS-EMOA: S-Metric-Selection Evolutionary Multiobjective Optimization Algorithm
#' 
#' Straight forward SMS-EMOA implementation. This function is used to optimize several surrogate models
#' when doing multi objective optimization with SPOT. See: \code{\link{spotParetoOptMulti}}.
#'
#' @param f target function to be optimized of  type f(x)=y where both x and y are vectors.
#' 			The target function should return a vector of length(y) containing NA's if the input vector x contains NA values.
#' @param lower the lower boundary vector of the decision space
#' @param upper the upper boundary vector of the decision space
#' @param ... further settings relayed to \code{f}
#' @param control list of parameters (defaults are: mu=100L, sbx.n=15, sbx.p=0.7, pm.n=25, pm.p=0.3)
#' @return list with archive of solutions, active pareto front and others
#' @author O. Mersmann
#'
#' @seealso N. Beume, B. Naujoks, and M.Emmerich. \emph{SMS-EMOA: Multiobjective selection based on dominated hypervolume}.
#' European Journal of Operational Research, 181(3):1653--1669, 2007. \cr \cr
#' See the following link for up to date version of this implementation: \url{https://git.p-value.net/emoa.git/plain/examples/sms_emoa.r} 
#' 
#' @export
###############################################################################
spotSmsEmoa <- function(f, lower, upper, ...,
                     control=list(mu=100L,
                       sbx.n=15, sbx.p=0.7,
                       pm.n=25, pm.p=0.3
                       )) {
  ## Extract control parameters:
  default <- formals(sys.function())$control
  control <- steady_state_emoa_control(f, lower, upper, ..., control=control, default=default)
  control <- sbx_control(f, upper, lower, ..., control=control, default=default)
  control <- pm_control(f, upper, lower, ..., control=control, default=default)  
  control$ref <- emoa:::coalesce(control[["ref"]], rep(11, control$d))

  ## Tracking variables:
  X <- matrix(0, nrow=control$n, ncol=control$maxeval)
  Y <- matrix(0, nrow=control$d, ncol=control$maxeval)
  dob <- rep(-1L, control$maxeval)
  eol <- rep(-1L, control$maxeval)
  
  ## Random inital population:
  X[, 1:control$mu] <- replicate(control$mu, runif(control$n, lower, upper))
  Y[, 1:control$mu] <- sapply(1:control$mu, function(i) f(X[,i]))

  neval <- control$mu       ## Count the number of function evaluations
  active <- 1:control$mu    ## Indices of individuals that are in the current pop.

  ## Save some common control parameters into the current
  ## environment. This saves a few msec of execution time...
  crossover <- control$crossover
  mutate <- control$mutate
  maxeval <- control$maxeval
  #logger <- control$logger
  
  #logger$start("sms_emoa")
  while(neval < maxeval) {
    ############################################################
    ## Variation:
    parents <- sample(active, 2)
    child <- crossover(X[, parents])[,sample(c(1, 2), 1)]
    x <- mutate(child)

    ## Add new individual:
    neval <- neval + 1
    X[, neval] <- x
    Y[, neval] <- f(x)
    dob[neval] <- neval ## For a steady state emoa this is trivial...
    active <- c(active, neval)

    ############################################################
    ## Selection:
    i <- nds_hv_selection(Y[, active])

    ## Remove the i-th active individual:
    eol[active[i]] <- neval
    active <- active[-i]

    ############################################################
    ## Logging:    
    #logger$step()
  }
  #logger$stop()
  
  res <- structure(list(X=X, Y=Y,
                        dob=dob,
                        eol=eol,
                        par=X[,active], value=Y[,active]),
                   class="emoa_result")
  return(res)
}

  
  # while(neval < maxeval) {
    # parents <- rbind(sample(active, nchild),sample(active, nchild))
	# fun<-function(x){crossover(X[,x])[,sample(c(1, 2), 1)]}
	# child <- apply(parents,2,fun) 
	# x <- apply(child,2,mutate)
	# nold <- neval
	# neval <- neval + nchild
    # X[, (nold+1):neval] <- x
    # Y[, (nold+1):neval] <- f(x)
  # }

# binh11<-function(x){browser()
# return(binh1(x))}
###Test:
# require(mco)
 # upper=c(10,10)
 # lower=c(-10,-10)
# res<-sms_emoa(binh11,lower,upper,control=list(mu=5))
# plot(t(res$value))
# plot(t(res$par))