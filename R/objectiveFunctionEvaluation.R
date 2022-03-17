#' @title objectiveFunctionEvaluation  Objective Function Evaluation
#' 
#' @description  This function handles the evaluation of the objective function in \code{\link{spot}}.
#' This includes handling of the random number generator stream as well as the actual evaluation.
#'
#' @param x matrix of already known solutions, to determine whether RNG seeds for new solutions need to be incremented.
#' @param xnew matrix of new solutions.
#' @param fun objective function to evaluate the solutions in \code{xnew}.
#' @param control control list, e.g., \code{seedFun:} 
#' initial seed to be used for the random number generator seed. 
#' Set to NA to avoid using a fixed seed. \code{noise:} \code{logical} parameter specifying 
#' whether the target function is noisy. \code{verbosity:} verbosity. Default: 0. \code{transformFun:} 
#' transformation functions applied to \code{xnew}.
#' @param ... parameters passed to \code{fun}.
#' 
#' @return the matrix ynew, which are the observations for fun(xnew)
#'
#' @seealso \code{\link{spot}} for more details on the parameters, e.g., \code{fun}
#'
#' @export
#' @keywords internal
objectiveFunctionEvaluation <- function(x=NULL,
                                        xnew,
                                        fun,
                                        control=list(),
                                        ...){
  seedFun <- control$seedFun
  noise <- control$noise
  verbosity <- control$verbosity
  transformFun <- control$transformFun
  
  if(verbosity>0){
    message("Entering objectiveFunctionEvaluation()")
  }
 	if(!is.null(x)){	
	  x <- data.matrix(x) 
	}
  xnew <- data.matrix(xnew) #TODO: same as in ocba. else, problems with identical() due to names
  ## if xnew is empty, return NULL
	if(nrow(xnew)==0){
	  ## return(numeric(0))
	  ## Changed in 2.2.0 to:
	  message("Warning: return(NULL) from objectiveFunctionEvaluation() because xnew is empty!")
		return(NULL)
	}

	## save seed status (to avoid disturbing the main loops RNG stream)
  ## note: theoretically only needed in case of noise==TRUE, but always done to avoid mistakes.
	if(exists(as.character(substitute(.Random.seed))))
    SAVESEED<-.Random.seed
  else
    SAVESEED=NULL

    if(verbosity>0){
    message("objectiveFunctionEvaluation(): xnew before transformX().")
    print(xnew)
  }
  
  if (length(transformFun) > 0) {
    xnew <- transformX(xNat=xnew, fn=transformFun)
  }
  
  if(verbosity>0){
    message("objectiveFunctionEvaluation(): xnew after transformX().")
    print(xnew)
  }
  
  ## Noisy function without seed handling: so, we have to implement a seed handling 
  ##
	if(noise & !is.na(seedFun)){	      
    ## calculate seeds for each evaluation
		seed <- numeric(nrow(xnew))
		seed <- updateSeedFromXnew(x, xnew, seed, seedFun, verbosity)
		
		## either pass seed to objective function fun (if there is a parameter to receive it)
		nms <- names(formals(fun))		
		passSeed <- FALSE
		if(length(nms)>1){
			passSeed <- names(formals(fun)[2])=="seed"
		}			
		if(passSeed){
			ynew <- fun(xnew,seed,...) 
			if(verbosity>0){
			  message("objectiveFunctionEvaluation(): Successfully executed fun with passes seed.")
			  print(ynew)
			}
			ynew
		}else{ ## or set seed directly here
		  tryCatch(
		    expr = {
		      ynew <- NULL
		      for(i in 1:nrow(xnew)){
		        set.seed(seed[i])
		        ynew <- rbind(ynew,fun(xnew[i,,drop=FALSE],...)) 
		      }
		      if(verbosity>0){
		      message("objectiveFunctionEvaluation(): Successfully executed fun with individual seed.")
		        print(ynew)
		      }
		      ynew
		    },
		    error = function(e){
		      message('objectiveFunctionEvaluation(): Caught an error!')
		      print(e)
		    }
		  )    
		}
		}else{
		  if(verbosity>0){
		  message("objectiveFunctionEvaluation(): calling fun() without extra seed handling. fun and xnew:")
		    print(fun)
		    print(xnew)
		  }
		ynew <- fun(xnew,...)
		if(verbosity>0){
		  message("objectiveFunctionEvaluation(): Successfully executed fun without extra seed handling.")
		  print(ynew)
		}
		ynew
	}
  if(verbosity>0){
    message("objectiveFunctionEvaluation(): ynew after calling fun().")
    print(ynew)
  }
  
  ## load seed status (to avoid disturbing the main loops RNG stream), see save at start of function.
  if(!is.null(SAVESEED))
    assign(".Random.seed", SAVESEED, envir=globalenv())
	
  ## Changed: 2022-03-12:
  ## ynew not a matrix and xnew>1 (several values are evaluated) 
  ## => return a  matrix with xnew rows
  ## The following code is replaced by the code directly below:
	# if(!is.matrix(ynew) & nrow(xnew)>1)
	# 	ynew <- matrix(data = ynew,
	# 	               ncol = 1) #convert to column matrix
	# if(!is.matrix(ynew) & nrow(xnew)==1)
	# 	ynew <- matrix(data = ynew,
	# 	               nrow = 1) #convert to row matrix
  if(!is.matrix(ynew)){
    ynew <- matrix(data = ynew,
                   nrow = nrow(xnew),
                   byrow = TRUE)
    } #convert to row matrix
	ynew
}

#' @title update seed
#' @param x matrix of already known solutions, to determine whether RNG seeds for new solutions need to be incremented.
#' @param xnew matrix of new solutions.
#' @param seed seed
#' @param seedFun initial seed to be used for the random number generator seed. Set to NA to avoid using a fixed seed.
#' @param verbosity verbosity. Default: 0
#' @return the updated seed
#' @export
#' @keywords internal
updateSeedFromXnew <- function(x, xnew, seed, seedFun, verbosity=0){
tryCatch(
  expr = {
    for(i in 1:nrow(xnew)){
      xnewi <- xnew[i,]
      x <- rbind(x, xnewi)
      repetitions <- sum(apply(x, 1, identical, xnewi)) -1
      seed[i] <- seedFun + repetitions
    }
   return(seed) 
  },
  error = function(e){ 
    message('updateSeedFromXnew: Caught an error!')
    print(e)
  },
  warning = function(w){
    message('updateSeedFromXnew: Caught an warning!')
    print(w)
  },
  finally = {
    if(verbosity>0){
    message('updateSeedFromXnew: All done, quitting.')
    }
  }
)
}


