#' @title checkArrival
#'
#' @description  Calculate arrival events for S-Ring.
#'
#' @param probNewCustomer probability of an arrival of a new customer
#' @importFrom stats runif
#' 
#' @return logical 
#'
#' @examples
#' checkArrival(0.5)
#' 
#' @export
#' 
checkArrival <- function(probNewCustomer){
  runif(1) < probNewCustomer
  }

#' @title init_ring 
#' 
#' @description Initialize ring parameters: generate arrival probabilities for S-Ring.
#'  - set beginning states to 0 and initialize random customer states and nElevators
#'  - nStates = (number of floors * 2) - 2. For example for 4 floors, 
#'               its 6 states because the upper and lower state have only 
#'               one direction and all other have 2 (UP and DOWN) 
#'
#' @param params list of 
#' \describe{
#'		\item{\code{randomSeed}}{random seed}
#'		\item{\code{nStates}}{number of S-Ring states}
#'		\item{\code{nElevators}}{number of elevators}
#'		\item{\code{probNewCustomer}}{probability pf a customer arrival}
#'		\item{\code{counter}}{Counter: number of waiting customers}
#'		\item{\code{sElevator}}{Vector representing elevators (s)}
#'	 	\item{\code{sCustomer}}{Vector representing customers (c)}
#'		\item{\code{currentState}}{Current state that is calculated}
#'		\item{\code{nextState}}{Next state that is calculated}
#'		\item{\code{nWeights}}{Number of weights for the perceptron (= 2 * nStates)}
#'     }
#'
#' @return list (params) of 
#' \describe{
#'		\item{\code{randomSeed}}{random seed}
#'		\item{\code{nStates}}{number of S-Ring states}
#'		\item{\code{nElevators}}{number of elevators}
#'		\item{\code{probNewCustomer}}{probability pf a customer arrival}
#'		\item{\code{counter}}{Counter: number of waiting customers}
#'		\item{\code{sElevator}}{Vector representing elevators (s)}
#'	 	\item{\code{sCustomer}}{Vector representing customers (c)}
#'		\item{\code{currentState}}{Current state that is calculated}
#'		\item{\code{nextState}}{Next state that is calculated}
#'		\item{\code{nWeights}}{Number of weights for the perceptron (= 2 * nStates)}
#'     } 
#'
#' @examples
#' 
#' params <-list(sElevator=NULL,
#'   sCustomer=NULL,
#'   currentState=NULL,
#'   nextState=NULL,
#'   counter=NULL,
#'   nStates=12,
#'   nElevators=2,
#'   probNewCustomer=0.1,
#'   weightsPerceptron=rep(0.1, 24),
#'   nWeights=NULL,
#'   nIterations=100,
#'   randomSeed=1234)
#'   
#' init_ring(params)
#'  
#' @export
#' 
init_ring <- function(params) {
  ## seed commented in ver 2.0.11
  # set.seed(params$randomSeed)
  nStates <- params$nStates
  nElevators <- params$nElevators
  probNewCustomer <- params$probNewCustomer
  params$counter <- params$nStates
  
  for (i in 1:nStates) {
    params$sElevator[i] <- 0
    params$sCustomer[i] <- 0
    if (checkArrival(probNewCustomer) == 1) {
      params$sCustomer[i] <- 1
      params$counter <- params$counter - 1
    }
  }
  
  for (i in 1:nElevators) {
    params$sElevator[i] <- 1
  }
  params$currentState <- 1
  params$nextState <- 2
  params$nWeights = 2 * nStates
  return(params)
}


#' perceptron
#'
#' Perceptron to calculate decisions
#' 
#'   Number of weights in NN controller is 2xnStates, 
#'   for each state (sElevator/sCustomer) there is one input
#'
#' @param currentState current state for decision (num)
#' @param nStates numer of states (int)
#' @param sElevator elevators vector (logical)
#' @param sCustomer customer vector (logical)
#' @param weightsPerceptron Weight vector (num)
#'
#' @return logical pass or take decision
#'
#' @export
#' 
perceptron <- function(currentState,
                       nStates,
                       sElevator,
                       sCustomer,
                       weightsPerceptron){
  x<- 0
  j <- currentState
  for(i in 1:nStates) {
    x <- x + (sElevator[j]*weightsPerceptron[i])
    j <- j%%nStates + 1
  }
  j <- currentState
  k <-(nStates+1)
  for(i in 1:nStates) {
    x<- x+(sCustomer[j]*weightsPerceptron[k])
    j<- j%%nStates + 1
    k <- k+1
  }
  if(x > 0) {
    return(1);
  } else {
    return(0);
  }
}


## 
#' ring
#'
#' main function which iterates the ring
#' 
#' @param params list of 
#' \describe{
#'		\item{\code{randomSeed}}{random seed}
#'		\item{\code{nStates}}{number of S-Ring states}
#'		\item{\code{nElevators}}{number of elevators}
#'		\item{\code{probNewCustomer}}{probability pf a customer arrival}
#'		\item{\code{counter}}{Counter: number of waiting customers}
#'		\item{\code{sElevator}}{Vector representing elevators (s)}
#'	 	\item{\code{sCustomer}}{Vector representing customers (c)}
#'		\item{\code{currentState}}{Current state that is calculated}
#'		\item{\code{nextState}}{Next state that is calculated}
#'		\item{\code{nWeights}}{Number of weights for the perceptron (= 2 * nStates)}
#'     }
#' @importFrom stats runif
#' @return number of waiting customers (estimation)
#'
#' @export
#' 
ring <- function(params) {
  fitness=0
  takeDecision=0
  h<-0
  
  #init S-ring
  params <- init_ring(params)
  
  sElevator=params$sElevator
  sCustomer= params$sCustomer
  currentState= params$currentState
  nextState= params$nextState
  counter= params$counter
  nStates=params$nStates
  nElevators=params$nElevators
  probNewCustomer=params$probNewCustomer
  weightsPerceptron=params$weightsPerceptron
  nWeights=params$nWeights
  nIterations=params$nIterations
  
  for(i in 1:nIterations) {
    newCustomer <- 0
    if(runif(1) < probNewCustomer) {
      newCustomer <- 1 }
    #Sring Steps
    if ((newCustomer == 1) & (sCustomer[currentState] == 0)) {
      sCustomer[currentState] <- 1 
      counter= counter - 1
    }
    if (sElevator[currentState]==1) {
      if (sCustomer[currentState]==1) {
        takeDecision <- perceptron(currentState,nStates,sElevator,sCustomer,weightsPerceptron)
        if ((takeDecision==1) | (sElevator[nextState]==1)) {
          sCustomer[currentState] <- 0
          counter=counter + 1
        } else {
          sElevator[nextState] <- sElevator[currentState] 
          sElevator[currentState] <- 0
        }
      } else if (sElevator[nextState] == 0) {
        sElevator[nextState] <- sElevator[currentState] 
        sElevator[currentState] <- 0
      }
    }
    nextState = currentState; 
    #print(paste("nextState",params$nextState))
    currentState = (((currentState-1) + (nStates - 1))%%nStates)+1;
    #Fitness
    fitness <- fitness + counter 
  }
  h <- (fitness/nIterations)
  return(nStates - h)
}


## 
#' sring
#'
#' simple elevator simulator
#' 
#' @param x perceptron weights
#' @param opt list of optional parameters, e.g., 
#' \describe{
#'		\item{\code{nElevators}}{number of elevators}
#'		\item{\code{probNewCustomer}}{probability pf a customer arrival}
#'		\item{\code{nIterations}}{Number of itertions}
#'		\item{\code{randomSeed}}{random seed}
#'   }
#' @param ... additional parameters   
#'   
#' @return fitness
#' @examples 
#' set.seed(123)
#' nStates = 6
#' nElevators = 2
#' sigma = 1
#' x = matrix( rnorm(n = 2*nStates, 1, sigma), 1,)
#' sring(x, opt = list(nElevators=nElevators,
#'                    nStates= nStates) )
#' 
#' @export
#' 
sring <- function(x,
                  opt = list(),
                  ...
                  ){
  options <-list(nStates= 6,
                 nElevators= 2,
                 probNewCustomer = 0.1,
                 nIterations= 1e3,
                 randomSeed = NULL, 
                 sElevator = NULL,
                 sCustomer = NULL,
                 currentState = NULL,
                 nextState = NULL,
                 counter = NULL,
                 weightsPerceptron = x,
                 nWeights = NULL)
  options[names(opt)] <- opt
  #assign optimization target
  #weightsPerceptron <- scale(x) 
  #normalize vector so SD=1, Mean=0 -> not active for perceptron
  #check if parameter are correct
  ns <- options$nStates
  if(length(x) < 2*ns) {
    stop("weight vector is too short")
  }
  ne <- options$nElevators
  if (ne  >= ns) {
    stop("nElevators must be smaller that nStates!")
  }
  p <- options$probNewCustomer
  if ( (p < 0.0) | (p > 1) ){
    stop("probNewCustomer must be positive and not larger that 1.0!")
  }
  fitness <- ring(options)
  return(fitness)
}


## 
#' funSring
#'
#' wrapper for \code{\link{sring}}
#' 
#' @param x perceptron weights
#' @param opt list of optional parameters, e.g., 
#' \describe{
#'		\item{\code{nElevators}}{number of elevators}
#'		\item{\code{probNewCustomer}}{probability pf a customer arrival}
#'		\item{\code{nIterations}}{Number of itertions}
#'		\item{\code{randomSeed}}{random seed}
#'   }
#' @param ... additional parameters   
#'     
#' @return fitness (matrix with one column)
#' 
#' @examples 
#' set.seed(123)
#' numberStates = 200
#' sigma = 1
#' x = matrix( rnorm(n = 2*numberStates, 1, sigma), 1,)
#' funSring(x)
#' 
#' @export
#' 
funSring <- function(x, opt = list(), ...){
  matrix(apply(x, # matrix
              1, # margin (apply over rows) 
              sring,
              opt),
        ,1) # number of columns
}


#' @title getCosts
#' 
#' @description 
#' Evaluate synthetic cost function that is based on the number of waiting customers and 
#' the number elevators 
#' 
#' @details 
#' Note: To accelerate testing, nIterations was set to 1e3 (instead of 1e6)
#' 
#' @param x vector with \code{sigma} weight multiplier and \code{ne} number of elevators 
#' @param ... optional parameters passed to \code{funSring}
#'     
#' @importFrom stats rnorm
#' 
#' @return fitness (costs)
#' 
#' @examples 
#' set.seed(123)
#' sigma = 1
#' ne = 10
#' x <- c(sigma, ne)
#' getCosts(x)
#'
#' @export
#'
getCosts <- function(x, ...){
  numberStates = 200
  sigma <- as.double(x[1])
  ne <- x[2]
  if (ne > numberStates/4){
    print("Too many elevators (ne).")
  } else {
    x = matrix( rnorm(n = 2*numberStates,
                      mean = 1,
                      sd = sigma),
                1,)
    # costs for handling the system with one elevator (change nIterations to 1e6)
    opt <- list(nElevators = 1,  nStates = numberStates, nIterations = 1e3)
    cost1 <- funSring(x = x, opt)
    # costs for handling the sytem with ne Elevators (change nIterations to 1e6)
    opt <- list(nElevators = ne,  nStates = numberStates, nIterations = 1e3)
    costn <- funSring(x = x, opt)
    ( 1e7 - costn * costn * log(ne) + cost1)
  }}


## 
#' funCosts
#'
#' optimWrapper for getCosts
#' 
#' Evaluate synthetic cost function that is based on the number of waiting customers and 
#' the number elevators 
#' 
#' @param x vector: weight multiplier \code{sigma} and number of elevators \code{ne} 
#'     
#' @return fitness (costs) as matrix
#'
#' @examples 
#' sigma = 1
#' ne = 10 
#' x <- matrix(c(sigma, ne), 1,)
#' funCosts(x)
#'
#' @export
#'
funCosts <- function(x)
{matrix(apply(x, # matrix
              1, # margin (apply over rows) 
              getCosts),
,1) # number of columns
}

# set.seed(123);  sigma = 1; ne = 10 ; funCosts(matrix(c(sigma, ne),1,))
