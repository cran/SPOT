context("test sring")

test_that("Ensure that results from the old sring version and the current version do not differ.", {
	skip_on_cran() 
    
    ## Old implementation:
    prob <- function(probNewCustomer) {
        if(runif(1) < probNewCustomer) {
            return(1);
        }
        return(0);
    }
    
    ## initialize ring parameters
    
    init_ring <- function(params) {
        # set.seed(params$randomSeed)
        nStates <- params$nStates;
        nElevators <- params$nElevators;
        probNewCustomer <- params$probNewCustomer;
        params$counter <- params$nStates;
        
        # set beginning states to 0 and initialize random customer states and nElevators
        # nStates = (number of floors * 2) - 2. For example for 4 floors, 
        # its 6 states because the upper and lower state have only one direction and all other have 2
        # UP and DOWN 
        for(i in 1:nStates) {
            params$sElevator[i] <- 0
            params$sCustomer[i] <- 0
            if(prob(probNewCustomer) == 1) {
                params$sCustomer[i] <- 1
                params$counter <- params$counter - 1
            }
        }
        
        for (i in 1:nElevators) {
            params$sElevator[i] <- 1
        }
        params$currentState <- 1
        params$nextState <- 2
        params$nWeights = 2*nStates
        return(params)
    }
    
    ## Number of weights in NN controller is 2xnStates, for each state (sElevator/sCustomer) there is one input
    
    perceptron <- function(currentState,nStates,sElevator,sCustomer,weightsPerceptron){
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
    
    ## main function which iterates the rings
    
    ring <- function(params) {
        fitness=0
        takeDecision=0
        h<-0
        
        #init S-ring
        params<- init_ring(params)
        
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
            #print(paste("iteration",i))
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
    
    ## optimWrapper
    
    sRingProb <- function(x,
                          nStates= 6,
                          nElevators= 2,
                          probNewCustomer= 0.1,
                          nIterations= 1e3,
                          randomSeed= NULL #17
    ) {
        #assign optimization target
        #weightsPerceptron <- scale(x) #normalize vector so SD=1, Mean=0 -> not active for perceptron
        weightsPerceptron <- x
        if(length(x) < nStates*2) {
            stop("weight vector is too short")
        }
        #function starts here
        #Initialization of global variables
        #init parameters with values
        
        #checks if parameter are correct
        if (nElevators>=nStates) nElevator=nStates-1;
        if (probNewCustomer<0.0) probNewCustomer=0.0;
        if (probNewCustomer>1.0) probNewCustomer=1.0;
        #globale parameter
        params <-list(sElevator=NULL,
                      sCustomer=NULL,
                      currentState=NULL,
                      nextState=NULL,
                      counter=NULL,
                      nStates=nStates,
                      nElevators=nElevators,
                      probNewCustomer=probNewCustomer,
                      weightsPerceptron=weightsPerceptron,
                      nWeights=NULL,
                      nIterations=nIterations,
                      randomSeed=randomSeed)
        
        fitness <- ring(params)
        return(fitness)
    }
    
    ## Werte eine Einstellung aus
    berechneWartezeit <- function(sigma, ne){
        numberStates = 200   
        if (ne > numberStates/4){
            print("Zu viele Aufzuege")
        } else {
            x = rnorm(n = 2*numberStates,
                      mean = 1,
                      sd = sigma)
            # print(x)
            cost1 <- sRingProb(x = x, nElevators = 1, nStates = numberStates)
            costn <- sRingProb(x = x, nElevators = ne, nStates = numberStates)
            (1e7 - costn * costn * log(ne) + cost1)
        }}
    
    ## call old version:
    set.seed(123)
    sigma = 1
    ne = 10
    yold <- berechneWartezeit(sigma, ne)
    
    ## call new version:
    set.seed(123)
    sigma = 1
    ne = 10
    x <- c(sigma, ne)
    ynew <- getCosts(x)
    expect_true(all(ynew == yold))
})





