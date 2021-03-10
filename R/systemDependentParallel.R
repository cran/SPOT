#' @title Parallel execution of code, dependent on the operating system
#' 
#' @description mclapply is only supported on linux and macOS. On Windows parlapply should be used.
#' This function switches between both dependent on the operating system of the user.
#' 
#' @param X vector with arguments to parallelize over 
#' @param FUN function that shall be applied to each element of X
#' @param nCores \code{integer}. Defines the number of cores.
#' @param ... optional arguments to FUN
#' 
#' @import parallel
#' 
#' @export
doParallel <- function(X, FUN, nCores = 2, ...){
    # check if os is windows and use parLapply
    if(.Platform$OS.type == "windows") {
        cl <- makeCluster(nCores)
        res <- parLapply(cl, X, FUN, ...)
        stopCluster(cl)
        return(res)
    } else { # if not Windows use mclapply
        res <- mclapply(X, 
                        FUN, 
                        ...,
                        mc.cores=nCores)
        return(res)
    }
}