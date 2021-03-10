#' @title Preprocess y Values to Plot Best Objective Value
#' 
#' @param y result vector 
#' @param end length. Default: \code{length(y)}
#' 
#' @return prog
#' 
#' @export
#' 
prepareBestObjectiveVal <- function(y, end=length(y)) {
prog <- rep(min(y), end)
prog[1:min(end, length(y))] <- y[1:min(end, length(y))] 
for(i in 2:end)
  if(is.na(prog[i]) || prog[i] > prog[i-1]) prog[i] <- prog[i-1] 
return(prog)
}


#' @title Plot Best Objective Value
#' 
#' @param y result vector 
#' @param end length. Default: \code{length(y)}
#' 
#' @return plot 
#' 
#' @importFrom graphics plot  
#'    
#' @export
#' 
plotBestObj <- function(y, end=length(y)) {
  prog <- prepareBestObjectiveVal(y)
  plot(prog, type = "l", col = "grey", 
       xlab = "function evaluations",
       ylab = "best objective value")
}