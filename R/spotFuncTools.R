###################################################################################
#' spotCalcNoise
#'
#' Calculates Noise for a given Function-Value (y) dependant on the
#' noise intensity (noise) and the way of calculating noise (noise.type)
#'
#' @param y the function value where noise will be added to
#' @param noise noise magnitude (absolute or weighted)
#' @param noise.type type of noise can either be "weighted" or "constant"
#' @param spot.noise.minimum.at.value Voice magnitude at minimum
#' 
#' @return numeric \cr
#' holding the noise Value
####################################################################################
spotCalcNoise <- function(y, noise=0.0, noise.type="weighted", spot.noise.minimum.at.value=0.0){
	
	noiseValue <- 0	
	if (noise == 0)return (noiseValue)

	if (noise.type=="weighted"){
		noiseValue <- (y - spot.noise.minimum.at.value)*noise*rnorm(1)/100
	}else if (noise.type=="constant"){
		noiseValue <- noise*rnorm(1)
	}
	
	return(noiseValue)
}
