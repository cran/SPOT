###################################################################################
#' spotPredictPOKER
#' 
#' Price Of Knowledge and Estimated Reward - ensemble approach
#'
#' @param rawB unmerged data
#' @param mergedB merged data
#' @param largeDesign new design points which should be predicted
#' @param spotConfig global list of all options, needed to provide data for calling functions
#' @param externalFit if an existing model fit is supplied, the model will not be build based on 
#'				data, but only evaluated with the model fit (on the largeDesign data). To build the model, 
#'				this parameter has to be NULL. If it is not NULL the paramters mergedB and rawB will not be 
#'				used at all in the function.
#'
#' @return returns the list \code{spotConfig} with two new entries:\cr
#' 	spotConfig$seq.modelFit fit of the Krig model used with predict() \cr
#'	spotConfig$seq.largeDesignY the y values of the large design, evaluated with the fit
#' @export
###################################################################################
spotPredictPOKER <- function(rawB,mergedB,largeDesign,spotConfig,externalFit=NULL){	
	models=spotConfig$seq.ensemble.models
	K=length(models)
	#if(is.null(spotConfig$seq.poker.T)){spotConfig$seq.poker.T<-10}
	H<-5
	#First calculate rewards if possible	
	if(!is.null(spotConfig$seq.poker.imax)){ #calculate reward if a model was chosen in the last step
		#reward is calculated by difference most recent "best" values
		
		mergedData <- spotPrepareData(spotConfig)
		reward<-spotConfig$alg.currentBest[nrow(spotConfig$alg.currentBest)-1,1]-min(mergedData$mergedY[mergedData$STEP==max(mergedData$STEP)])
		browser()
		#if(reward<0){
		#	reward=0
		#}
		spotConfig$seq.poker.r[spotConfig$seq.poker.imax]<-spotConfig$seq.poker.r[spotConfig$seq.poker.imax]+reward
		spotConfig$seq.poker.r2[spotConfig$seq.poker.imax]<-spotConfig$seq.poker.r2[spotConfig$seq.poker.imax]+reward^2
	}
	
	if(is.null(spotConfig$seq.poker.n)){
	#first initialization step
		spotConfig$seq.poker.n<-rep(0,K)
		spotConfig$seq.poker.r<-rep(0,K)
		spotConfig$seq.poker.r2<-rep(0,K)
		spotConfig$seq.poker.t<-1
		imax<-sample(1:K,1)
	}
	else if(spotConfig$seq.poker.t==2){
	#second initialization step, still completely random
		temp<-1:K
		temp<-temp[-spotConfig$seq.poker.imax]
		if(length(temp)==1){imax<-temp}
		else{imax<-sample(temp,1)}
	}
	else{
		q<- length(which(spotConfig$seq.poker.r>0))
		i0<-which.max(spotConfig$seq.poker.r/spotConfig$seq.poker.n)
		i1<-which(round(sqrt(q))==order(spotConfig$seq.poker.r))
		mus<-i0
		deltamu<-((spotConfig$seq.poker.r[i0]/spotConfig$seq.poker.n[i0])-(spotConfig$seq.poker.r[i1]/spotConfig$seq.poker.n[i1]))/round(sqrt(q))
		pmax<-(-Inf)#initialize pmax and imax
		imax<-NULL
		for(i in 1:K){
			if(spotConfig$seq.poker.n[i]>0){
				n<-spotConfig$seq.poker.n[i]
				mu<-spotConfig$seq.poker.r[i]/spotConfig$seq.poker.n[i]
			}else{
				n<-1 #better solution? else there will be divided by zero for p<-mu+deltamu*.......			
				k<-which(spotConfig$seq.poker.n>0)
				mu<-sum(spotConfig$seq.poker.r[k]/spotConfig$seq.poker.n[k])/sum(spotConfig$seq.poker.n[k])			
			}
			if(spotConfig$seq.poker.n[i]>1){
				sigma<-sqrt((spotConfig$seq.poker.r2[i]/spotConfig$seq.poker.n[i])-((spotConfig$seq.poker.r[i]/spotConfig$seq.poker.n[i])^2))
			}else{
				k<-which(spotConfig$seq.poker.n>1)
				sigma<-sum(sqrt((spotConfig$seq.poker.r2[k]/spotConfig$seq.poker.n[k])-((spotConfig$seq.poker.r[k]/spotConfig$seq.poker.n[k])^2)))/sum(spotConfig$seq.poker.n[k])		
			}	
			#hier H durch T-t ersetzen?
			p<-mu+deltamu*H*(1-pnorm(mus+deltamu,mu,sigma/sqrt(n)))
			if(p>pmax){
				pmax<-p
				imax<-i
			}
			browser()
		}	
	}	
	conf <- eval(call(models[imax] #evaluated chosen model
								, rawB
								, mergedB
								, largeDesign
								, spotConfig))
	res=conf$seq.largeDesignY	
	spotConfig$seq.poker.imax<-imax#safe index, to associate reward with model
	spotConfig$seq.largeDesignY<-res#usual output
	spotConfig$seq.poker.n[imax]<-spotConfig$seq.poker.n[imax]+1#increase number of "lever pulls" for chosen model
	spotConfig$seq.poker.t<-spotConfig$seq.poker.t+1#increment internal step counter
	return(spotConfig)#...
}
