##################################################################################
#' Sorting by NDS-rank and Hypervolume Contribution
#'
#' Sorts the large design for the purpose of multi objective optimization with SPOT.
#' First non dominated sorting rank (NDS) is used. If the choice of points for the next
#' sequential step is not clear by nds rank, the hypervolume contribution of the 
#' competing points is recalculated sequentially to remove those with the smallest 
#' contribution.
#'
#' @param largeDesign the design matrix in the parameter space, to be sorted by the associated y-values for each objective
#' @param designY objective value matrix. Contains objective values associated to largeDesign
#' @param newsize this is the number of points that need to be selected, i.e. the seq.design.new.size
#' @return largeDesign \cr 
#' - The sorted large design
#' @export
###################################################################################
spotMcoSort <- function (largeDesign, designY, newsize){
	lhdY=t(as.matrix(designY))
	ndsR<-nds_rank(lhdY) #First: Sort by nds rank
	largeDesign <-  as.data.frame(largeDesign[order(ndsR,decreasing=FALSE),]);
	lhdY<-lhdY[,order(ndsR,decreasing=FALSE)]
	ndsR<-ndsR[order(ndsR)]
	summe<-0
	if(newsize<nrow(largeDesign)){
		for(i in 1:max(ndsR)){ #Look for front that will not be used completely, sort it by hypervol contrib
			index<-which(ndsR==i)
			summe<-summe+length(index)
			if(summe==newsize)break;
			if((summe>newsize) && (length(index)>1)){
				set<-largeDesign[index,]
				removeN=summe-newsize
				sortVec=rep(0,length(index))
				frontY<-lhdY[,index]
				for(jj in 1:removeN){ #repeated selection by hypervolume contribution, selected individuals will be last in order
					iREM=nds_hv_selection(frontY[,sortVec==0])
					iREM= which(frontY==frontY[,sortVec==0][,iREM],arr.ind=T)[1,2] #ugly hack to select which column to remove by comparing with original front
					sortVec[iREM]=1						
				}
				set<-set[order(sortVec),]
				largeDesign[index,]<-set
				break;
			}
		}
	}
	return(largeDesign)	
}


##################################################################################
#' Sorting by NDS-rank and Hypervolume Contribution, with known points
#'
#' Sorts the large design for the purpose of multi objective optimization with SPOT.
#' First non dominated sorting (NDS) rank is used. If the choice of points for the next
#' sequential step is not clear by nds rank, the hypervolume contribution of the 
#' competing points is recalculated sequentially to remove those with the smallest 
#' contribution. 
#'
#' In contrast to \code{\link{spotMcoSort}}, this function consideres the known points in \code{mergedX} and \code{mergedY}
#' so that new points will rather be chosen in between known points, thus producing a better Pareto front.
#' To do so, the known points are added to the set of solutions. To ensure that they are not removed, they receive infinite hypervolume contribution,
#' and are not counted when determining the number of NDS ranks to be considered.
#'
#' @param largeDesign the design matrix in the parameter space, to be sorted by the associated y-values for each objective
#' @param designY objective value matrix. Contains objective values associated to largeDesign
#' @param newsize this is the number of points that need to be selected, i.e. the seq.design.new.size
#' @param mergedX position of the already known points in parameter space (vector of parameter values)
#' @param mergedY y-values of the already known points (vector of objective values)
#' @return largeDesign \cr 
#' - The sorted large design
#' @export
###################################################################################
spotMcoInfill <- function (largeDesign, designY, newsize, mergedX, mergedY){
	info<-c(rep(0,nrow(designY)),rep(1,nrow(mergedY))) # create info of point origin (0=largedesign, 1=allready evaluated)
	colnames(designY)<-colnames(mergedY)
	designY<-rbind(designY,mergedY)#merge points and known values	
	largeDesign<-rbind(largeDesign,mergedX)#merge large design and known values	
	lhdY=t(as.matrix(designY))
	ndsR<-nds_rank(lhdY) #determine nds rank
	info<-info[order(ndsR,decreasing=FALSE)] #sort info about points by nds rank
	largeDesign <-  as.data.frame(largeDesign[order(ndsR,decreasing=FALSE),]) #sort large design by nds rank
	lhdY<-lhdY[,order(ndsR,decreasing=FALSE)] #sort y values by nds rank
	ndsR<-ndsR[order(ndsR)]#sort ndsR values by nds rank
	summe<-0 #initial value for sum
	if(newsize<nrow(largeDesign)){
		for(i in 1:max(ndsR)){ #Look for front that will not be used completely, sort it by hypervol contrib
			index<-which(ndsR==i)
			summe<-summe+length(index)- sum(info[index]) #sum of points from this point (which will be added): all points, minus known points
			if(summe==newsize)break;
			if((summe>newsize) && (length(index)>1)){
				set<-largeDesign[index,]
				removeN=summe-newsize
				sortVec=rep(0,length(index))
				frontY<-lhdY[,index]
				frontInfo<-info[index]
				for(jj in 1:removeN){ #repeated selection by hypervolume contribution, selected individuals will be last in order
					contrib<-hypervolume_contribution(frontY[,sortVec==0]) #todo: check if this algorithm works robustly
					contrib[frontInfo==1]<-Inf#max(contrib)+1 #make sure known points can not be removed!    
					iREM=which.min(contrib)    
					iREM= which(frontY==frontY[,sortVec==0][,iREM],arr.ind=T)[1,2] #ugly hack to select which column to remove by comparing with original front
					sortVec[iREM]=1						
				}
				set<-set[order(sortVec),]
				info[index]<-frontInfo[order(sortVec)]
				largeDesign[index,]<-set #fill sorted pareto set into large design
				break;
			}
		}
	}	
	largeDesign<-largeDesign[info==0,]#remove known points from largedesign
	return(largeDesign)	
}