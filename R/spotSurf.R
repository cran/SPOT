###################################################################################################
#' spotSurf3d
#'
#' Simple surface plot in three dimensions, using the rgl package with persp3 to plot.
#'
#' @param f function to be plotted. The function should either be able to take two vectors or one matrix specifying sample locations. i.e. \code{z=f(X)} or \code{z=f(x2,x1)} where Z is a two column matrix containing the sample locations \code{x1} and \code{x2}.
#' @param lo lower boundary for x1 and x2 (defaults to \code{c(0,0)}).
#' @param up upper boundary (defaults to \code{c(1,1)}).
#' @param s number of samples along each dimension. e.g. \code{f} will be evaluated \code{s^2} times.
#' @param clip \code{z} values smaller than \code{clip[1]} and larger than \code{clip[2]} are set to \code{NA}, to prevent them being visible in the plot. May result in ragged plots, but controls scaling. 
#' @param points can be omitted, but if given the points in this matrix are added to the plot
#' @param ... additional parameters passed to \code{f}
#'
#' @examples
#' spotSurf3d(function(x){apply(x,1,spotBraninFunction)},c(-5,0),c(10,15))
#'
#' @seealso \code{\link{spotSurfContour}} 
#' @export
###################################################################################################
spotSurf3d <- function(f=function(x){sum(x^2)}, lo=c(0,0) , up=c(1,1) , s=100,clip=c(NA,NA), points, ...){
	spotInstAndLoadPackages("rgl")
	x <- seq(lo[1], up[1], length = s)
	y <- seq(lo[2], up[2], length = s) 
	if(exists("..."))
		n_dot_args = length(list(...))
	else
		n_dot_args = 0
	if(length(formals(f))-n_dot_args==1){
		fn <- function(a,b){res<-f(cbind(a,b))}	
		z <- outer(x, y, fn, ...)
	}else if(length(formals(f))-n_dot_args==2){
		z <- outer(x, y, f, ...)	
	}		
	pal <- topo.colors(100)
	col.ind <- cut(z,100)
	open3d()
	if(!any(is.na(clip))){
		z[z>clip[2]]=NA
		z[z<clip[1]]=NA
	}
	persp3d(x, y, z, col=pal[col.ind],xlab="x",ylab="y")
	if(!missing(points)){
		rgl.points(points,color="black",size=4)
	}
}

###################################################################################################
#' spotSurfContour
#'
#' Simple surface plot, using the filled.contour function.
#'
#' @param f function to be plotted. The function should either be able to take two vectors or one matrix specifying sample locations. i.e. \code{y=f(X)} or \code{y=f(x2,x1)} where Z is a two column matrix containing the sample locations \code{x1} and \code{x2}.
#' @param lo lower boundary for x1 and x2 (defaults to \code{c(0,0)}).
#' @param up upper boundary (defaults to \code{c(1,1)}).
#' @param s number of samples along each dimension. e.g. \code{f} will be evaluated \code{s^2} times.
#' @param xlab lable of first axis
#' @param ylab lable of second axis
#' @param title title of the plot 
#' @param points1 can be omitted, but if given the points in this matrix are added to the plot in form of dots
#' @param points2 can be omitted, but if given the points in this matrix are added to the plot in form of crosses
#' @param ... additional parameters passed to \code{f}
#'
#' @examples
#' spotSurfContour(function(x){apply(x,1,spotBraninFunction)},c(-5,0),c(10,15))
#'
#' @seealso \code{\link{spotSurf3d}} 
#' @export
###################################################################################################
spotSurfContour <- function(f=function(x){sum(x^2)}, lo=c(0,0) , up=c(1,1) , s=100, xlab="x1",ylab="x2", title=" ", points1, points2, ...){
	x <- seq(lo[1], up[1], length = s)
	y <- seq(lo[2], up[2], length = s) 
	if(exists("..."))
		n_dot_args = length(list(...))
	else
		n_dot_args = 0
	if(length(formals(f))-n_dot_args==1){
		fn <- function(a,b){res<-f(cbind(a,b))}	
		z <- outer(x, y, fn, ...)
	}else if(length(formals(f))-n_dot_args==2){
		z <- outer(x, y, f, ...)	
	}		
	if(missing(points1)&missing(points2)){
		filled.contour(x, y, z, color.palette=terrain.colors,
				plot.title=title(title,
				xlab=xlab,
				ylab=ylab))
	}else if(missing(points1)&!missing(points2)){
			filled.contour(x, y, z, color.palette=terrain.colors, 
				plot.title=title(title,
				xlab=xlab,
				ylab=ylab),
				plot.axes = { points(points2,pch=4); axis(1); axis(2);	})
	}else if(!missing(points1)&missing(points2)){
			filled.contour(x, y, z, color.palette=terrain.colors, 
				plot.title=title(title,
				xlab=xlab,
				ylab=ylab),
				plot.axes = { points(points1,pch=19); axis(1); axis(2);	 })
	}else{
			filled.contour(x, y, z, color.palette=terrain.colors, 
				plot.title=title(title,
				xlab=xlab,
				ylab=ylab),
				plot.axes = { points(points1,pch=19); points(points2,pch=4);axis(1); axis(2); })
	}
	#todo: draw residuals?
}











