#' @include RcppExports.R raspr-internal.R generics.R
NULL

#' Simulate planning units
#'
#' This function simulates planning units for RASP.
#'
#' @param n \code{integer} number of planning units. \code{sqrt(n)} must yield a valid number.
#' @param xmn \code{numeric} value for minimim x-coordinate. 
#' @param xmx \code{numeric} value for maximum x-coordinate. 
#' @param ymn \code{numeric} value for minimim y-coordinate. 
#' @param ymx \code{numeric} value for maximum y-coordinate.
#' @return \code{SpatialPolygons} with planning units.
#' @details Square planning units are generated in the shape of a square. Default coordinate arguments are such that the planning units will be centered at origin.
#' The data slot contains an 'id' (\code{integer}), cost (\code{numeric}), 'status' (\code{integer}), and area (\code{numeric}).
#' @export
simulate.pus <- function(n, xmn=-sqrt(n)/2, xmx=sqrt(n)/2, ymn=-sqrt(n)/2, ymx=sqrt(n)/2, ...) {
	# check n has valid square root
	if (sqrt(n)!=ceiling(sqrt(n)))
		stop('sqrt(n) must yield a whole number')
	# create raster
	rst=raster(nrow=sqrt(n), ncol=sqrt(n), xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx)
	rst=setValues(rst, seq_len(n))
	# convert to SpatialPolygonsDataFrame
	ret=as(rst, 'SpatialPolygonsDataFrame')
	# insert default values 
	ret@data$cost=1
	ret@data$status=0L
	ret@data$area=prod(res(rst))
	ret@data=ret@data[,-1,drop=FALSE]
	names(ret@data)=c('cost','status', 'area')
	# return polygons 
	return(ret)
}


 
