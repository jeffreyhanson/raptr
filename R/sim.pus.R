#' @include RcppExports.R raptr-internal.R generics.R
NULL

#' Simulate planning units
#'
#' This function simulates planning units for RAP.
#'
#' @param n \code{integer} number of planning units. \code{sqrt(n)} must yield a valid number.
#' @param xmn \code{numeric} value for minimum x-coordinate.
#' @param xmx \code{numeric} value for maximum x-coordinate.
#' @param ymn \code{numeric} value for minimum y-coordinate.
#' @param ymx \code{numeric} value for maximum y-coordinate.
#' @return \code{SpatialPolygons} with planning units.
#' @details Square planning units are generated in the shape of a square. Default coordinate arguments are such that the planning units will be centered at origin.
#' The data slot contains an 'id' (\code{integer}), cost (\code{numeric}), 'status' (\code{integer}), and area (\code{numeric}).
#' @export sim.pus
#' @examples
#' # generate 225 sqauare planning units arranged in a square with 1 unit height/width
#' x <- sim.pus(225)
#' # generate 225 rectangular pus arranged in a square
#' y <- sim.pus(225, xmn=-5, xmx=10, ymn=-5, ymx=5)
#' \dontrun{
#' par(mfrow=c(1,2))
#' plot(x, main='x')
#' plot(y, main='y')
#' par(mfrow=c(1,1))
#' }
sim.pus <- function(n, xmn=-sqrt(n)/2, xmx=sqrt(n)/2, ymn=-sqrt(n)/2, ymx=sqrt(n)/2) {
	# check n has valid square root
	if (sqrt(n)!=ceiling(sqrt(n)))
		stop('sqrt(n) must yield a whole number')
	# create raster
	rst <- raster(nrow=sqrt(n), ncol=sqrt(n), xmn=xmn, xmx=xmx, ymn=ymn, ymx=ymx)
	rst <- setValues(rst, seq_len(n))
	# convert to SpatialPolygonsDataFrame
	ret <- as(rst, 'SpatialPolygonsDataFrame')
	# insert default values
	ret@data$cost <- 1
	ret@data$status <- 0L
	ret@data$area=prod(res(rst))
	ret@data <- ret@data[,-1,drop=FALSE]
	names(ret@data) <- c('cost','status', 'area')
	# return polygons
	return(ret)
}
