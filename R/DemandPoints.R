#' @include RcppExports.R raptr-internal.R generics.R
NULL

#' DemandPoints: An S4 class to represent demand points
#'
#' This class is used to store demand point information.
#'
#' @slot coords \code{matrix} of coordinates for each demand point.
#' @slot weights \code{numeric} weights for each demand point.
#' @seealso \code{\link{DemandPoints}}
#' @export
setClass("DemandPoints",
	representation(
		coords='matrix',
		weights='numeric'
	),
	validity=function(object) {
		# check coords have variance
		expect_false(max(apply(object@coords, 2, function(x) {length(unique(x))}))==1, info='demand points must not all be identical')

		# check coords are not NA
		expect_true(all(is.finite(c(object@coords))), info='argument to coords contains NA or non-finite values')
		expect_true(nrow(object@coords)>0, info='argument to coords must have at least one row')
		
		# weights
		expect_true(all(is.finite(object@weights)), info='argument to weights contains NA or non-finite values')
		expect_true(length(object@weights)>0, info='argument to weights must have at least one element')
		expect_true(all(object@weights>0), info='argument to weights must have positive numbers')

		# cross-slot dependencies
		expect_equal(nrow(object@coords), length(object@weights), info='argument to points must have have the same number of rows as the length of weights')
		return(TRUE)
	}
)

#' Create new DemandPoints object
#'
#' This function creates a new DemandPoints object
#'
#' @param coords \code{matrix} of coordinates for each demand point.
#' @param weights \code{numeric} weights for each demand point.
#' @seealso \code{\link{DemandPoints-class}}
#' @export
#' @examples
#' dps <- DemandPoints(
#'	matrix(rnorm(100), ncol=2),
#'	runif(50)
#' )
DemandPoints<-function(coords, weights) {
	dp<-new("DemandPoints", coords=coords, weights=weights)
	validObject(dp, test=FALSE)
	return(dp)
}

#' Generate demand points for RAP
#'
#' This function generates demand points for RAP using kernels.
#'
#' @param points \code{SpatialPointsDataFrame} or \code{SpatialPoints} with species presence records.
#' @param n \code{integer} number of demand points to use for each attribute space for each species. Defaults to 100L.
#' @param quantile \code{numeric} quantile to generate demand points within. If 0 then demand points are generated across the full range of values the \code{points} intersect. Defaults to 0.5. 
#' @param kernel.method \code{character} name of kernel method to use to generate demand points. Defaults to 'ks'.
#' @param ... arguments passed to kernel density estimating functions
#' @return \code{DemandPoints} object.
#' @details The values of the species records in the rasters is extracted. A kernel is fit to the points in the attribute space. 
#' Volumes are then fit to the points in the attribute space(s). Points are randomly generated inside the volume. Demand points are generated
#' as random points inside the volume. A kernel is fit to the species records and used to predict the density at each of the demand points.
#' By using 'ks' as an argument in \code{kernel.method}, the volume is fit as a minimum convex polygon using \code{\link[adehabitatHR]{mcp}} and 
#' \code{\link[ks]{kde}} is used to fit the kernel. Note this can only be used when the data is low-dimensional (d<3). By using
#' 'hypervolume' as an argument, the \code{\link[hypervolume]{hypervolume}} function is used. This can be used for hyper-dimensional data.
#' @seealso \code{\link[hypervolume]{hypervolume}}, \code{\link[ks]{kde}}, \code{\link[adehabitatHR]{mcp}}.
#' @export
#' @examples
#' data(cs_spp, cs_space)
#' # generate species points
#' species.points <- randomPoints(cs_spp[[1]], n=100, prob=TRUE)
#' env.points <- extract(cs_space, species.points)
#' # generate demand points for a 1d space using ks
#' dps1 <- make.DemandPoints(
#'	points=env.points[,1],
#'	kernel.method='ks'
#' )
#' # generate demand points for a 2d space using hypervolume
#' dps2 <- make.DemandPoints(
#'	points=env.points,
#'	kernel.method='hypervolume'
#' )
make.DemandPoints<-function(points, n=100L, quantile=0.5, kernel.method=c('ks', 'hypervolume')[1], ...) {
	# check inputs for validity
	o <- points
	
	if (sum(!is.finite(points))>0)
		stop('argument to points contains non-finite values')
	kernel.method <- match.arg(kernel.method, c('ks', 'hypervolume'))
	# convert to matrix
	if (!inherits(points, 'matrix') & inherits(points, 'numeric'))
			points<-matrix(points, ncol=1)
	if (ncol(points)>2 & kernel.method!='hypervolume')
			stop(paste0('argument to kernel.method must be "hypervolume" when ncol(points)>2'))
	# generate demand points
	if (kernel.method=='ks') {
		if (ncol(points)==1) {
			dp<-demand.points.density1d(points, n=n, quantile=quantile, ...)
		}
		if (ncol(points)==2) {
			dp<-demand.points.density2d(points, n=n, quantile=quantile, ...)
		}
	} else {
		dp<-demand.points.hypervolume(points, n=n, quantile=quantile, ...)
	}
	# return demand points
	return(
		DemandPoints(
			coords=dp$coords,
			weights=dp$weights
		)
	)
}

