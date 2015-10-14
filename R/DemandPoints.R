#' @include RcppExports.R raspr-internal.R generics.R
NULL

#' DemandPoints: An S4 class to represent demand points
#'
#' This class is used to store demand point information.
#'
#' @slot points \code{SimplePoints} coordinates for each demand point in the attribute space.
#' @slot weights \code{numeric} weights for each demand poont.
#' @seealso \code{\link{DemandPoints}}
#' @export
setClass("DemandPoints",
	representation(
		points='SimplePoints',
		weights='numeric'
	),
	validity=function(object) {
		# coords
		if (!inherits(object@points, 'SimplePoints'))
			stop('argument to points must be SimplePoints')
		
		# weights
		if (!all(is.numeric(object@weights)))
			stop('argument to weights must be numeric')
		if (!all(is.finite(object@weights)))
			stop('argument to weights contains NA or non-finite values')
		
		# cross-slot dependencies
		if (nrow(object@points@coords)!=length(object@weights))
			stop('argument to points must have have the same number of rows as the length of weights')
	}
)

#' Create new DemandPoints object
#'
#' This funciton creates a new DemandPoints object
#'
#' @param points \code{SimplePoints} coordinates for each demand point in the attribute space.
#' @param weights \code{numeric} weights for each demand poont.
#' @seealso \code{\link{DemandPoints-class}}
#' @export
#' @examples
#' dps <- DemandPoints(
#'	SimplePoints(matrix(rnorm(100), ncol=2)),
#'	runif(50)
#' )
DemandPoints<-function(points, weights) {
	dp<-new("DemandPoints", points=points, weights=weights)
	validObject(dp, test=FALSE)
	return(dp)
}

#' Generate demand points for RASP
#'
#' This function generates demand points for RASP using kernels.
#'
#' @param species.points \code{SpatialPointsDataFrame} or \code{SpatialPoints} with species presence records.
#' @param space.rasters \code{NULL}, \code{RasterLayer}, \code{RasterStack}, \code{RasterBrick} with projections of the attribute space over geographic space. If NULL (default) then geographic space is used.
#' @param n \code{integer} number of demand points to use for each attribute space for each species. Defaults to 100L.
#' @param quantile \code{numeric} quantile to generate demand points within. If 0 then demand points are generated across the full range of values the \code{species.points} intersect. Defaults to 0.2. 
#' @param kernel.method \code{character} name of kernel method to use to generate demand points. Defaults to 'sm.density'.
#' @param ... arguments passed to kernel density estimating functions
#' @return \code{DemandPoints} object.
#' @details The values of the species records in the rasters is extracted. A kernel is fit to the points in the attribute space. 
#' Volumes are then fit to the points in the attribute space(s). Points are randomly generated inside the volume. Demand points are generated
#' as random points inside the volume. A kernel is fit to the species records and used to predict the density at each of the demand points.
#' By using 'sm.density' as an argument in \code{kernel.method}, the volume is fit as a minimum convex polygon using \code{\link[rgeos]{gConvexHull}} and 
#' \code{\link[sm]{sm.density}} is used to fit the kernel. Note this can only be used when the data is low-dimensional (d<3). By using
#" 'hypervolume' as an argument, the \code{\link[hypervolume]{hypervolume}} function is used. This can be used for hyer-dimensional data.
#' @seealso \code{\link[hypervolume]{hypervolume}}, \code{\link[sm]{sm.density}}, \code{\link[rgeos]{gConvexHull}}.
#' @export
#' @examples
#' data(cs_spp, cs_space)
#' # generate species points
#'  species.points <- dismo::randomPoints(cs_spp, n=100, prob=TRUE, lonlatCorrection=FALSE)
#' # generate demand points for a 1d space using sm.density
#' dps1 <- make.DemandPoints(
#'	species.points=species.points,
#'	space.rasters=cs_space[[1]],
#'	kernel.method='sm.density'
#' )
#' # generate demand points for a 2d space using hypervolume
#' dps12 <- make.DemandPoints(
#'	species.points=species.points,
#'	space.rasters=cs_space,
#'	kernel.method='hypervolume'
#' )
make.DemandPoints<-function(species.points, space.rasters=NULL, n=100L, quantile=0.2, kernel.method=c('sm.density', 'hypervolume')[1], ...) {
	# check inputs for validityhod
	match.arg(kernel.method, c('sm.density', 'hypervolume'))
	if (!is.null(space.rasters))
		if (nlayers(space.rasters)>2 &  kernel.method=='sm.density')
			stop(paste0('argument to kernel.method must be "hypervolume" when nlayers(space.rasters)>2'))
	# extract values
	if (!is.null(space.rasters)) {
		space.points<-extract(space.rasters,species.points)
		if (!inherits(space.points, 'matrix'))
			space.points<-matrix(space.points, ncol=1)
	} else {
		space.points<-species.points@coords
	}
	# generate demand points for species.points
	if (kernel.method=='sm.density') {
		if (ncol(space.points)==1) {
			dp<-demand.points.density1d(space.points, n=n, quantile=quantile, ...)
		}
		if (ncol(space.points)==2) {
			dp<-demand.points.density2d(space.points, n=n, quantile=quantile, ...)
		}
	} else {
		dp<-demand.points.hypervolume(space.points, n=n, quantile=quantile, ...)
	}
	# return demand points
	return(
		DemandPoints(
			points=SimplePoints(dp$coords),
			weights=dp$weights
		)
	)
}

