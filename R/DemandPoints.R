#' @include RcppExports.R raspr-internal.R generics.R
NULL

#' DemandPoints: An S4 class to represent demand points
#'
#' This class is used to store demand point information.
#'
#' @slot coords \code{matrix} coordinates for each demand point in the attribute space.
#' @slot weights \code{numeric} weights for each demand poont.
#' @slot species \code{integer} idenifiers for which species each demand point belongs to.
#' @seealso \code{\link{DemandPoints}}
#' @export
setClass("DemandPoints",
	representation(
		coords='matrix',
		weights='numeric',
		species='integer'
	),
	validity=function(object) {
		# coords
		if (!all(is.matrix(object@coords)))
			stop('argument to coords must be matrix')
		if (!all(is.finite(object@coords)))
			stop('argument to coords contains NA or non-finite values')
		
		# weights
		if (!all(is.numeric(object@weights)))
			stop('argument to weights must be numeric')
		if (!all(is.finite(object@weights)))
			stop('argument to weights contains NA or non-finite values')
		
		# species
		if (!all(is.integer(object@species)))
			stop('argument to species must be integer')
		if (!all(is.finite(object@species)))
			stop('argument to species contains NA or non-finite values')

		# cross-slot dependencies
		if (nrow(object@coords)!=length(object@species))
			stop('argument to coords must have have the same number of rows as the length of species')
		if (nrow(object@coords)!=length(object@weights))
			stop('argument to coords must have have the same number of rows as the length of weights')
	}
)

#' Create new DemandPoints object
#'
#' This funciton creates a new DemandPoints object
#'
#' @slot coords \code{matrix} coordinates for each demand point in the attribute space.
#' @slot weights \code{numeric} weights for each demand poont.
#' @slot species \code{integer} idenifiers for which species each demand point belongs to.
#' @seealso \code{\link{DemandPoints-class}}
#' @export
DemandPoints<-function(coords, weights, species) {
	# make new dp object
	dp<-new("DemandPoints", coords=coords, weights=weights, species=species)
	# test for validity
	validObject(dp, test=FALSE)
	return(dp)
}

#' Merge a list of DemandPoints objects togeather
#'
#' This is a conviencne function to merge DemandPoints objects togeather.
#'
#' @param x \code{list} of \code{DemandPoints} objects.
#' @seealso \code{DemandPoints}
#' @export
merge.DemandPoints<-function(x, ...) {
	# check all elements have same dimensions
	x<<-x
	if (length(unique(laply(x, function(x) {ncol(x@coords)})))!=1)
		stop('Demand points have different numbers of dimensions in the attribute space')
	# return new DemandPoints object
	return(
		DemandPoints(
			coords=do.call(rbind, llply(x, slot, 'coords')),
			species=unlist(sapply(x, slot, 'species', simplify=FALSE, USE.NAMES=FALSE), recursive=FALSE, use.names=FALSE),
			weights=unlist(sapply(x, slot, 'weights', simplify=FALSE, USE.NAMES=FALSE), recursive=FALSE, use.names=FALSE)
		)
	)
}


#' Generate demand points for RASP
#'
#' This function generates demand points for RASP using kernels.
#'
#' @param species.points \code{SpatialPointsDataFrame} or \code{SpatialPoints} with species presence records.
#' @param space.rasters \code{NULL}, \code{RasterLayer}, \code{RasterStack}, \code{RasterBrick} with projections of the attribute space over geographic space. If NULL (default) then geographic space is used.
#' @param n \code{integer} number of demand points to use for each attribute space for each species. Defaults to 1000L.
#' @param quantile \code{numeric} quantile to generate demand points within. If 0 then demand points are generated across the full range of values the \code{species.points} intersect. Defaults to 0.2. 
#' @param id \code{integer} species id. Defaults to 1L.
#' @param kernel.method \code{character} name of kernel method to use to generate demand points. Defaults to 'sm.density'.
#' @param ... arguments passed to kernel density estimating functions
#' @return \code{DemandPoints} object.
#' @details The values of the species records in the rasters is extracted. A kernel is fit to the points in the attribute space. 
#' Volumes are then fit to the points in the attribute space(s). Points are randomly generated inside the volume. Demand points are generated
#' as random points inside the volume. A kernel is fit to the species records and used to predict the density at each of the demand points.
#' By using 'sm.density' as an argument in \code{kernel.method}, the volume is fit as a minimum convex polygon using \code{\link[rgeos]{gConvex}} and 
#' \code{\link[sm]{sm.density}} is used to fit the kernel. Note this can only be used when the data is low-dimensional (d<3). By using
#" 'hypervolume' as an argument, the \code{\link[hypervolume]{hypervolume}} function is used. This can be used for hyer-dimensional data.
#' @seealso \code{\link[hypervolume]{hypervolume}}, \code{\link[sm]{sm.density}}, \code{\link[rgeos]{gConvex}}.
#' @export
make.DemandPoints<-function(species.points, space.rasters=NULL, n=1000L, quantile=0.2, id=1L, kernel.method=c('sm.density', 'hypervolume')[1], ...) {
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
			coords=dp$coords,
			weights=dp$weights,
			species=rep(id, n)
		)
	)
}

