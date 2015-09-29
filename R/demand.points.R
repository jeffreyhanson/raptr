#' @include RcppExports.R raspr-internal.R generics.R DemandPoints.R
NULL

#' Generate demand points for RASP
#'
#' This function generates demand points for RASP using kernels.
#'
#' @param species.points \code{SpatialPointsDataFrame} or \code{SpatialPoints} with species presence records.
#' @param space.rasters \code{NULL}, \code{RasterLayer}, \code{RasterStack}, \code{RasterBrick} with projections of the attribute space over geographic space. If NULL then geographic space is used.
#' @param id \code{integer} species id.
#' @param demand.points \code{integer} number of demand points to use for each attribute space for each species.
#' @param kernel.method \code{character} name of kernel method to use to generate demand points.
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
demand.points<-function(species.points, space.rasters=NULL, id=1L, demand.points=1000L, kernel.method=c('sm.density', 'hypervolume')[1], ...) {
	# check inputs for validityhod
	match.args(kernel.method, c('sm.density', 'hypervolume'))
	if (nlyaers(space.rasters)>2 &  kernel.method=='hypervolume')
		stop(paste0('argument to kernel.method must be "hypervolume" when nlayers(space.rasters)>2'))
	# extract values
	if (!is.null(species.rasters)) {
		space.points<-extract(species.points, species.rasters)
	} else {
		space.points<-species.points@coords
	}
	# generate demand points for species.points
	if (kernel.method=='sm.density') {
		if (ncol(space.points)==1) {
			dp<-demand.points.density1d(space.points)
		}
		if (ncol(species.values)==2) {
			dp<-demand.points.density2d(space.points)
		} else {
			
		}
	} else {
		dp<-demand.points.hypervolume(space.points)
	}
	# return demand points
	return(
		DemandPoints(
			coords=dp$coords,
			weights=dp$weights,
			species=rep(id, demand.points)
		)
	)
}

