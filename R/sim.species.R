#' @include RcppExports.R raspr-internal.R generics.R sim.space.R misc.R
NULL

#' @export sim.species
#' @rdname sim.species
sim.species.RasterLayer <- function(x, n=1, model=list('normal', 'constant', 'bimodal', RPgauss())[[1]], ...) {
	ret=list()
	# generate raster layers
	if (inherits(model, 'RMmodel')) {
		x<-sim.space(x, d=n, ...)
		for (i in seq_len(nlayers(x)))
			ret[[i]] <- setValues(x[[i]], inv.logit(getValues(x[[i]])))
	} else {
		x_coords<-rasterToPoints(x)
		for (i in seq_len(n))
			ret[[i]]<-setValues(x, do.call(paste0(model, '_niche'), args=list(x=x_coords[,1], y=x_coords[,2])))
	}
	# convert to logistic values using inv.logit
	# return RasterStack
	return(stack(ret))
}

#' @export sim.species
#' @rdname sim.species
sim.species.SpatialPolygons <- function(x, res, n=1, model=list('normal', 'uniform', 'bimodal', RPgauss())[[1]], ...) {
	# return simulations
	return(sim.species.RasterLayer(x=blank.raster(x, res), n=n, model=model, ...))
}

