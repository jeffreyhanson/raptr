#' @include RcppExports.R raspr-internal.R generics.R simulate.space.R misc.R
NULL

#' @export
#' @rdname simulate.species
simulate.species.RasterLayer <- function(x, n=1, model=RPgauss(), ...) {
	# generate raster layers
	x<-simulate.space(x, d=n, ...)
	# convert to logistic values using inv.logit
	for (i in seq_along(nlayers(x)))
		x[[i]] <- setValues(x[[i]], inv.logit(getValues(x[[i]])))
	# return RasterStack
	return(stack(x))
}

#' @export
#' @rdname simulate.species
simulate.species.SpatialPolygons <- function(x, res, n=1, model=RPgauss(), ...) {
	# return simulations
	return(simulate.species.RasterLayer(blank.raster(x, res), n=n, model=model, ...))
}

