#' @include RcppExports.R raspr-internal.R generics.R misc.R
NULL

#' @export
#' @rdname simulate.space
simulate.space.RasterLayer <- function(x, d=2, model=RMgauss(), ...) {
	# generate values for rasters
	valMTX=RFsimulate(model, as(x, 'SpatialPoints')@coords, n=d, spConform=FALSE, ...)
	# convert to matrix if not a matrix
	if (!inherits(valMTX, 'matrix'))
		valMTX=matrix(valMTX, ncol=1)
	# populate rasters with values
	stk=stack(llply(seq_len(ncol(valMTX)), .fun=function(i) {
		r=x
		r[Which(!is.na(r))]=valMTX[,i]
		return(r)
	}))
	# return RasterStack
	return(stack(stk))
}

#' @export
#' @rdname simulate.space
simulate.space.SpatialPolygons <- function(x, res, d=2, model=RMgauss(), ...) {
	# return simulations
	return(simulate.space.RasterLayer(blank.raster(x, res), d=d, model=model, ...))
}
