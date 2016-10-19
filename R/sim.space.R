#' @include RcppExports.R raptr-internal.R generics.R misc.R
NULL

#' @rdname sim.space
#' @method sim.space RasterLayer
#' @export
sim.space.RasterLayer<-function(x, d=2, model=RMgauss(), ...) {
	# generate values for rasters
	valMTX <- RFsimulate(model=model, as(x, 'SpatialPoints')@coords, n=d, spConform=FALSE, ...)
	# convert to matrix if not a matrix
	if (!inherits(valMTX, 'matrix'))
		valMTX <- matrix(valMTX, ncol=1)
	# populate rasters with values
	stk <- stack(llply(seq_len(ncol(valMTX)), .fun=function(i) {
		r <- x
		r[Which(!is.na(r))] <- valMTX[,i]
		return(r)
	}))
	# return RasterStack
	return(stack(stk))
}

#' @rdname sim.space
#' @method sim.space SpatialPolygons
#' @export
sim.space.SpatialPolygons<-function(x, res, d=2, model=RMgauss(), ...) {
	# return simulations
	return(sim.space.RasterLayer(blank.raster(x, res), d=d, model=model, ...))
}
