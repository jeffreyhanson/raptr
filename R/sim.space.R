#' @include RcppExports.R raptr-internal.R generics.R misc.R
NULL

#' @rdname sim.space
#'
#' @method sim.space RasterLayer
#'
#' @export
sim.space.RasterLayer <- function(x, d  =2, model = RandomFields::RMgauss(),
                                  ...) {
  # generate values for rasters
  valMTX <- RandomFields::RFsimulate(model = model,
                                     methods::as(x, "SpatialPoints")@coords,
                                     n = d,
                                     spConform = FALSE, ...)
  # convert to matrix if not a matrix
  if (!inherits(valMTX, "matrix"))
    valMTX <- matrix(valMTX, ncol = 1)
  # populate rasters with values
  stk <- raster::stack(lapply(seq_len(ncol(valMTX)),
                              function(i) {
    r <- x
    r[raster::Which(!is.na(r))] <- valMTX[, i]
    r
  }))
  # return RasterStack object
  raster::stack(stk)
}

#' @rdname sim.space
#'
#' @method sim.space SpatialPolygons
#'
#' @export
sim.space.SpatialPolygons <- function(x, res, d = 2,
                                      model = RandomFields::RMgauss(), ...) {
  sim.space.RasterLayer(blank.raster(x, res), d = d, model = model, ...)
}
