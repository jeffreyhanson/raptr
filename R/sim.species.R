#' @include RcppExports.R raptr-internal.R generics.R sim.space.R misc.R
NULL

#' @rdname sim.species
#'
#' @method sim.species RasterLayer
#'
#' @export
sim.species.RasterLayer <- function(x, n = 1,
                                    model = list("uniform", "normal",
                                                 "bimodal",
                                                 RandomFields::RPgauss())[[1]],
                                                 ...) {
  ret <- list()
  # generate raster layers
  if (inherits(model, "RMmodel")) {
    x <- sim.space(x, d = n, ...)
    for (i in seq_len(raster::nlayers(x)))
      ret[[i]] <- raster::setValues(x[[i]],
                                    boot::inv.logit(raster::getValues(x[[i]])))
  } else {
    x_coords <- raster::rasterToPoints(x)
    for (i in seq_len(n))
      ret[[i]] <- raster::setValues(x, do.call(paste0(model, "_niche"),
                                               args = list(x = x_coords[, 1],
                                                           y = x_coords[, 2])))
  }
  # return RasterStack
  return(raster::stack(ret))
}

#' @rdname sim.species
#'
#' @method sim.species SpatialPolygons
#'
#' @export
sim.species.SpatialPolygons <- function(x, res, n = 1,
                                        model = list("normal", "uniform",
                                                     "bimodal",
                                                 RandomFields::RPgauss())[[1]],
                                                     ...) {
  sim.species.RasterLayer(x = blank.raster(x, res), n = n, model = model, ...)
}
