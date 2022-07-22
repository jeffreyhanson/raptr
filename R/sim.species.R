#' @include RcppExports.R raptr-internal.R generics.R sim.space.R misc.R
NULL

#' @rdname sim.species
#'
#' @method sim.species RasterLayer
#'
#' @export
sim.species.RasterLayer <- function(x, n = 1, model = "normal", ...) {
  assertthat::assert_that(
    length(list(...)) == 0,
    msg = c(
      "additional arguments should not be supplied because they are not used."
    )
  )
  # initialize output
  ret <- list()
  # generate raster layers
  if (inherits(model, "numeric")) {
    ## simulate data
    x <- sim.space(x, d = n, scale = model, ...)
    for (i in seq_len(raster::nlayers(x)))
      ret[[i]] <- raster::setValues(x[[i]],
                                    boot::inv.logit(raster::getValues(x[[i]])))
  } else {
    assertthat::assert_that(
      assertthat::is.string(model),
      assertthat::noNA(model)
    )
    assertthat::assert_that(
      model %in% c("uniform", "normal", "bimodal")
    )
    x_coords <- raster::rasterToPoints(x)
    for (i in seq_len(n))
      ret[[i]] <- raster::setValues(x, do.call(paste0(model, "_niche"),
                                               args = list(x = x_coords[, 1],
                                                           y = x_coords[, 2])))
  }
  # return RasterStack object
  raster::stack(ret)
}

#' @rdname sim.species
#'
#' @method sim.species SpatialPolygons
#'
#' @export
sim.species.SpatialPolygons <- function(x, res, n = 1, model = "normal", ...) {
  sim.species.RasterLayer(x = blank.raster(x, res), n = n, model = model, ...)
}
