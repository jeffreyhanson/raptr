#' @include RcppExports.R raptr-internal.R generics.R misc.R
NULL

#' Simulate attribute space data for RAP
#'
#' This function simulates attribute space data for RAP.
#'
#' @inheritParams sim.species
#'
#' @param d `integer` number of dimensions. Defaults to 2.
#'
#' @param model `numeric` scale parameter for simulating spatially
#'   auto-correlated data using Gaussian random fields.
#'   Higher values produce patchier data with more well defined clusters,
#'   and lower values produce more evenly distributed data.
#'   Defaults to 0.2.
#'
#' @return [terra::rast()] with layers for each dimension of the space.
#'
#' @name sim.space
#'
#' @examples
#' \dontrun{
#' # simulate planning units
#' sim_pus <- sim.pus(225L)
#'
#' # simulate 1d space using SpatRaster
#' s1 <- sim.space(blank.raster(sim_pus, 1), d = 1)
#'
#' # simulate 1d space using sf
#' s2 <- sim.space(sim_pus, res = 1, d = 1)
#'
#' # simulate 2d space using sf
#' s3 <- sim.space(sim_pus, res = 1, d = 2)
#'
#' # plot simulated spaces
#' par(mfrow = c(2,2))
#' plot(s1, main = "s1")
#' plot(s2, main = "s2")
#' plot(s3[[1]], main = "s3: first dimension")
#' plot(s3[[2]], main = "s3: second dimension")
#' }
#' @export sim.space
sim.space <- function(x, ...) UseMethod("sim.space")

#' @rdname sim.space
#'
#' @method sim.space SpatRaster
#'
#' @export
sim.space.SpatRaster <- function(x, d = 2, model = 0.2, ...) {
  pts <- terra::as.data.frame(x[[1]], xy = TRUE)
  idx <- which(!is.na(pts[[3]]))
  pts <- as.matrix(pts[, c("x", "y"), drop = FALSE])
  # generate values for rasters
  valMTX <- simulate_gaussian_random_field(
    n = d,
    coords = pts,
    mu = 0,
    scale = model
  )
  # convert to matrix if not a matrix
  if (!inherits(valMTX, "matrix")) {
    valMTX <- matrix(valMTX, ncol = 1)
  }
  # populate rasters with values
  r <- terra::rast(lapply(seq_len(ncol(valMTX)), function(i) {
    r <- x
    r[idx] <- valMTX[, i]
    r
  }))
  # return object
  r
}

#' @rdname sim.space
#'
#' @method sim.space SpatialPolygons
#'
#' @export
sim.space.SpatialPolygons <- function(x, res, d = 2, model = 0.2, ...) {
  .Defunct(
    msg = paste(
      "support for sp::SpatialPolygons data has been deprecated,",
      "use sf::st_as_sf() to convert to an sf::st_sf() object and try again"
    )
  )
}

#' @rdname sim.space
#'
#' @method sim.space sf
#'
#' @export
sim.space.sf <- function(x, res, d = 2, model = 0.2, ...) {
  sim.space.SpatRaster(blank.raster(x, res), d = d, model = model, ...)
}
