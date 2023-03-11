#' @include RcppExports.R raptr-internal.R generics.R sim.space.R misc.R
NULL

#' Simulate species distribution data for RAP
#'
#' This function simulates species distributions for RAP.
#'
#' @param x [terra::rast()] or [sf::st_sf()] object delineating
#'   the spatial extent for the study area.
#'
#' @param n `integer` number of species. Defaults to 1.
#'
#' @param res `numeric` resolution to simulate distributions. Only needed
#'   when [sf::st_sf()] are supplied.
#'
#' @param model `character` or `numeric` for simulating data.
#'   If a `character` value is supplied, then the following values can
#'   can be used to simulate species distributions with particular
#'   characteristics:
#'   `"uniform"`, `"normal"`, and `"bimodal"`.
#'   If a `numeric` value is supplied, then this is used to simulate
#'   species distributions using a Gaussian random field, where the
#'   `numeric` value is treated as the scale parameter.
#'   Defaults to `"normal"`.
#'
#' @param ... not used.
#'
#' @return [terra::rast()] with layers for each species.
#'
#' @examples
#' \dontrun{
#' # make polygons
#' sim_pus <- sim.pus(225L)
#'
#' # simulate 1 uniform species distribution using SpatRaster
#' s1 <- sim.species(blank.raster(sim_pus, 1), n = 1, model = "uniform")
#'
#' # simulate 1 uniform species distribution based on sf
#' s2 <- sim.species(sim_pus, res = 1, n = 1, model = "uniform")
#'
#' # simulate 1 normal species distributions
#' s3 <- sim.species(sim_pus, res = 1, n = 1, model = "normal")
#'
#' # simulate 1 bimodal species distribution
#' s4 <- sim.species(sim_pus, res = 1, n = 1, model = "bimodal")
#'
#' # simulate 1 species distribution using a random field
#' s5 <- sim.species(sim_pus, res = 1, n = 1, model = 0.2)
#'
#' # plot simulations
#' par(mfrow = c(2,2))
#' plot(s2, main = "constant")
#' plot(s3, main = "normal")
#' plot(s4, main = "bimodal")
#' plot(s5, main = "random field")
#' }
#' @export sim.species
sim.species <- function(x, ...) UseMethod("sim.species")

#' @rdname sim.species
#'
#' @method sim.species SpatRaster
#'
#' @export
sim.species.SpatRaster <- function(x, n = 1, model = "normal", ...) {
  assertthat::assert_that(
    inherits(x, "SpatRaster"),
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
    for (i in seq_len(terra::nlyr(x))) {
      ret[[i]] <- terra::setValues(
        x[[i]],
        boot::inv.logit(terra::values(x[[i]])[[1]])
      )
    }
  } else {
    assertthat::assert_that(
      assertthat::is.string(model),
      assertthat::noNA(model)
    )
    assertthat::assert_that(
      model %in% c("uniform", "normal", "bimodal")
    )
    x_coords <- terra::as.data.frame(x, xy = TRUE)[, c("x", "y"), drop = FALSE]
    for (i in seq_len(n)) {
      ret[[i]] <- terra::setValues(
        x,
        do.call(
          paste0(model, "_niche"),
          args = list(x = x_coords[, 1], y = x_coords[, 2])
        )
      )
    }
  }
  # return object
  terra::rast(ret)
}

#' @rdname sim.species
#'
#' @method sim.species SpatialPolygons
#'
#' @export
sim.species.SpatialPolygons <- function(x, res, n = 1, model = "normal", ...) {
  .Defunct(
    msg = paste(
      "support for sp::SpatialPolygons data has been deprecated,",
      "use sf::st_as_sf() to convert to an sf::st_sf() object and try again"
    )
  )
}

#' @rdname sim.species
#'
#' @method sim.species sf
#'
#' @export
sim.species.sf <- function(x, res, n = 1, model = "normal", ...) {
  sim.species.SpatRaster(x = blank.raster(x, res), n = n, model = model, ...)
}
