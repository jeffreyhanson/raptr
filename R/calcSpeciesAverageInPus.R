#' @include RcppExports.R
NULL

#' Calculate average value for species data in planning units
#'
#' This function calculates the average of species values in each planning unit.
#' By default all polygons will be treated as having separate ids.
#'
#' @param x [sf::st_as_sf()], [sp::SpatialPolygons()] or
#'   [sp::SpatialPolygonsDataFrame()] object.
#'
#' @param y [terra::rast()] object.
#'
#' @param ids `integer` vector of ids. Defaults to indices of layers in
#'   argument to `y`.
#'
#' @param field `integer` index or `character` name of column with
#'   planning unit ids. Valid only when `x` is a
#'   [sf::st_sf()] or [sp::SpatialPolygonsDataFrame()] object.
#'   Default behavior is to treat each polygon as a different planning unit.
#'
#' @param ... not used.
#'
#' @section Note:
#' Although earlier versions of the package had an additional `ncores`
#' parameter, this parameter has been deprecated.
#'
#' @return A [base::data.frame()] object.
#'
#' @examples
#' # simulate data
#' sim_pus <- sim.pus(225L)
#' sim_spp <- terra::rast(
#'   lapply(c("uniform", "normal", "bimodal"),
#'          sim.species, n = 1, res = 1, x = sim_pus)
#' )
#'
#' # calculate average for 1 species
#' puvspr1.dat <- calcSpeciesAverageInPus(sim_pus, sim_spp[[1]])
#'
#' # calculate average for multiple species
#' puvspr2.dat <- calcSpeciesAverageInPus(sim_pus, sim_spp)
#'
#' @export
calcSpeciesAverageInPus <- function(x, ...) UseMethod("calcSpeciesAverageInPus")

#' @method calcSpeciesAverageInPus SpatialPolygons
#'
#' @rdname calcSpeciesAverageInPus
#'
#' @export
calcSpeciesAverageInPus.SpatialPolygons <- function(
  x, y, ids = seq_len(terra::nlyr(y)), ...) {
  # check for invalid inputs
  assertthat::assert_that(
    inherits(x, "SpatialPolygons"),
    inherits(y, "SpatRaster"),
    terra::nlyr(y) == length(ids),
    sum(duplicated(ids)) == 0
  )
  # run calculations
  calcSpeciesAverageInPus.SpatialPolygonsDataFrame(
    x = sp::SpatialPolygonsDataFrame(
      x@polygons,
      data = data.frame(
        id = seq_len(nrow(x@data)),
        row.names = sapply(x@polygons, methods::slot, name = "ID")
      )
    ),
    y = y,
    ids = ids,
    field = "id"
  )
}

#' @method calcSpeciesAverageInPus SpatialPolygonsDataFrame
#'
#' @rdname calcSpeciesAverageInPus
#'
#' @export
calcSpeciesAverageInPus.SpatialPolygonsDataFrame <- function(
  x, y, ids = seq_len(terra::nlyr(y)), field = NULL, ...) {
  calcSpeciesAverageInPus.sf(
    x = sf::st_as_sf(x),
    y = y,
    ids = ids,
    field = "id"
  )
}

#' @method calcSpeciesAverageInPus sf
#'
#' @rdname calcSpeciesAverageInPus
#'
#' @export
calcSpeciesAverageInPus.sf <- function(
  x, y, ids = seq_len(terra::nlyr(y)), field = NULL, ...) {
  # check for invalid inputs
  assertthat::assert_that(
    inherits(x, "sf"),
    inherits(y, "SpatRaster"),
    terra::nlyr(y) == length(ids),
    identical(anyDuplicated(ids), 0L),
    assertthat::is.string(field) || is.null(field)
  )
  # prepare attribute table
  if (is.null(field)) {
    x$id <- seq_len(nrow(x))
  } else {
    x$id <- x[[field]]
  }
  # generate raster layer with polygons
  x <- terra::rasterize(
    methods::as(x, "SpatVector"), y, method = "ngb", field = "id"
  )
  # main processing
  zonalMean(x, y, ids)
}
