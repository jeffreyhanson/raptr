#' @include RcppExports.R
NULL

#' @method calcSpeciesAverageInPus SpatialPolygons
#'
#' @rdname calcSpeciesAverageInPus
#'
#' @export
calcSpeciesAverageInPus.SpatialPolygons <- function(x, y,
                                                    ids = seq_len(nlayers(y)),
                                                    ncores = 1,
                                                    ...) {
  # check for invalid inputs
  assertthat::assert_that(inherits(x, "SpatialPolygons"), inherits(y, "Raster"),
                          raster::nlayers(y) == length(ids),
                          sum(duplicated(ids)) == 0,
                          assertthat::is.count(ncores))
  return(calcSpeciesAverageInPus.SpatialPolygonsDataFrame(
    x = SpatialPolygonsDataFrame(x@polygons,
                                 data = data.frame(id = seq_len(nrow(x@data)),
                                                   row.names =
                                                     sapply(x@polygons,
                                                            methods::slot,
                                                            name = "ID"))),
    y = y, ids = ids, ncores = ncores, field = "id"))
}

#' @method calcSpeciesAverageInPus SpatialPolygonsDataFrame
#'
#' @rdname calcSpeciesAverageInPus
#'
#' @export
calcSpeciesAverageInPus.SpatialPolygonsDataFrame <- function(
  x, y, ids = seq_len(nlayers(y)), ncores = 1, field = NULL, ...) {
  # check for invalid inputs
  assertthat::assert_that(inherits(x, "SpatialPolygonsDataFrame"),
                          inherits(y, "Raster"),
                          raster::nlayers(y) == length(ids),
                          sum(duplicated(ids)) == 0,
                          assertthat::is.count(ncores),
                          assertthat::is.string(field) || is.null(field))
  # prepare attribute table
  if (is.null(field)) {
    x@data <- data.frame(id = seq_len(nrow(x@data)),
                         row.names = row.names(x@data))
  } else {
    x@data <- data.frame(id = x@data[[field]], row.names = row.names(x@data))
  }
  # generate raster layer with polygons
  x <- raster::rasterize(x, y[[1]], method = "ngb")
  # main processing
  return(zonalMean(x, y, ids, ncores))
}
