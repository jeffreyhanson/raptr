#' @include RcppExports.R
NULL

#' @method calcSpeciesAverageInPus SpatialPolygons
#'
#' @rdname calcSpeciesAverageInPus
#'
#' @export
calcSpeciesAverageInPus.SpatialPolygons <- function(x, y,
                                                    ids = seq_len(nlayers(y)),
                                                    ncores = 1, gdal = FALSE,
                                                    ...) {
  # check for invalid inputs
  assertthat::assert_that(inherits(x, "SpatialPolygons"), inherits(y, "Raster"),
                          raster::nlayers(y) == length(ids),
                          sum(duplicated(ids)) == 0,
                          assertthat::is.count(ncores),
                          assertthat::is.flag(gdal))
  return(calcSpeciesAverageInPus.SpatialPolygonsDataFrame(
    x = SpatialPolygonsDataFrame(x@polygons,
                                 data = data.frame(id = seq_len(nrow(x@data)),
                                                   row.names =
                                                     sapply(x@polygons,
                                                            methods::slot,
                                                            name = "ID"))),
    y = y, ids = ids, ncores = ncores, gdal = gdal, field = "id"))
}

#' @method calcSpeciesAverageInPus SpatialPolygonsDataFrame
#'
#' @rdname calcSpeciesAverageInPus
#'
#' @export
calcSpeciesAverageInPus.SpatialPolygonsDataFrame <- function(
  x, y, ids = seq_len(nlayers(y)), ncores = 1, gdal = FALSE, field = NULL,
  ...) {
  # check for invalid inputs
  assertthat::assert_that(inherits(x, "SpatialPolygonsDataFrame"),
                          inherits(y, "Raster"),
                          raster::nlayers(y) == length(ids),
                          sum(duplicated(ids)) == 0,
                          assertthat::is.count(ncores),
                          assertthat::is.flag(gdal),
                          assertthat::is.string(field) || is.null(field))
  # prepare attribute table
  if (is.null(field)) {
    x@data <- data.frame(id = seq_len(nrow(x@data)),
                         row.names = row.names(x@data))
  } else {
    x@data <- data.frame(id = x@data[[field]], row.names = row.names(x@data))
  }
  # generate raster layer with polygons
  if (gdal && is.gdalInstalled()) {
    x <- rasterizeGDAL(x, y[[1]], "id")
  } else {
    if (gdal && !is.gdalInstalled())
      warning(paste0("GDAL is not installed on this computer, using ",
                     "raster::rasterize for processing"))
    x <- raster::rasterize(x, y[[1]], method = "ngb")
  }
  # main processing
  return(zonalMean(x, y, ids, ncores))
}
