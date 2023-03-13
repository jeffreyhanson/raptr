 #' @include RcppExports.R raptr-internal.R
NULL

#' Convert object to PolySet data
#'
#' This function converts [sf::st_sf()] and
#' [sp::SpatialPolygonsDataFrame()] objects to
#' `PBSmapping::PolySet()` objects.
#'
#' @param x [sf::st_sf()], [sp::SpatialPolygons()] or
#'   [sp::SpatialPolygonsDataFrame()] object.
#'
#' @param n_preallocate `integer` How much memory should be preallocated
#'   for processing? Ideally, this number should equal the number of vertices
#'   in the [sp::SpatialPolygons()] object. If data processing is
#'   taking too long consider increasing this value.
#'
#' @return `PBSmapping::PolySet()` object.
#'
#' @note Be aware that this function is designed to be as fast as possible, but
#'   as a result it depends on C++ code and if used inappropriately this
#'   function will crash R.
#'
#' @seealso For a slower, more stable equivalent see
#'   `maptools::SpatialPolygons2PolySet`.
#'
#' @examples
#' \dontrun{
#' # generate sf object
#' sim_pus <- sim.pus(225L)
#'
#' # convert to PolySet
#' x <- convert2PolySet(sim_pus)
#' }
#' @export
convert2PolySet <- function(x, n_preallocate)
  UseMethod("convert2PolySet")

#' @rdname convert2PolySet
#'
#' @method convert2PolySet SpatialPolygonsDataFrame
#'
#' @export
convert2PolySet.SpatialPolygonsDataFrame <- function(x,
                                                     n_preallocate = 10000L) {
  rcpp_Polygons2PolySet(x@polygons)
}

#' @rdname convert2PolySet
#'
#' @method convert2PolySet SpatialPolygons
#'
#' @export
convert2PolySet.SpatialPolygons <- function(x, n_preallocate = 10000L) {
  rcpp_Polygons2PolySet(x@polygons)
}

#' @rdname convert2PolySet
#'
#' @method convert2PolySet sf
#'
#' @export
convert2PolySet.sf <- function(x, n_preallocate = 10000L) {
  convert2PolySet(sf::as_Spatial(x))
}
