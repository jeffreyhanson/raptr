#' @include RcppExports.R raptr-internal.R
NULL

#' Calculate boundary data for planning units
#'
#' This function calculates boundary length data. Be aware that this
#' function is designed with performance in mind, and as a consequence, if this
#' function is used improperly then it may crash R. Furthermore, multipart
#' polygons with touching edges will likely result in inaccuracies.
#'
#' @param x [sf::st_sf()] or `PBSMapping::PolySet` object.
#'
#' @param tol `numeric` to specify precision of calculations. In other
#'   words, how far apart vertices have to be to be considered different?
#'
#' @param length.factor `numeric` to scale boundary lengths.
#'
#' @param edge.factor `numeric` to scale boundary lengths for edges that
#'   do not have any neighbors, such as those that occur along the margins.
#'
#' @return A `data.frame` with 'id1' (`integer`), 'id2'
#'   (`integer`), and 'amount' (`numeric`) columns.
#'
#' @seealso This function is based on the algorithm in QMARXAN
#'   <https://github.com/tsw-apropos/qmarxan> for calculating boundary
#'   length.
#'
#' @examples
#' \dontrun{
#' # simulate planning units
#' sim_pus <- sim.pus(225L)
#'
#' # calculate boundary data
#' bound.dat <- calcBoundaryData(sim_pus)
#'
#' # print summary of boundary data
#' summary(bound.dat)
#' }
#' @export
calcBoundaryData <- function(x, tol, length.factor, edge.factor) {
  UseMethod("calcBoundaryData")
}

#' @rdname calcBoundaryData
#'
#' @method calcBoundaryData PolySet
#'
#' @export
calcBoundaryData.PolySet <- function(x,
                                     tol = 0.001,
                                     length.factor = 1.0,
                                     edge.factor = 1.0) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "PolySet"),
    assertthat::is.scalar(tol),
    assertthat::is.scalar(length.factor),
    assertthat::is.scalar(edge.factor)
  )
  # calculate boundary data
  ret <- rcpp_calcBoundaryDF(
    x, tolerance = tol, lengthFactor = length.factor, edgeFactor = edge.factor
  )
  if (length(ret$warnings) != 0) {
    warning(paste0("Invalid geometries detected, see \"warnings\" attribute ",
                   "for more information."))
    attr(ret$bldf, "warnings") <- ret$warnings
  }
  # return result
  ret$bldf
}

#' @rdname calcBoundaryData
#'
#' @method calcBoundaryData SpatialPolygons
#'
#' @export
calcBoundaryData.SpatialPolygons <- function(x,
                                             tol = 0.001,
                                             length.factor = 1.0,
                                             edge.factor = 1.0) {
  .Defunct(
    msg = paste(
      "support for sp::SpatialPolygons data has been deprecated,",
      "use sf::st_as_sf() to convert to an sf::st_sf() object and try again"
    )
  )
}

#' @rdname calcBoundaryData
#'
#' @method calcBoundaryData sf
#'
#' @export
calcBoundaryData.sf <- function(x,
                                tol = 0.001,
                                length.factor = 1.0,
                                edge.factor = 1.0) {
  calcBoundaryData(
    rcpp_Polygons2PolySet(sf::as_Spatial(x)@polygons),
    tol, length.factor, edge.factor
  )
}
