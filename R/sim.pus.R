#' @include RcppExports.R raptr-internal.R generics.R
NULL

#' Simulate planning units
#'
#' This function simulates planning units for RAP.
#'
#' @param n `integer` number of planning units. `sqrt(n)` must yield
#'   a valid number.
#'
#' @param xmn `numeric` value for minimum x-coordinate.
#'
#' @param xmx `numeric` value for maximum x-coordinate.
#'
#' @param ymn `numeric` value for minimum y-coordinate.
#'
#' @param ymx `numeric` value for maximum y-coordinate.
#'
#' @return [sp::SpatialPolygons()] with planning units.
#'
#' @details Square planning units are generated in the shape of a square.
#'   Default coordinate arguments are such that the planning units will be
#'   centered at origin. The data slot contains an "id" (`integer`),
#'   "cost" (`numeric`), "status" (`integer`), and "area"
#'   (`numeric`).
#'
#' @examples
#' # generate 225 sqauare planning units arranged in a square
#' # with 1 unit height / width
#' x <- sim.pus(225)
#'
#' # generate 225 rectangular pus arranged in a square
#' y <- sim.pus(225, xmn = -5, xmx = 10, ymn = -5, ymx = 5)
#' \dontrun{
#' par(mfrow = c(1, 2))
#' plot(x, main = "x")
#' plot(y, main = "y")
#' par(mfrow = c(1, 1))
#' }
#'
#' @export sim.pus
sim.pus <- function(n, xmn = -sqrt(n) / 2, xmx = sqrt(n) / 2,
                    ymn = -sqrt(n) / 2, ymx = sqrt(n) / 2) {
  # validate inputs
  assertthat::assert_that(assertthat::is.scalar(n),
                          assertthat::is.scalar(xmn),
                          assertthat::is.scalar(xmx),
                          assertthat::is.scalar(ymn),
                          assertthat::is.scalar(ymx),
                          is.finite(n), is.finite(xmn), is.finite(xmx),
                          is.finite(ymn), is.finite(ymx))
  # check n has valid square root
  assertthat::assert_that(
    sqrt(n) == ceiling(sqrt(n)),
    msg = "sqrt(n) must yield a whole number")
  # create raster
  rst <- raster::raster(nrow = sqrt(n), ncol = sqrt(n), xmn = xmn, xmx = xmx,
                        ymn = ymn, ymx = ymx)
  rst <- raster::setValues(rst, seq_len(n))
  # convert to SpatialPolygonsDataFrame
  ret <- methods::as(rst, "SpatialPolygonsDataFrame")
  # insert default values
  ret@data$cost <- 1
  ret@data$status <- 0L
  ret@data$area <- prod(raster::res(rst))
  ret@data <- ret@data[, -1, drop = FALSE]
  names(ret@data) <- c("cost", "status", "area")
  # return polygons
  return(ret)
}
