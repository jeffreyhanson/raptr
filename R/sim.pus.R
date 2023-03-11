#' @include RcppExports.R raptr-internal.R generics.R
NULL

#' Simulate planning units
#'
#' This function simulates planning units for RAP.
#'
#' @param n `integer` number of planning units. Note `sqrt(n)` must yield
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
#' @return [sf::st_as_sf()] with planning units.
#'
#' @details Square planning units are generated in the shape of a square.
#'   Default coordinate arguments are such that the planning units will be
#'   centered at origin. The data slot contains an "id" (`integer`),
#'   "cost" (`numeric`), "status" (`integer`), and "area"
#'   (`numeric`).
#'
#' @examples
#' \dontrun{
#' # generate 225 square planning units arranged in a square
#' # with 1 unit height / width
#' x <- sim.pus(225)
#'
#' # generate 225 rectangular pus arranged in a square
#' y <- sim.pus(225, xmn = -5, xmx = 10, ymn = -5, ymx = 5)
#' par(mfrow = c(1, 2))
#' plot(x, main = "x")
#' plot(y, main = "y")
#' par(mfrow = c(1, 1))
#' }
#' @export sim.pus
sim.pus <- function(n, xmn = -sqrt(n) / 2, xmx = sqrt(n) / 2,
                    ymn = -sqrt(n) / 2, ymx = sqrt(n) / 2) {
  # validate inputs
  assertthat::assert_that(
    assertthat::is.scalar(n),
    assertthat::is.scalar(xmn),
    assertthat::is.scalar(xmx),
    assertthat::is.scalar(ymn),
    assertthat::is.scalar(ymx),
    is.finite(n),
    is.finite(xmn),
    is.finite(xmx),
    is.finite(ymn),
    is.finite(ymx)
  )
  # check n has valid square root
  assertthat::assert_that(
    sqrt(n) == ceiling(sqrt(n)),
    msg = "sqrt(n) must yield a whole number"
  )
  # create raster
  rst <- terra::rast(
    nrow = sqrt(n), ncol = sqrt(n),
    xmin = xmn, xmax = xmx, ymin = ymn, ymax = ymx
  )
  rst <- terra::setValues(rst, seq_len(n))
  # convert to sf object
  ret <- sf::st_as_sf(terra::as.polygons(rst))
  # insert default values
  ret$cost <- 1
  ret$status <- 0L
  ret$area <- prod(terra::res(rst))
  ret <- ret[, c("cost", "status", "area"), drop = FALSE]
  # return polygons
  ret
}
