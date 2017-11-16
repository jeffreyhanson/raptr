#' Proportion held using unreliable RAP formulation.
#'
#' This is a convenience function to quickly calculate the proportion of
#' variation that one set of points captures in a another set of points using
#' the unreliable formulation.
#'
#' @param x \code{\link[base]{matrix}} of points
#'
#' @param y \code{\link[base]{matrix}} of points
#'
#' @param y.weights \code{numeric} vector of weights for each point in
#'   \code{y}. Defaults to equal weights for all points in \code{y}.
#'
#' @return \code{numeric} value indicating the proportion of variation that
#'   \code{x} explains in \code{y}
#'
#' @examples
#' urap.proportion.held(as.matrix(iris[1:2,-5]), as.matrix(iris[1:5,-5]))
#'
#' @export
urap.proportion.held <- function(x, y, y.weights = rep(1, nrow(y))) {
  # data integrity checks
  assertthat::assert_that(inherits(x, "matrix"),
                          inherits(y, "matrix"),
                          is.numeric(y.weights),
                          nrow(y) == length(y.weights),
                          ncol(x) == ncol(y),
                          all(is.finite(c(x))),
                          all(is.finite(c(y))),
                          all(is.finite(c(y.weights))),
                          all(y.weights >= 0),
                          nrow(x) >= 1,
                          nrow(y) >= 1)
  rcpp_proportion_held(x, y, y.weights)
}
