#' Proportion held using reliable RAP formulation.
#'
#' This is a convenience function to quickly calculate the proportion of
#' variation that one set of points captures in a another set of points using
#' the reliable formulation.
#'
#' @param pu.coordinates [base::matrix()] of planning unit
#'   coordinates.
#'
#' @param pu.probabilities `numeric` vector of planning unit probabilities.
#'
#' @param dp.coordinates [base::matrix()] of demand point coordinates.
#'
#' @param dp.weights `numeric` vector of demand point weights.
#'
#' @param failure.distance `numeric` indicating the cost of the failure
#'   planning unit.
#'
#' @param maximum.r.level `integer` maximum failure (R) level to use for
#'   calculations.
#'
#' @return `numeric` value indicating the proportion of variation that the
#'   demand points explain in the planning units
#'
#' @examples
#' rrap.proportion.held(as.matrix(iris[1:2,-5]), runif(1:2),
#'                      as.matrix(iris[1:5,-5]), runif(1:5), 10)
#'
#' @export
rrap.proportion.held <- function(pu.coordinates, pu.probabilities,
                                 dp.coordinates, dp.weights, failure.distance,
                                 maximum.r.level =
                                   as.integer(length(pu.probabilities))) {
  # data integreity checks
  assertthat::assert_that(inherits(pu.coordinates, "matrix"),
                          is.numeric(pu.probabilities),
                          inherits(dp.coordinates, "matrix"),
                          is.numeric(dp.weights),
                          assertthat::is.scalar(failure.distance),
                          assertthat::is.count(maximum.r.level),
                          nrow(pu.coordinates) == length(pu.probabilities),
                          nrow(dp.coordinates) == length(dp.weights),
                          ncol(dp.coordinates) == ncol(pu.coordinates),
                          all(is.finite(c(dp.weights))),
                          all(is.finite(c(pu.probabilities))),
                          all(is.finite(c(pu.coordinates))),
                          all(is.finite(c(dp.coordinates))),
                          all(is.finite(c(failure.distance))),
                          all(is.finite(c(maximum.r.level))),
                          maximum.r.level <= nrow(pu.coordinates),
                          failure.distance >=  0,
                          nrow(pu.coordinates) >= 1,
                          nrow(dp.coordinates) >= 1)
  # main processing
  rcpp_rrap_proportion_held(pu.coordinates, pu.probabilities, dp.coordinates,
                            dp.weights, failure.distance, maximum.r.level)
}
