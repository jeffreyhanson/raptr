#' Proportion held using reliable RAP formulation.
#'
#' This is a convenience function to quickly calculate the proportion of variation that one set of points captures
#' in a another set of points using the reliable formulation.
#' @param pu.coordinates \code{\link[base]{matrix}} of planning unit coordinates.
#' @param pu.probabilities \code{numeric} vector of planning unit probabilities.
#' @param dp.coordinates \code{\link[base]{matrix}} of demand point coordinates.
#' @param dp.weights \code{numeric} vector of demand point weights.
#' @param failure.distance \code{numeric} indicating the cost of the failure planning unit.
#' @param maximum.r.level \code{integer} maximum failure (R) level to use for calculations.
#' @return \code{numeric} value indicating the proportion of variation that the demand points explain in the planning units
#' @export
#' @examples
#' rrap.proportion.held(as.matrix(iris[1:2,-5]), runif(1:2), as.matrix(iris[1:5,-5]), runif(1:5), 10)
rrap.proportion.held <- function(pu.coordinates, pu.probabilities, dp.coordinates, dp.weights, failure.distance, maximum.r.level=as.integer(length(pu.probabilities))) {
	# data integreity checks
	expect_is(pu.coordinates, 'matrix')
	expect_is(dp.coordinates, 'matrix')
	expect_is(pu.probabilities, 'numeric')
	expect_is(dp.weights, 'numeric')
	expect_is(failure.distance, 'numeric')
	expect_is(maximum.r.level, 'integer')
	expect_length(failure.distance, 1)
	expect_length(maximum.r.level, 1)
	expect_equal(nrow(pu.coordinates), length(pu.probabilities))
	expect_equal(nrow(dp.coordinates), length(dp.weights))
	expect_equal(ncol(dp.coordinates), ncol(pu.coordinates))
	expect_true(all(is.finite(c(dp.weights))))
	expect_true(all(is.finite(c(pu.probabilities))))
	expect_true(all(is.finite(c(pu.coordinates))))
	expect_true(all(is.finite(c(dp.coordinates))))
	expect_true(all(is.finite(c(failure.distance))))
	expect_true(all(is.finite(c(maximum.r.level))))
	expect_true(maximum.r.level <= nrow(pu.coordinates))
	expect_true(maximum.r.level >=  1)
	expect_true(failure.distance >=  0)
	expect_true(nrow(pu.coordinates)>=1)
	expect_true(nrow(dp.coordinates)>=1)
	
	# main processing
	rcpp_rrap_proportion_held(pu.coordinates, pu.probabilities, dp.coordinates, dp.weights, failure.distance, maximum.r.level)
}
 
