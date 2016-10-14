#' Proportion held using unreliable RAP formulation.
#'
#' This is a convenience function to quickly calculate the proportion of variation that one set of points captures
#' in a another set of points using the unreliable formulation.
#' @param x \code{matrix} of points
#' @param y \code{matrix} of points
#' @param y.weights \code{numeric} vector of weights for each point in \code{y}. Defaults to equal weights for all points in \code{y}.
#' @return \code{numeric} value indicating the proportion of variation that \code{x} explains in \code{y}
#' @export
#' @examples
#' urap.proportion.held(as.matrix(iris[1:2,-5]), as.matrix(iris[1:5,-5]))
urap.proportion.held <- function(x, y, y.weights=rep(1, nrow(y))) {
	# data integreity checks
	expect_is(x, 'matrix')
	expect_is(y, 'matrix')
	expect_is(y.weights, 'numeric')
	expect_equal(nrow(y), length(y.weights))
	expect_true(all(is.finite(c(x))))
	expect_true(all(is.finite(c(y))))
	expect_true(all(is.finite(c(y.weights))))
	expect_true(all(y.weights >=  0))
	rcpp_proportion_held(x, y, y.weights)
}
