#' Proportion held using unrelible RAP formulation.
#'
#' This is a convenience function to quickly calculate the proportion of variation that one set of points captures
#' in a another set of points using the unreliable formulation.
#' @param x \code{matrix} of points
#' @param y \code{matrix} of points
#' @param y.weights \code{numeric} vector of weights for each point in \code{y}. Defaults to equal weights for all points in \code{y}.
#' @return \code{numeric} value indicating the proportion of variation that \code{x} explains in \code{y}
#' @export
#' @examples
#' proportion.held(iris[1:2,-5], iris[1:5,-5])
urap.proportion.held <- function(x, y, y.weights=rep(1, nrow(y))) {
	x <- as.matrix(x)
	y <- as.matrix(y)
	y.weights <- as.numeric(y.weights)
	print(y.weights)
	stopifnot(ncol(x)==ncol(x))
	stopifnot(nrow(y)==length(y.weights))
	rcpp_proportion_held(x, y, y.weights)
}
