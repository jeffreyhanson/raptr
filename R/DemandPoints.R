#' @include RcppExports.R raspr-internal.R generics.R
NULL

#' DemandPoints: An S4 class to represent demand points
#'
#' This class is used to store demand point information.
#'
#' @slot coords \code{matrix} coordinates for each demand point in the attribute space.
#' @slot weights \code{numeric} weights for each demand poont.
#' @slot species \code{integer} idenifiers for which species each demand point belongs to.
#' @seealso \code{\link{DemandPoints}}
#' @export
setClass("DemandPoints",
	representation(
		coords='matrix',
		weights='numeric',
		species='integer'
	),
	validity=function(object) {
		# coords
		if (!all(is.matrix(object@coords)))
			stop('argument to coords must be matrix')
		if (!all(is.finite(object@coords)))
			stop('argument to coords contains NA or non-finite values')
		
		# weights
		if (!all(is.numeric(object@weights)))
			stop('argument to weights must be numeric')
		if (!all(is.finite(object@weights)))
			stop('argument to weights contains NA or non-finite values')
		
		# species
		if (!all(is.integer(object@species)))
			stop('argument to species must be integer')
		if (!all(is.finite(object@species)))
			stop('argument to species contains NA or non-finite values')

		# cross-slot dependencies
		if (nrow(object@coords)!=length(object@species))
			stop('argument to coords must have have the smae number of rows as the length of species')
		if (nrow(object@coords)!=length(object@weights))
			stop('argument to coords must have have the smae number of rows as the length of weights')
	}
)

#' Create new DemandPoints object
#'
#' This funciton creates a new DemandPoints object
#'
#' @slot coords \code{matrix} coordinates for each demand point in the attribute space.
#' @slot weights \code{numeric} weights for each demand poont.
#' @slot species \code{integer} idenifiers for which species each demand point belongs to.
#' @seealso \code{\link{DemandPoints-class}}
#' @export
DemandPoints<-function(coords, weights, species) {
	# make new dp object
	dp<-new("DemandPoints", coords=coords, weights=weights, species=species)
	# test for validity
	validObject(dp, test=FALSE)
	return(dp)
}

#' Merge a list of DemandPoints objects togeather
#'
#' This is a conviencne function to merge DemandPoints objects togeather.
#'
#' @param x \code{list} of \code{DemandPoints} objects.
#' @seealso \code{DemandPoints}
#' @export
merge.DemandPoints<-function(x, ...) {
	# check all elements have same dimensions
	if (n_distinct(laply(x, function(x) {ncol(x@coords)}))!=1)
		stop('Demand points have different numbers of dimensions in the attribute space')
	# return new DemandPoints object
	return(
		DemandPoints(
			coords=do.call(rbind, llply(x, slot, 'coords')),
			species=c(laply(x, slot, 'species')),
			weights=c(laply(x, slot, 'weights'))
		)
	)
}