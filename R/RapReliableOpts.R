#' @include RcppExports.R raptr-internal.R misc.R
NULL

#' RapReliableOpts: An S4 class to represent input parameters for the reliable formulation of RAP.
#'
#' This class is used to store input parameters for the reliable formulation of RAP.
#'
#' @slot BLM \code{numeric} boundary length modifier. Defaults to 0.
#' @slot failure.multiplier \code{numeric} multiplier for failure planning unit. Defaults to 1.1.
#' @slot max.r.level \code{numeric} maximum R failure level for approximation. Defaults to 5L.
#' @seealso \code{\link{RapReliableOpts}}
#' @export
setClass("RapReliableOpts",
	representation(
		failure.multiplier="numeric",
		max.r.level="integer"
	),
	prototype=list(
		failure.multiplier=1.1,
		max.r.level=5L
	),
	contains='RapOpts',
	validity=function(object) {
		# failure.multiplier
		if (!is.numeric(object@failure.multiplier)) stop('argument to failure.multiplier is not numeric')
		if (!is.finite(object@failure.multiplier)) stop('argument to failure.multiplier is NA or non-finite value')

		# BLM
		if (!is.numeric(object@BLM)) stop('argument to BLM is not numeric')
		if (!is.finite(object@BLM)) stop('argument to BLM is NA or non-finite value')

		# max.r.level
		if (!is.integer(object@max.r.level)) stop('argument to max.r.level is not integer')
		if (!is.finite(object@max.r.level)) stop('argument to max.r.level is NA or non-finite value')
		return(TRUE)
	}
)


#' Create RapReliableOpts object
#'
#' This function creates a new RapReliableOpts object.
#'
#' @param BLM \code{numeric} boundary length modifier. Defaults to 0.
#' @param failure.multiplier \code{numeric} multiplier for failure planning unit. Defaults to 1.1.
#' @param max.r.level \code{numeric} maximum R failure level for approximation. Defaults to 5L.
#' @return \code{RapReliableOpts} object
#' @seealso \code{\link{RapReliableOpts-class}}.
#' @export
#' @examples
#' # create RapReliableOpts using defaults
#' RapReliableOpts(BLM=0, failure.multiplier=1.1, max.r.level=5L)
#' @export
RapReliableOpts<-function(BLM=0, failure.multiplier=1.1, max.r.level=5L) {
	ro<-new("RapReliableOpts", BLM=BLM, failure.multiplier=failure.multiplier, max.r.level=max.r.level)
	validObject(ro, test=FALSE)
	return(ro)
}

#' @method print RapReliableOpts
#' @rdname print
#' @export
print.RapReliableOpts <- function(x, ..., header=TRUE) {
	if (header)
		cat("RapReliableOpts object.\n")
	cat('  BLM:',x@BLM,'\n')
	cat('  failure.multiplier:',x@failure.multiplier,'\n')
	cat('  max.r.level:',x@max.r.level,'\n')
}

#' @rdname show
#' @export
setMethod(
	'show',
	'RapReliableOpts',
	function(object)
		print.RapReliableOpts(object)
)

#' @rdname update
#' @method update RapReliableOpts
#' @export
update.RapReliableOpts<-function(object, BLM=NULL, failure.multiplier=NULL, max.r.level=NULL, ...) {
	# update params
	if (!is.null(BLM))
		object@BLM<-BLM
	if (!is.null(failure.multiplier))
		object@failure.multiplier<-failure.multiplier
	if (!is.null(max.r.level))
		object@max.r.level<-max.r.level
	# check object for validity
	validObject(object, test=FALSE)
	# return object
	return(object)
}
