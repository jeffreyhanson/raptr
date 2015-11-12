#' @include RcppExports.R rapr-internal.R misc.R
NULL

#' RapReliableOpts: An S4 class to represent input parameters for the reliable formulation of RASP.
#'
#' This class is used to store input parameters for the reliable formulation of RASP.
#'
#' @slot BLM \code{numeric} boundary length modifier. Defaults to 0.
#' @slot FailureMultiplier \code{numeric} multiplier for failure planning unit. Defaults to 1.1.
#' @slot MaxRLevel \code{numeric} maximum R failure level for approximation. Defaults to 5L.
#' @seealso \code{\link{RapReliableOpts}}
#' @export
setClass("RapReliableOpts",
	representation(
		FailureMultiplier="numeric",
		MaxRLevel="integer"
	),
	prototype=list(
		FailureMultiplier=1.1,
		MaxRLevel=5L
	),
	contains='RapOpts',
	validity=function(object) {
		# FailureMultiplier
		if (!is.numeric(object@FailureMultiplier)) stop('argument to FailureMultiplier is not numeric')
		if (!is.finite(object@FailureMultiplier)) stop('argument to FailureMultiplier is NA or non-finite value')

		# BLM
		if (!is.numeric(object@BLM)) stop('argument to BLM is not numeric')
		if (!is.finite(object@BLM)) stop('argument to BLM is NA or non-finite value')

		# MaxRLevel
		if (!is.integer(object@MaxRLevel)) stop('argument to MaxRLevel is not integer')
		if (!is.finite(object@MaxRLevel)) stop('argument to MaxRLevel is NA or non-finite value')


		return(TRUE)
	}
)


#' Create RapReliableOpts object
#'
#' This function creates a new RapReliableOpts object.
#'
#' @param BLM \code{numeric} boundary length modifier. Defaults to 0.
#' @param FailureMultiplier \code{numeric} multiplier for failure planning unit. Defaults to 1.1.
#' @param MaxRLevel \code{numeric} maximum R failure level for approximation. Defaults to 5L.
#' @return \code{RapReliableOpts} object
#' @seealso \code{\link{RapReliableOpts-class}}.
#' @export
#' @examples
#' # create RapReliableOpts using defaults
#' RapReliableOpts(BLM=0, FailureMultiplier=1.1, MaxRLevel=5L)
#' @export
RapReliableOpts<-function(BLM=0, FailureMultiplier=1.1, MaxRLevel=5L) {
	ro<-new("RapReliableOpts", BLM=BLM, FailureMultiplier=FailureMultiplier, MaxRLevel=MaxRLevel)
	validObject(ro, test=FALSE)
	return(ro)
}

#' @method print RapReliableOpts
#' @rdname print
#' @export
print.RapReliableOpts=function(x, ..., header=TRUE) {
	if (header)
		cat("RapReliableOpts object.\n")
	cat('  BLM:',x@BLM,'\n')
	cat('  FailureMultiplier:',x@FailureMultiplier,'\n')
	cat('  MaxRLevel:',x@MaxRLevel,'\n')
}

#' @describeIn show
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
update.RapReliableOpts<-function(object, BLM=NULL, FailureMultiplier=NULL, MaxRLevel=NULL, ...) {
	# update params
	if (!is.null(BLM))
		object@BLM<-BLM
	if (!is.null(FailureMultiplier))
		object@FailureMultiplier<-FailureMultiplier
	if (!is.null(MaxRLevel))
		object@MaxRLevel<-MaxRLevel
	# check object for validity
	validObject(object, test=FALSE)
	# return object
	return(object)
}
