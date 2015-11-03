#' @include RcppExports.R rapr-internal.R misc.R
NULL

#' RapReliableOpts: An S4 class to represent input parameters for the reliable formulation of RASP.
#'
#' This class is used to store input parameters for the reliable formulation of RASP.
#'
#' @slot BLM \code{numeric} boundary length modifier. Defaults to 0.
#' @slot FAILUREMULTIPLIER \code{numeric} multiplier for failure planning unit. Defaults to 1.1.
#' @slot MAXRLEVEL \code{numeric} maximum R failure level for approximation. Defaults to 5L.
#' @seealso \code{\link{RapReliableOpts}}
#' @export
setClass("RapReliableOpts",
	representation(
		FAILUREMULTIPLIER="numeric",
		MAXRLEVEL="integer"
	),
	prototype=list(
		FAILUREMULTIPLIER=1.1,
		MAXRLEVEL=5L
	),
	contains='RapOpts',
	validity=function(object) {
		# FAILUREMULTIPLIER
		if (!is.numeric(object@FAILUREMULTIPLIER)) stop('argument to FAILUREMULTIPLIER is not numeric')
		if (!is.finite(object@FAILUREMULTIPLIER)) stop('argument to FAILUREMULTIPLIER is NA or non-finite value')

		# BLM
		if (!is.numeric(object@BLM)) stop('argument to BLM is not numeric')
		if (!is.finite(object@BLM)) stop('argument to BLM is NA or non-finite value')

		# MAXRLEVEL
		if (!is.integer(object@MAXRLEVEL)) stop('argument to MAXRLEVEL is not integer')
		if (!is.finite(object@MAXRLEVEL)) stop('argument to MAXRLEVEL is NA or non-finite value')


		return(TRUE)
	}
)


#' Create RapReliableOpts object
#'
#' This function creates a new RapReliableOpts object.
#'
#' @param BLM \code{numeric} boundary length modifier. Defaults to 0.
#' @param FAILUREMULTIPLIER \code{numeric} multiplier for failure planning unit. Defaults to 1.1.
#' @param MAXRLEVEL \code{numeric} maximum R failure level for approximation. Defaults to 5L.
#' @return \code{RapReliableOpts} object
#' @seealso \code{\link{RapReliableOpts-class}}.
#' @export
#' @examples
#' # create RapReliableOpts using defaults
#' RapReliableOpts(BLM=0, FAILUREMULTIPLIER=1.1, MAXRLEVEL=5L)
#' @export
RapReliableOpts<-function(BLM=0, FAILUREMULTIPLIER=1.1, MAXRLEVEL=5L) {
	ro<-new("RapReliableOpts", BLM=BLM, FAILUREMULTIPLIER=FAILUREMULTIPLIER, MAXRLEVEL=MAXRLEVEL)
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
	cat('  FAILUREMULTIPLIER:',x@FAILUREMULTIPLIER,'\n')
	cat('  MAXRLEVEL:',x@MAXRLEVEL,'\n')
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
update.RapReliableOpts<-function(object, BLM=NULL, FAILUREMULTIPLIER=NULL, MAXRLEVEL=NULL, ...) {
	# update params
	if (!is.null(BLM))
		object@BLM<-BLM
	if (!is.null(FAILUREMULTIPLIER))
		object@FAILUREMULTIPLIER<-FAILUREMULTIPLIER
	if (!is.null(MAXRLEVEL))
		object@MAXRLEVEL<-MAXRLEVEL
	# check object for validity
	validObject(object, test=FALSE)
	# return object
	return(object)
}
