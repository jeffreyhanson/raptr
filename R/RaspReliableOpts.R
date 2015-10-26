#' @include RcppExports.R raspr-internal.R misc.R
NULL

#' RaspReliableOpts: An S4 class to represent input parameters for the reliable formulation of RASP.
#'
#' This class is used to store input parameters for the reliable formulation of RASP.
#'
#' @slot BLM \code{numeric} boundary length modifier. Defaults to 0.
#' @slot FAILUREMULTIPLIER \code{numeric} multiplier for failure planning unit. Defaults to 1.1.
#' @slot MAXRLEVEL \code{numeric} maximum R failure level for approximation. Defaults to 5L.
#' @export
setClass("RaspReliableOpts",
	representation(
		FAILUREMULTIPLIER="numeric",
		MAXRLEVEL="integer"
	),
	prototype=list(
		FAILUREMULTIPLIER=1.1,
		MAXRLEVEL=5L
	),
	contains='RaspOpts',
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


#' Create RaspReliableOpts object
#'
#' This function creates a new RaspReliableOpts object.
#'
#' @param BLM \code{numeric} boundary length modifier. Defaults to 0.
#' @param FAILUREMULTIPLIER \code{numeric} multiplier for failure planning unit. Defaults to 1.1.
#' @param MAXRLEVEL \code{numeric} maximum R failure level for approximation. Defaults to 5L.
#' @return \code{RaspReliableOpts} object
#' @seealso \code{\link{RaspReliableOpts-class}}.
#' @export
#' @examples
#' # create RaspReliableOpts using defaults
#' RaspReliableOpts(BLM=0, FAILUREMULTIPLIER=1.1, MAXRLEVEL=5L)
#' @export
RaspReliableOpts<-function(BLM=0, FAILUREMULTIPLIER=1.1, MAXRLEVEL=5L) {
	ro<-new("RaspReliableOpts", BLM=BLM, FAILUREMULTIPLIER=FAILUREMULTIPLIER, MAXRLEVEL=MAXRLEVEL)
	validObject(ro, test=FALSE)
	return(ro)
}

#' @method print RaspReliableOpts
#' @rdname print
#' @export
print.RaspReliableOpts=function(x, ..., header=TRUE) {
	if (header)
		cat("RaspReliableOpts object.\n")
	cat('  BLM:',x@BLM,'\n')
	cat('  FAILUREMULTIPLIER:',x@FAILUREMULTIPLIER,'\n')
	cat('  MAXRLEVEL:',x@MAXRLEVEL,'\n')
}

#' @describeIn show
#' @export
setMethod(
	'show',
	'RaspReliableOpts',
	function(object)
		print.RaspReliableOpts(object)
)

#' @rdname update
#' @method update RaspReliableOpts
#' @export
update.RaspReliableOpts<-function(object, BLM=NULL, FAILUREMULTIPLIER=NULL, MAXRLEVEL=NULL) {
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
