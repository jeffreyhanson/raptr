#' @include RcppExports.R raspr-internal.R misc.R
NULL

#' RaspOpts: An S4 class to represent RASP input parameters
#'
#' This class is used to store RASP input parameters.
#'
#' @slot BLM \code{numeric} boundary length modifier. Defaults to 0.
#' @slot FAILUREMULTIPLIER \code{numeric} multiplier for failure planning unit. Defaults to 1000.
#' @slot MAXRLEVEL \code{numeric} maximum R failure level for approximation. Defaults to 5L.
#' @slot NUMREPS \code{integer} number of replicate runs. Defaults to 1L.
#' @export
setClass("RaspOpts",
	representation(
		BLM="numeric",
		FAILUREMULTIPLIER="numeric",
		MAXRLEVEL="integer",
		NUMREPS="integer"
	),
	prototype=list(
		BLM=0,
		FAILUREMULTIPLIER=1000,
		MAXRLEVEL=5L,
		NUMREPS=1L
	),
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
		
		# NUMREPS
		if (!is.integer(object@NUMREPS)) stop('argument to NUMREPS is not numeric')
		if (!is.finite(object@NUMREPS)) stop('argument to NUMREPS is NA or non-finite values')
		
		return(TRUE)
	}
)


#' Create RaspOpts object
#'
#' This function creates a new RaspOpts object.
#'
#' @param BLM \code{numeric} boundary length modifier. Defaults to 0.
#' @param FAILUREMULTIPLIER \code{numeric} multiplier for failure planning unit. Defaults to 1000.
#' @param MAXRLEVEL \code{numeric} maximum R failure level for approximation. Defaults to 5L.
#' @param NUMREPS \code{integer} number of replicate runs. Defaults to 1L.
#' @return \code{MarxanOpts} object
#' @seealso \code{\link{RaspOpts-class}}.
#' @export
#' @examples
#' x<-RaspOpts(BLM=1, NUMREPS=1L)
#' @export
RaspOpts<-function(BLM=0, FAILUREMULTIPLIER=1000, MAXRLEVEL=5L, NUMREPS=1L) {
	ro<-new("RaspOpts", BLM=BLM, FAILUREMULTIPLIER=FAILUREMULTIPLIER, MAXRLEVEL=MAXRLEVEL, NUMREPS=NUMREPS)
	validObject(ro, test=FALSE)
	return(ro)
}

#' @method print RaspOpts
#' @rdname print
#' @export
print.RaspOpts=function(x, ..., header=TRUE) {
	if (header)
		cat("RaspOpts object.\n")
	cat('  BLM:',x@BLM,'\n')
	cat('  FAILUREMULTIPLIER:',x@FAILUREMULTIPLIER,'\n')
	cat('  MAXRLEVEL:',x@MAXRLEVEL,'\n')
	cat('  NUMREPS:',x@NUMREPS,'\n')
}

#' @describeIn show
#' @export
setMethod(
	'show',
	'RaspOpts',
	function(object)
		print.RaspOpts(object)
)

