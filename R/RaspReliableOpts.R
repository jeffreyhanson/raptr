#' @include RcppExports.R raspr-internal.R misc.R
NULL

#' RaspReliableOpts: An S4 class to represent input parameters for the reliable formulation of RASP.
#'
#' This class is used to store input parameters for the reliable formulation of RASP.
#'
#' @slot BLM \code{numeric} boundary length modifier. Defaults to 0.
#' @slot FAILUREMULTIPLIER \code{numeric} multiplier for failure planning unit. Defaults to 1.1.
#' @slot MAXRLEVEL \code{numeric} maximum R failure level for approximation. Defaults to 5L.
#' @slot NUMREPS \code{integer} number of replicate runs. Defaults to 1L.
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
update.RaspReliableOpts<-function(object, ..., ignore.extra=FALSE) {
	# deparse arguments
	params<-as.list(substitute(list(...)))[-1L]
	if (!ignore.extra & any(!names(params) %in% slotNames('RaspReliableOpts')))
		stop(
			paste0(
				paste(names(params)[!names(params) %in% slotNames('RaspReliableOpts')], collapse=', '),
				' is not a slot(s) in RaspReliableOpts'
			)
		)
	params<-params[which(names(params) %in% slotNames('RaspReliableOpts'))]
	# update parameters
	for (i in seq_along(params))
		slot(object, names(params)[i]) <- eval(params[[i]])
	# check object for validity
	validObject(object, test=FALSE)
	# return object
	return(object)
}
