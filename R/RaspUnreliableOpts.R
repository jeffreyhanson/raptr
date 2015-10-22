#' @include RcppExports.R raspr-internal.R misc.R
NULL

#' RaspUnreliableOpts: An S4 class to represent parameters for the unreliable RASP problem
#'
#' This class is used to store input parameters for the unreliable RASP problem formulation.
#'
#' @slot BLM \code{numeric} boundary length modifier. Defaults to 0.
#' @export
setClass("RaspUnreliableOpts",
	contains='RaspOpts',
	validity=function(object) {
		# BLM
		if (!is.numeric(object@BLM)) stop('argument to BLM is not numeric')
		if (!is.finite(object@BLM)) stop('argument to BLM is NA or non-finite value')
		return(TRUE)
	}
)


#' Create RaspUnreliableOpts object
#'
#' This function creates a new RaspUnreliableOpts object.
#'
#' @param BLM \code{numeric} boundary length modifier. Defaults to 0.
#' @return \code{RaspUnreliableOpts} object
#' @seealso \code{\link{RaspUnreliableOpts-class}}.
#' @export
#' @examples
#' # create RaspUnreliableOpts using defaults
#' RaspUnreliableOpts(BLM=0)
#' @export
RaspUnreliableOpts<-function(BLM=0) {
	ro<-new("RaspUnreliableOpts", BLM=BLM)
	validObject(ro, test=FALSE)
	return(ro)
}

#' @method print RaspUnreliableOpts
#' @rdname print
#' @export
print.RaspUnreliableOpts=function(x, ..., header=TRUE) {
	if (header)
		cat("RaspUnreliableOpts object.\n")
	cat('  BLM:',x@BLM,'\n')
}

#' @describeIn show
#' @export
setMethod(
	'show',
	'RaspUnreliableOpts',
	function(object)
		print.RaspUnreliableOpts(object)
)

#' @rdname update
#' @method update RaspUnreliableOpts
#' @export
update.RaspUnreliableOpts<-function(object, ..., ignore.extra=FALSE) {
	# deparse arguments
	params<-as.list(substitute(list(...)))[-1L]
	if (!ignore.extra & any(!names(params) %in% slotNames('RaspUnreliableOpts')))
		stop(
			paste0(
					paste(names(params)[!names(params) %in% slotNames('RaspUnreliableOpts')], collapse=', '),
				' is not a slot(s) in RaspUnreliableOpts'
			)
		)
	params<-params[which(names(params) %in% slotNames('RaspUnreliableOpts'))]
	# update parameters
	for (i in seq_along(params))
		slot(object, names(params)[i]) <- eval(params[[i]])
	# check object for validity
	validObject(object, test=FALSE)
	# return object
	return(object)
}
