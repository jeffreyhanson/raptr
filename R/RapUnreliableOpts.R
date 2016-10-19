#' @include RcppExports.R raptr-internal.R misc.R
NULL

#' RapUnreliableOpts: An S4 class to represent parameters for the unreliable RAP problem
#'
#' This class is used to store input parameters for the unreliable RAP problem formulation.
#'
#' @slot BLM \code{numeric} boundary length modifier. Defaults to 0.
#' @export
setClass("RapUnreliableOpts",
	contains='RapOpts',
	validity=function(object) {
		# BLM
		if (!is.numeric(object@BLM)) stop('argument to BLM is not numeric')
		if (!is.finite(object@BLM)) stop('argument to BLM is NA or non-finite value')		
		return(TRUE)
	}
)


#' Create RapUnreliableOpts object
#'
#' This function creates a new RapUnreliableOpts object.
#'
#' @param BLM \code{numeric} boundary length modifier. Defaults to 0.
#' @return \code{RapUnreliableOpts} object
#' @seealso \code{\link{RapUnreliableOpts-class}}.
#' @export
#' @examples
#' # create RapUnreliableOpts using defaults
#' RapUnreliableOpts(BLM=0)
#' @export
RapUnreliableOpts<-function(BLM=0) {
	ro<-new("RapUnreliableOpts", BLM=BLM)
	validObject(ro, test=FALSE)
	return(ro)
}

#' @method print RapUnreliableOpts
#' @rdname print
#' @export
print.RapUnreliableOpts <- function(x, ..., header=TRUE) {
	if (header)
		cat("RapUnreliableOpts object.\n")
	cat('  BLM:',x@BLM,'\n')
}

#' @rdname show
#' @export
setMethod(
	'show',
	'RapUnreliableOpts',
	function(object)
		print.RapUnreliableOpts(object)
)

#' @rdname update
#' @method update RapUnreliableOpts
#' @export
update.RapUnreliableOpts<-function(object, BLM=NULL, ...) {
	if (!is.null(BLM))
		object@BLM<-BLM
	# check object for validity
	validObject(object, test=FALSE)
	# return object
	return(object)
}
