#' @include RcppExports.R raspr-internal.R misc.R
NULL

#' RaspOpts: An S4 class to represent RASP input parameters
#'
#' This class is used to store RASP input parameters.
#'
#' @slot BLM \code{numeric} boundary length modifier. Defaults to 0.
#' @slot MAXRLEVEL \code{numeric} maximum R failure level for approximation. Defaults to 5L.
#' @slot NUMREPS \code{integer} number of replicate runs. Defaults to 1L.
#' @export
setClass("RaspOpts",
	representation(
		BLM="numeric",
		MAXRLEVEL="integer",
		NUMREPS="integer"
	),
	prototype=list(
		BLM=0,
		MAXRLEVEL=5L,
		NUMREPS=1L
	),
	validity=function(object) {
		# BLM
		if (!is.numeric(object@BLM)) stop('argument to BLM is not numeric')
		if (!is.finite(object@BLM)) stop('argument to BLM is NA or non-finite values')

		# MAXRLEVEL
		if (!is.numeric(object@MAXRLEVEL)) stop('argument to BLM is not numeric')
		if (!is.finite(object@MAXRLEVEL)) stop('argument to BLM is NA or non-finite values')
				
		# NUMREPS
		if (!is.integer(object@NUMREPS)) stop('argument to NUMREPS is not numeric')
		if (!is.finite(object@NUMREPS)) stop('argument to NUMREPS is NA or non-finite values')
		
		return(TRUE)
	}
)

setMethod(
	"initialize", 
	"RaspOpts", 
	function(.Object, BLM, MAXRLEVEL, NUMREPS) {
		callNextMethod(.Object, BLM=BLM, MAXRLEVEL=MAXRLEVEL, NUMREPS=NUMREPS)
	}
)


#' Create RaspOpts object
#'
#' This function creates a new RaspOpts object.
#'
#' @param BLM \code{numeric} boundary length modifier. Defaults to 0.
#' @param MAXRLEVEL \code{numeric} maximum R failure level for approximation. Defaults to 5L.
#' @param NUMREPS \code{integer} number of replicate runs. Defaults to 1L.
#' @return \code{MarxanOpts} object
#' @seealso \code{\link{RaspOpts-class}}.
#' @export
#' @examples
#' x<-RaspOpts(NTHREADS=2, NUMREPS=2)
#' @export
RaspOpts<-function(BLM=0, MAXRLEVEL=5L, NUMREPS=1L) {
	ro<-new("RaspOpts", BLM=BLM, MAXRLEVEL=MAXRLEVEL, NUMREPS=NUMREPS)
	validObject(ro, test=FALSE)
	return(ro)
}



