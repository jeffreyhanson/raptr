#' @include RcppExports.R raspr-internal.R misc.R
NULL

#' RaspOpts: An S4 class to represent RASP input parameters
#'
#' This class is used to store RASP input parameters.
#'
#' @slot BLM \code{numeric} boundary length modifier. Defaults to 0.
#' @slot NUMREPS \code{integer} number of replicate runs. Defaults to 1L.
#' @slot NTHREADS \code{integer} number of cores to use for processing. Defaults to 1L.
#' @slot GAP \code{numeric} MIP gap (lp_solve parameter). Defaults to 0.05.
#' @slot PRESOLVE \code{integer} code for level of computation in presolve (lp_solve parameter). Defaults to 2.
#' @slot TIMELIMIT \code{integer} number of seconds to allow for solving. Defaults to NA_integer_, and so a time limit is not imposed.
#' @slot VERBOSITY \code{integer} code for amount of output to display. Defaults to 1L.
#' @export
setClass("RaspOpts",
	representation(
		BLM="numeric",
		NUMREPS="integer",
		GAP="numeric",
		NTHREADS="integer",
		TIMELIMIT="integer",
		PRESOLVE="integer",
		VERBOSITY="integer"
	),
	prototype=list(
		BLM=0,
		NUMREPS=1L,
		NTHREADS=1L,
		GAP=0.05,
		TIMELIMIT=NA_integer_,
		PRESOLVE=2L,
		VERBOSITY=1L
	),
	validity=function(object) {
		# BLM
		if (!is.numeric(object@BLM)) stop('argument to BLM is not numeric')
		if (!is.finite(object@BLM)) stop('argument to BLM is NA or non-finite values')
		
		# NUMREPS
		if (!is.integer(object@NUMREPS)) stop('argument to NUMREPS is not numeric')
		if (!is.finite(object@NUMREPS)) stop('argument to NUMREPS is NA or non-finite values')
		
		# PRESOLVE
		if (!is.integer(object@PRESOLVE)) stop('argument to PRESOLVE is not numeric')
		if (!is.finite(object@PRESOLVE)) stop('argument to PRESOLVE is NA or non-finite values')

		# VERBOSITY
		if (!is.integer(object@VERBOSITY)) stop('argument to VERBOSITY is not numeric')
		if (!is.finite(object@VERBOSITY)) stop('argument to VERBOSITY is NA or non-finite values')

		# GAP
		if (!is.numeric(object@GAP)) stop('argument to GAP is not numeric')
		if (!is.finite(object@GAP)) stop('argument to GAP is NA or non-finite values')

		return(TRUE)
	}
)

setMethod(
	"initialize", 
	"RaspOpts", 
	function(.Object, BLM, NUMREPS, NTHREADS, GAP, PRESOLVE, TIMELIMIT, VERBOSITY) {
		callNextMethod(.Object, BLM=BLM, NUMREPS=NUMREPS, NTHREADS=NTHREADS, PRESOLVE=PRESOLVE, TIMELIMIT=TIMELIMIT, VERBOSITY=VERBOSITY)
	}
)


#' Create RaspOpts object
#'
#' This function creates a new RaspOpts object.
#'
#' @param BLM \code{numeric} boundary length modifier. Defaults to 0.
#' @param NUMREPS \code{integer} number of replicate runs. Defaults to 1L.
#' @param NTHREADS \code{integer} number of cores to use for processing. Defaults to 1L.
#' @param GAP \code{numeric} MIP gap (lp_solve parameter). Defaults to 0.05.
#' @param PRESOLVE \code{integer} code for level of computation in presolve (lp_solve parameter). Defaults to 2L.
#' @param TIMELIMIT \code{integer} number of seconds to allow for solving. Defaults to NA_integer_, and so a time limit is not imposed.
#' @param VERBOSITY \code{integer} code for amount of output to display. Defaults to 1L.
#' @return \code{MarxanOpts} object
#' @seealso \code{\link{RaspOpts-class}}.
#' @export
#' @examples
#' x<-RaspOpts(NTHREADS=2, NUMREPS=2)
#' @export
RaspOpts<-function(BLM=0, NUMREPS=1L, NTHREADS=1L, GAP=0.05, PRESOLVE=2L, TIMELIMIT=NA_integer_, VERBOSITY=1L) {
	ro<-new("RaspOpts", BLM=BLM, NUMREPS=NUMREPS, NTHREADS=NTHREADS, PRESOLVE=PRESOLVE, TIMELIMIT=TIMELIMIT, VERBOSITY=VERBOSITY)
	validObject(ro, test=FALSE)
	return(ro)
}


