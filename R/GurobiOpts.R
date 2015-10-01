 #' @include RcppExports.R raspr-internal.R misc.R
NULL

#' GurobiOpts: An S4 class to represent Gurobi parameters
#'
#' This class is used to store Gurobi input parameters.
#'
#' @slot NTHREADS \code{integer} number of cores to use for processing. Defaults to 1L.
#' @slot GAP \code{numeric} MIP gap (lp_solve parameter). Defaults to 0.05.
#' @slot PRESOLVE \code{integer} code for level of computation in presolve (lp_solve parameter). Defaults to 2.
#' @slot TIMELIMIT \code{integer} number of seconds to allow for solving. Defaults to NA_integer_, and so a time limit is not imposed.
#' @slot VERBOSITY \code{integer} code for amount of output to display. Defaults to 1L.
#' @export
setClass("GurobiOpts",
	representation(
		NTHREADS="integer",
		GAP="numeric",
		PRESOLVE="integer",
		TIMELIMIT="integer",
		VERBOSITY="integer"
	),
	prototype=list(
		NTHREADS=1L,
		GAP=0.05,
		PRESOLVE=2L,
		TIMELIMIT=NA_integer_,
		VERBOSITY=1L
	),
	validity=function(object) {
		
		# TIMELIMIT
		if (!is.integer(object@TIMELIMIT)) stop('argument to TIMELIMIT is not integer')

		# NTHREADS
		if (!is.integer(object@NTHREADS)) stop('argument to NTHREADS is not numeric')
		if (!is.finite(object@NTHREADS)) stop('argument to NTHREADS is NA or non-finite values')

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
	"GurobiOpts", 
	function(.Object, NTHREADS, GAP, PRESOLVE, TIMELIMIT, VERBOSITY) {
		callNextMethod(.Object, NTHREADS=NTHREADS, PRESOLVE=PRESOLVE, TIMELIMIT=TIMELIMIT, VERBOSITY=VERBOSITY)
	}
)


#' Create GurobiOpts object
#'
#' This function creates a new GurobiOpts object.
#'
#' @param NTHREADS \code{integer} number of cores to use for processing. Defaults to 1L.
#' @param GAP \code{numeric} MIP gap (lp_solve parameter). Defaults to 0.05.
#' @param PRESOLVE \code{integer} code for level of computation in presolve (lp_solve parameter). Defaults to 2.
#' @param TIMELIMIT \code{integer} number of seconds to allow for solving. Defaults to NA_integer_, and so a time limit is not imposed.
#' @param VERBOSITY \code{integer} code for amount of output to display. Defaults to 1L.
#' @return \code{GurobiOpts} object
#' @seealso \code{\link{GurobiOpts-class}}.
#' @export
#' @examples
#' x<-GurobiOpts(NTHREADS=2)
#' @export
GurobiOpts<-function(NTHREADS=1L, GAP=0.05, PRESOLVE=2L, TIMELIMIT=NA_integer_, VERBOSITY=1L) {
	go<-new("GurobiOpts", NTHREADS=NTHREADS, PRESOLVE=PRESOLVE, TIMELIMIT=TIMELIMIT, VERBOSITY=VERBOSITY)
	validObject(go, test=FALSE)
	return(go)
}


