 #' @include RcppExports.R raspr-internal.R misc.R
NULL

#' GurobiOpts: An S4 class to represent Gurobi parameters
#'
#' This class is used to store Gurobi input parameters.
#'
#' @slot Threads \code{integer} number of cores to use for processing. Defaults to 1L.
#' @slot MIPGap \code{numeric} MIP gap. Defaults to 0.05.
#' @slot Presolve \code{integer} code for level of computation in presolve. Defaults to 2.
#' @slot TimeLimit \code{integer} number of seconds to allow for solving. Defaults to NA_integer_, and so a time limit is not imposed.
#' @export
setClass("GurobiOpts",
	representation(	
		Threads="integer",
		MIPGap="numeric",
		Presolve="integer",
		TimeLimit="integer"
	),
	prototype=list(
		Threads=1L,
		MIPGap=0.05,
		Presolve=2L,
		TimeLimit=NA_integer_
	),
	validity=function(object) {
		
		# TimeLimit
		if (!is.integer(object@TimeLimit)) stop('argument to TimeLimit is not integer')

		# Threads
		if (!is.integer(object@Threads)) stop('argument to Threads is not numeric')
		if (!is.finite(object@Threads)) stop('argument to Threads is NA or non-finite values')
		if (object@Threads > detectCores(logical=TRUE)) {
			warning(paste0('argument Threads greater than number of threads on the system, changing Threads to ',object@Threads))
			object@Threads<-detectCores(logcal=TRUE)
		} 

		# Presolve
		if (!is.integer(object@Presolve)) stop('argument to Presolve is not integer')
		if (!is.finite(object@Presolve)) stop('argument to Presolve is NA or non-finite values')
		if (object@Presolve < -1 || object@Presolve > 2) stop('argument to Presolve must be between -1 and 2')
		
		# MIPGap
		if (!is.numeric(object@MIPGap)) stop('argument to MIPGap is not numeric')
		if (!is.finite(object@MIPGap)) stop('argument to MIPGap is NA or non-finite values')
		if (object@MIPGap<0) stop('argument to MIPGap must be > 0')
		return(TRUE)
	}
)

#' Create GurobiOpts object
#'
#' This function creates a new GurobiOpts object.
#'
#' @param Threads \code{integer} number of cores to use for processing. Defaults to 1L.
#' @param MIPGap \code{numeric} MIP gap (lp_solve parameter). Defaults to 0.05.
#' @param Presolve \code{integer} code for level of computation in presolve (lp_solve parameter). Defaults to 2.
#' @param TimeLimit \code{integer} number of seconds to allow for solving. Defaults to NA_integer_, and so a time limit is not imposed.
#' @return \code{GurobiOpts} object
#' @seealso \code{\link{GurobiOpts-class}}.
#' @export
#' @examples
#' # create GurobiOpts object using default parameters
#' x <- GurobiOpts(Threads=1L, MIPGap=0.05, Presolve=2L, TimeLimit=NA_integer_)
#' # print object
#' print(x)
#' @export
GurobiOpts<-function(Threads=1L, MIPGap=0.05, Presolve=2L, TimeLimit=NA_integer_) {
	go<-new("GurobiOpts", Threads=Threads, MIPGap=MIPGap, Presolve=Presolve, TimeLimit=TimeLimit)
	validObject(go, test=FALSE)
	return(go)
}

#' @method print GurobiOpts
#' @rdname print
#' @export
print.GurobiOpts=function(x, ..., header=TRUE) {
	if (header)
		cat("GurobiOpts object.\n")
	cat('  Threads:',x@Threads,'\n')
	cat('  MIPGap:',x@MIPGap,'\n')
	cat('  Presolve:',x@Presolve,'\n')
	cat('  TimeLimit:',x@TimeLimit,'\n')
}

#' @describeIn show
#' @export
setMethod(
	'show',
	'GurobiOpts',
	function(object)
		print.GurobiOpts(object)
)

#' @method as.list GurobiOpts
#' @rdname as.list
#' @export
as.list.GurobiOpts<-function(x, ...) {
	y=list(	
		Threads=x@Threads,
		MIPGap=x@MIPGap,
		Presolve=x@Presolve
	)
	if (is.finite(x@TimeLimit))
		y=append(y, list(TimeLimit=x@TimeLimit))
	return(y)
}


