 #' @include RcppExports.R raptr-internal.R misc.R
NULL

#' GurobiOpts: An S4 class to represent Gurobi parameters
#'
#' This class is used to store Gurobi input parameters.
#'
#' @slot Threads \code{integer} number of cores to use for processing. Defaults to 1L.
#' @slot MIPGap \code{numeric} MIP gap specifying minimum solution quality. Defaults to 0.1.
#' @slot Method \code{integer} Algorithm to use for solving model. Defaults to 0L.
#' @slot Presolve \code{integer} code for level of computation in presolve. Defaults to 2.
#' @slot TimeLimit \code{integer} number of seconds to allow for solving. Defaults to NA_integer_, and so a time limit is not imposed.
#' @slot NumberSolutions \code{integer} number of solutions to generate. Defaults to 1L.
#' @slot MultipleSolutionsMethod \code{character} name of method to obtain multiple solutions \code{NumberSolutions} > 1. Available options are 'benders.cuts' and 'solution.pool'. Defaults to 'benders.cuts'. Note that the \code{rgurobi} package must be to use the 'solution.pool' method.
#' @seealso \code{\link{GurobiOpts}}.
#' @export
setClass("GurobiOpts",
	representation(
		Threads="integer",
		MIPGap="numeric",
		Method="integer",
		Presolve="integer",
		TimeLimit="integer",
		NumberSolutions="integer",
		MultipleSolutionsMethod='character'
	),
	prototype=list(
		Threads=1L,
		MIPGap=0.1,
		Method=0L,
		Presolve=2L,
		TimeLimit=NA_integer_,
		NumberSolutions=1L,
		MultipleSolutionsMethod='benders.cuts'
	),
	contains='SolverOpts',
	validity=function(object) {

		# NumberSolutions
		if (!is.integer(object@NumberSolutions)) stop('argument to NumberSolutionsEPS is not integer')
		if (!is.finite(object@NumberSolutions)) stop('argument to NumberSolutions is NA or non-finite values')

		# TimeLimit
		if (!is.integer(object@TimeLimit)) stop('argument to TimeLimit is not integer')

		# Threads
		if (!is.integer(object@Threads)) stop('argument to Threads is not numeric')
		if (!is.finite(object@Threads)) stop('argument to Threads is NA or non-finite values')
		if (object@Threads > detectCores(logical=TRUE)) {
			warning(paste0('argument Threads greater than number of threads on the system, changing Threads to ',object@Threads))
			object@Threads<-detectCores(logical=TRUE)
		}

		# MultipleSolutionsMethod
		match.arg(object@MultipleSolutionsMethod, c('benders.cuts', 'solution.pool'))
		
		# Presolve
		if (!is.integer(object@Presolve)) stop('argument to Presolve is not integer')
		if (!is.finite(object@Presolve)) stop('argument to Presolve is NA or non-finite values')
		if (object@Presolve < -1 || object@Presolve > 2) stop('argument to Presolve must be between -1 and 2')

		# Method
		if (!is.integer(object@Method)) stop('argument to Method is not integer')
		if (!is.finite(object@Method)) stop('argument to Method is NA or non-finite values')
		if (object@Method < -1 || object@Method > 4) stop('argument to Presolve must be between -1 and 4')

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
#' @param MIPGap \code{numeric} MIP gap specifying minimum solution quality. Defaults to 0.1.
#' @param Method \code{integer} Algorithm to use for solving model. Defaults to 0L.
#' @param Presolve \code{integer} code for level of computation in presolve (lp_solve parameter). Defaults to 2.
#' @param TimeLimit \code{integer} number of seconds to allow for solving. Defaults to NA_integer_, and so a time limit is not imposed.
#' @param NumberSolutions \code{integer} number of solutions to generate. Defaults to 1L.
#' @param MultipleSolutionsMethod \code{character} name of method to obtain multiple solutions \code{NumberSolutions} > 1. Available options are 'benders.cuts' and 'solution.pool'. Defaults to 'benders.cuts'. Note that the \code{rgurobi} package must be to use the 'solution.pool' method.
#' @return \code{GurobiOpts} object
#' @seealso \code{\link{GurobiOpts-class}}.
#' @export
#' @examples
#' # create GurobiOpts object using default parameters
#' GurobiOpts(Threads=1L, MIPGap=0.1, Method=0L, 
#' 	Presolve=2L, TimeLimit=NA_integer_, NumberSolutions=1L)
#' @export
GurobiOpts<-function(Threads=1L, MIPGap=0.1, Method=0L, Presolve=2L, TimeLimit=NA_integer_, NumberSolutions=1L, MultipleSolutionsMethod='benders.cuts') {
	go<-new("GurobiOpts", Threads=Threads, MIPGap=MIPGap, Method=0L, Presolve=Presolve, TimeLimit=TimeLimit, NumberSolutions=NumberSolutions, MultipleSolutionsMethod=MultipleSolutionsMethod)
	validObject(go, test=FALSE)
	return(go)
}

#' @method print GurobiOpts
#' @rdname print
#' @export
print.GurobiOpts <- function(x, ..., header=TRUE) {
	if (header) {
		cat("GurobiOpts object.\n")
	} else {
		cat('  Method: Gurobi\n')
	}
	cat('  Threads:',x@Threads,'\n')
	cat('  MIPGap:',x@MIPGap,'\n')
	cat('  Method:',x@Method,'\n')
	cat('  Presolve:',x@Presolve,'\n')
	cat('  TimeLimit:',x@TimeLimit,'\n')
	cat('  NumberSolutions:',x@NumberSolutions,'\n')
	cat('  MultipleSolutionsMethod:',x@MultipleSolutionsMethod,'\n')
}

#' @rdname show
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
	y <- list(
		Threads=x@Threads,
		MIPGap=x@MIPGap,
		Presolve=x@Presolve,
		Method=x@Method
	)
	if (is.finite(x@TimeLimit))
		y <- append(y, list(TimeLimit=x@TimeLimit))
	return(y)
}


#' @rdname update
#' @method update GurobiOpts
#' @export
update.GurobiOpts<-function(object, Threads=NULL, MIPGap=NULL, Method=NULL, Presolve=NULL, TimeLimit=NULL, NumberSolutions=NULL, MultipleSolutionsMethod=NULL, ...) {
	# update arguments
	if (!is.null(Threads))
		object@Threads<-Threads
	if (!is.null(MIPGap))
		object@MIPGap<-MIPGap
	if (!is.null(Method))
		object@Method<-Method
	if (!is.null(Presolve))
		object@Presolve<-Presolve
	if (!is.null(TimeLimit))
		object@TimeLimit<-TimeLimit
	if (!is.null(NumberSolutions))
		object@NumberSolutions<-NumberSolutions
	if (!is.null(MultipleSolutionsMethod))
		object@MultipleSolutionsMethod<-MultipleSolutionsMethod
	# check object for validity
	validObject(object, test=FALSE)
	# return object
	return(object)
}
