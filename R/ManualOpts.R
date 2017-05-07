 #' @include RcppExports.R raptr-internal.R misc.R
NULL

#' ManualOpts: An S4 class to represent parameters for manually specified solutions
#'
#' This class is used to store parameters.
#'
#' @slot NumberSolutions \code{integer} number of solutions.
#' @seealso \code{\link{ManualOpts}}.
#' @export
setClass("ManualOpts",
	representation(
		NumberSolutions="integer"
	),
	prototype=list(
		NumberSolutions=1L
	),
	contains='SolverOpts',
	validity=function(object) {
		# NumberSolutions
		if (!is.integer(object@NumberSolutions)) stop('argument to NumberSolutions is not integer')
		if (!is.finite(object@NumberSolutions)) stop('argument to NumberSolutions is NA or non-finite values')
		return(TRUE)
	}
)

#' Create ManualOpts object
#'
#' This function creates a new ManualOpts object.
#'
#' @param NumberSolutions \code{integer} number of solutions to generate. Defaults to 1L.
#' @return \code{\link{ManualOpts}} object
#' @seealso \code{\link{ManualOpts-class}}.
#' @export
#' @examples
#' # create ManualOpts object
#' ManualOpts(NumberSolutions=1L)
#' @export
ManualOpts <- function(NumberSolutions=1L) {
	mo<-new("ManualOpts", NumberSolutions=NumberSolutions)
	validObject(mo, test=FALSE)
	return(mo)
}

#' @method print ManualOpts
#' @rdname print
#' @export
print.ManualOpts <- function(x, ..., header=TRUE) {
	if (header) {
		cat("ManualOpts object.\n")
	} else {
		cat('  Method: manual\n')
	}
	cat('  NumberSolutions:',x@NumberSolutions,'\n')
}

#' @rdname show
#' @export
setMethod(
	'show',
	'ManualOpts',
	function(object)
		print.ManualOpts(object)
)

#' @rdname update
#' @method update ManualOpts
#' @export
update.ManualOpts <- function(object, NumberSolutions=NULL, ...) {
	if (!is.null(NumberSolutions))
		stop('Solver parameters cannot be updated for manually specified solutions.')
	return(object)
}
