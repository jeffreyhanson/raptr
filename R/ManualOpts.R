 #' @include RcppExports.R raspr-internal.R misc.R
NULL

#' ManualOpts: An S4 class to represent parameters for manually specified solutions
#'
#' This class is used to store parameters.
#'
#' @slot NumberSolutions \code{integer} number of solutions.
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
		if (!is.integer(object@NumberSolutions)) stop('argument to NumberSolutionsEPS is not integer')
		if (!is.finite(object@NumberSolutions)) stop('argument to NumberSolutions is NA or non-finite values')
		return(TRUE)
	}
)

#' Create ManualOpts object
#'
#' This function creates a new ManualOpts object.
#'
#' @param NumberSolutions \code{integer} number of solutions to generate. Defaults to 1L.
#' @return \code{ManualOpts} object
#' @seealso \code{\link{ManualOpts-class}}.
#' @export
#' @examples
#' # create ManualOpts object
#' ManualOpts(NumberSolutions=1L)
#' @export
ManualOpts<-function(NumberSolutions=1L) {
	mo<-new("ManualOpts", NumberSolutions=NumberSolutions)
	validObject(mo, test=FALSE)
	return(mo)
}

#' @method print ManualOpts
#' @rdname print
#' @export
print.ManualOpts=function(x, ..., header=TRUE) {
	if (header) {
		cat("ManualOpts object.\n")
	} else {
		cat('  Solver Method: solutions manually specified\n')
	}
	cat('  NumberSolutions:',x@NumberSolutions,'\n')
}

#' @describeIn show
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
update.ManualOpts<-function(object, NumberSolutions=NULL) {
	if (!is.null(NumberSolutions))
		stop('Solver parameters cannot be updated for manually specified solutions.')
	return(object)
}
