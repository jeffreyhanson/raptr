#' @include RcppExports.R raptr-internal.R generics.R misc.R PlanningUnitPoints.R DemandPoints.R AttributeSpace.R
NULL

#' AttributeSpaces: An S4 class to represent a collection of attribute spaces for different species.
#'
#' This class is used to store a collection of attribute spaces for different species.
#'
#' @slot spaces \code{list} of \code{AttributeSpace} objects for different species.
#' @slot name \code{character} name to identify the attribute space.
#' @seealso \code{\link{AttributeSpace-class}}.
#' @export
setClass("AttributeSpaces",
	representation(
		spaces='list',
		name='character'
	),
	validity=function(object) {
		# check that all elements in the list are AttributeSpace objects
		sapply(object@spaces, expect_is, class='AttributeSpace', info='argument to object@spaces must be a list of AttributeSpace objects')
	
		# expect name is not NA
		expect_false(is.na(object@name), info='argument to name must not be NA')
		expect_equal(length(object@name), 1, info='argument to name must have a single element')
		return(TRUE)
	}
)

#' Create new AttributeSpaces object
#'
#' This function creates a new \code{AttributeSpaces} object.
#'
#' @param spaces \code{list} of \code{AttributeSpace} objects for different species.
#' @param name \code{character} name to identify the attribute space.
#' @seealso \code{\link{AttributeSpace-class}}.
#' @export
#' @examples
#' space1 <- AttributeSpace(
#' PlanningUnitPoints(
#'		matrix(rnorm(100), ncol=2),
#'		seq_len(50)
#'	),
#'	DemandPoints(
#'		matrix(rnorm(100), ncol=2),
#'		runif(50)
#'	),
#'	species=1L
#' )
#'
#' space2 <- AttributeSpace(
#'	PlanningUnitPoints(
#'		matrix(rnorm(100), ncol=2),
#'		seq_len(50)
#'	),
#'	DemandPoints(
#'		matrix(rnorm(100), ncol=2),
#'		runif(50)
#'	),
#'	species=2L
#' )
#' spaces1 <- AttributeSpaces(list(space1, space2), 'test')
AttributeSpaces<-function(spaces, name) {
	as<-new("AttributeSpaces", spaces=spaces, name=as.character(name))
	validObject(as, test=FALSE)
	return(as)
}


#' @method print AttributeSpaces
#' @rdname print
#' @export
print.AttributeSpaces<-function(x, ..., header=TRUE) {
	if (header)
		cat("AttributeSpaces object.\n")
	cat("  Name:",x@name,"\n")
	cat("  Number of species:",length(x@spaces),"\n")
	sapply(x@spaces, print, header=FALSE)
	return(invisible())
}


